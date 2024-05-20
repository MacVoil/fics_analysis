options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(openxlsx)
library(arrow)
library(umap)
library(plotly)
library(diceR)
library(mclust)
library(dbscan)
library(furrr)
library(tidymodels)
library(modeltime)

source("scripts/facts_dims_filters.R")

fics_dims <- get_fics_dims()

fics <- get_fics_facts(
    floor_date(today()-years(5), "year")
) 

fics_base_disponibles <-  fics %>% 
    fics_activos() %>% 
    fics_dias() %>% 
    fics_base()

fics_abiertos_clust <- fics_base_disponibles %>% 
    filter(fecha_corte >= floor_date(today(), "years")-years(1)) %>% 
    group_by(cod) %>%
    mutate(n = n()) %>% 
    mutate(any_mov = transaccciones_netas != 0,
           mov_per = sum(any_mov)/n) %>% 
    filter(mov_per >= 0.5) %>% 
    ungroup() %>% 
    filter(n == max(n)) %>% 
    select(fecha_corte, 
           cod,
           crecimiento_dia) %>%
    drop_na()

datos_clust <- fics_abiertos_clust %>% 
    mutate(across(crecimiento_dia, ~ standardize_vec(., silent = TRUE))) %>%
    pivot_wider(names_from = fecha_corte, values_from = crecimiento_dia)

umap_data <- umap(datos_clust[,-1], n_components = 2, random_state = 4981)
umap_data$layout %>% head()

umap_data_tibble <- as_tibble(umap_data$layout) %>% 
    bind_cols(datos_clust %>% select(cod))

plot_ly(umap_data_tibble, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        size = 1) 

elementos <- c("hc", "pam", "gmm", "diana", "km", "ap", "som", "cmeans")

todas_combinaciones <- unlist(lapply(1:length(elementos), function(x) {
    combn(elementos, x, simplify = FALSE)
}), recursive = FALSE)

comb_2 <- expand.grid(1:255, c(5,7,9))
matrix_umap <- umap_data$layout
rownames(matrix_umap) <-  datos_clust$cod

get_sil <- function(i){
    comb <- comb_2[i,1]
    k <- comb_2[i,2]
    
    tibble(n=i) %>% bind_cols(silhouette = dice(matrix_umap,
                                                nk = k,
                                                algorithms = todas_combinaciones[[comb]],
                                                hc.method = "ward.D2",
                                                trim = T,
                                                reweigh = T,
                                                n = 11,
                                                cons.funs = "CSPA",
                                                nmf.method = "lee",
                                                prep.data = "none",
                                                reps = 100,
                                                seed = 4981,
                                                seed.data = 4981)$indices$ii[[1]]["CSPA",-1]$silhouette)
    
}

get_sil(dim(comb_2)[1])


tictoc::tic()
plan(multisession, workers = availableCores())

alg_table <- future_map_dfr(1:dim(comb_2)[1], ~  get_sil(.x))

plan(sequential)
tictoc::toc()

silhouette <- alg_table %>% 
    filter(silhouette == max(silhouette)) %>% 
    arrange(desc(n)) %>% 
    distinct(silhouette, .keep_all = TRUE) %>% 
    pull(n)

datos_dice <- dice(matrix_umap,
                   nk = comb_2[silhouette,]$Var2,
                   algorithms = todas_combinaciones[[n_components = comb_2[silhouette,]$Var1]],
                   hc.method = "ward.D2",
                   trim = T,
                   reweigh = T,
                   n = 11,
                   cons.funs = "CSPA",
                   nmf.method = "lee",
                   prep.data = "none",
                   reps = 100,
                   seed = 4981,
                   seed.data = 4981)

datos_dice$clusters %>% table()

table_datos_dice <- tibble(cod = rownames(datos_dice$clusters))  %>% bind_cols(as_tibble(datos_dice$clusters))

umap_data_tibble_dice <- umap_data_tibble %>% 
    left_join(table_datos_dice) %>% 
    mutate(cluster = as_factor(CSPA))

plot_ly(umap_data_tibble_dice, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(12, "Set3"),
        marker = list(size = 15),
        opacity = 0.75) 

datos_cluster_dice <- fics_base_disponibles %>% 
    filter(fecha_corte >= floor_date(today(), "years")-years(1)) %>% 
    filter(cod %in% fics_abiertos_clust$cod) %>% 
    rent_brut() %>% 
    left_join(table_datos_dice) %>% 
    left_join(fics_dims) %>% 
    drop_na() 

datos_cluster_dice %>% 
    group_by(CSPA) %>% 
    plot_time_series(fecha_corte, rent_brut_30, .color_var = nombre_patrimonio, .smooth = FALSE, .facet_ncol = 1, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000)) 

representacion_clustes <- datos_cluster_dice %>% 
    group_by(CSPA, fecha_corte) %>% 
    summarise(q1 = quantile(rent_brut_30, 0.25),
              q2 = quantile(rent_brut_30, 0.5),
              q3 = quantile(rent_brut_30, 0.75),
    ) %>% 
    ungroup()

representacion_clustes %>% 
    plot_time_series(fecha_corte, q2, .color_var = CSPA, .smooth = FALSE) 

p <- representacion_clustes %>% 
    ggplot(aes(x =fecha_corte, y = q2, col = factor(CSPA), group =  factor(CSPA), fill= factor(CSPA))) +
    geom_line() +
    geom_ribbon(aes(ymin=q1, ymax=q3), alpha = 0.5)

ggplotly(p, dynamicTicks = TRUE)

clust_select <- table_datos_dice %>% 
    filter(cod %in% c("5_16_1_10936")) %>% 
    pull(CSPA)

clust_selected <- table_datos_dice %>% 
    filter(CSPA %in% c(clust_select)) %>% 
    filter(!(cod %in% c("5_3_1_105788", "5_3_1_105787", 
                      "5_3_1_105784", "85_28_1_58921"))) %>% 
    pull(cod)

fics_dims %>% filter(cod %in% clust_selected) %>% arrange(nombre_entidad)

fics_forecast_mean <- fics_base_disponibles %>% 
    filter(cod %in% clust_selected) %>% 
    rent_vol() %>% 
    filter(fecha_corte >= floor_date(today(), "years")-years(1)) %>% 
    left_join(fics_dims) %>%
    group_by(cod) %>% 
    mutate(mediana = median(rent_30)) %>% 
    ungroup() %>% 
    group_by(nombre_entidad, cod) %>% 
    summarise(mediana = min(mediana)) %>% 
    ungroup() %>% 
    group_by(nombre_entidad) %>% 
    mutate(mediana_emp =  median(mediana),
           cercania = abs(mediana-mediana_emp)) %>% 
    filter(cercania == min(cercania)) %>% 
    filter(mediana == max(mediana)) %>% 
    pull(cod)

fics_forecast_base <- fics_base_disponibles %>% 
    filter(cod %in% fics_forecast_mean) %>% 
    rent_brut()


fics_forecast_base %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000), .smooth = FALSE,
                     .facet_ncol = 3, .facet_nrow = 3)

datos_forecast_limpieza_base_activos_major_n_inv_mean <- fics_forecast_base %>% 
    group_by(fecha_corte) %>% 
    summarise(mean_creci = quantile(crecimiento_dia, 0.5)
    ) %>% 
    ungroup()

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_time_series(fecha_corte, mean_creci, .smooth = FALSE)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    #filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = c("observed", "season", "trend", "remainder"), .interactive = F)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    drop_na() %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = mean_creci, .lags = 2:365)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "wday.lbl", .geom = "violin")

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "month.lbl", .geom = "violin" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "year", .geom = "violin" )

trm <- read.xlsx("auxiliares/trm.xlsx", detectDates = TRUE) %>% 
    select(fecha, trm)

ipc <- read.xlsx("Auxiliares/ipc.xlsx", detectDates = FALSE) %>% 
    select(fecha, ipc) %>% 
    mutate(fecha = ymd(fecha, truncated = TRUE)) %>% 
    pad_by_time(fecha,
                "day", .fill_na_direction = "down", .end_date = ceiling_date(max(.$fecha), unit = "month")) 

creci_trend <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    tk_stl_diagnostics(.date_var = fecha_corte, .value = mean_creci) %>% 
    select(fecha_corte, trend) %>% 
    rename(creci_trend = trend) %>% 
    left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
    mutate(across(c(creci_trend, trm, ipc), standardize_vec)) 

creci_trend %>% 
    pivot_longer(-fecha_corte) %>% 
    plot_time_series(.date_var = fecha_corte, .value = value, .color_var = name,.smooth = FALSE)

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
    left_join(ipc, by = c("fecha_corte" = "fecha")) 

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    drop_na() %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = mean_creci, .ccf_vars = c(ipc, trm), .show_ccf_vars_only = TRUE,  .facet_ncol = 1)

festivos <- read.xlsx("Auxiliares/festivos.xlsx", detectDates = TRUE) 

datos_completos <- fics_forecast_base %>%
    rename(creci_dia = crecimiento_dia) %>%
    group_by(cod) %>%
    future_frame(fecha_corte, .length_out = 28, .bind_data = TRUE) %>%
    ungroup() %>%
    left_join(trm, by = c("fecha_corte" = "fecha")) %>%
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
    left_join(festivos, by = c("fecha_corte" = "fecha")) %>% 
    replace_na(list(festivo = FALSE)) %>% 
    mutate(dia_semana = wday(fecha_corte, week_start = 1),
           fin_de_semama = dia_semana %in% 6:7,
           dia_no_habil = as.numeric(festivo|fin_de_semama)) %>% 
    select(-c(festivo:fin_de_semama)) %>% 
    mutate(across(trm:ipc, standardize_vec)) %>% 
    select(cod, fecha_corte, creci_dia, trm, ipc, dia_no_habil)

datos_forecast <- datos_completos %>% 
    filter(is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_modelar <- datos_completos %>% 
    filter(!is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_splits <- time_series_split(datos_modelar, assess = 28, cumulative = TRUE)

datos_splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha_corte, creci_dia)

datos_train <- training(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_test <- testing(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

time_series_cv(datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
                   filter(fecha_corte >= today() - years(2)),
               date_var    = fecha_corte,
               assess      = "28 days",
               skip        = "28 days",
               cumulative = TRUE,
               slice_limit = 13) %>%
    plot_time_series_cv_plan(fecha_corte, mean_creci, .interactive = FALSE, .facet_ncol = 3, .facet_dir = "v")

## ir a python ---

library(reticulate)
use_condaenv(condaenv = "C:\\Users\\user\\miniconda3\\envs\\ag\\python.exe")


## regresando de python ---

datos_pred <- py$predictions_full %>% 
    rename(cod = item_id, fecha_corte = timestamp, creci_dia = mean) %>% 
    mutate(fecha_corte = ymd(as.Date(fecha_corte))) %>% 
    select(cod, fecha_corte, creci_dia, `0.25`, `0.75`)

datos_plot <- datos_modelar %>% 
    select(-trm, -ipc, dia_no_habil) %>% 
    bind_rows(datos_pred)

datos_modeltime <- datos_plot %>% 
    mutate(.model_id = if_else(is.na(`0.25`), NA, 1) ,
           .model_desc = if_else(is.na(`0.25`), "ACTUAL", "AutoGluon_TS_KC"),
           .key = if_else(is.na(`0.25`), "actual", "prediction")) %>% 
    rename(.index = fecha_corte, .value = creci_dia, .conf_lo = `0.25`, .conf_hi = `0.75`)

datos_modeltime %>% 
    group_by(cod) %>% 
    filter(.index >= today()-months(6)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

plot_pyoject <- datos_modeltime %>% 
    group_by(cod) %>% 
    mutate(across(.conf_lo:.conf_hi, ~coalesce(.,.value)),
           across(c(.value,.conf_lo:.conf_hi), ~ slidify_vec(
               .x      = .,
               .period = 30,
               .f      = ~ (prod(.+1)^(365/30))-1,
               .align  = "rigth")))

plot_pyoject %>% 
    group_by(cod) %>% 
    filter(.index >= today()-months(12)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))
