options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(openxlsx)


# Facts ########################################################################
from <- floor_date(today()-years(2), "year")
to <-  today()

url_head <- str_glue(
    "https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=
    SELECT 
        fecha_corte, 
        tipo_entidad, 
        codigo_entidad, 
        subtipo_negocio, 
        codigo_negocio, 
        tipo_participacion, 
        rendimientos_abonados, 
        precierre_fondo_dia_t, 
        numero_unidades_fondo_cierre, 
        valor_unidad_operaciones,
        aportes_recibidos, 
        retiros_redenciones, 
        anulaciones, 
        valor_fondo_cierre_dia_t, 
        numero_inversionistas 
    where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' 
        and subtipo_negocio not in('7')  
    LIMIT 100000000") %>% 
    URLencode()

facts <- fromJSON(url_head) %>% 
    mutate(across(rendimientos_abonados:numero_inversionistas, as.numeric),
           fecha_corte = ymd_hms(fecha_corte) %>% 
               ymd()) %>%  
    arrange(fecha_corte,
            codigo_entidad,
            subtipo_negocio,
            codigo_negocio,
            tipo_participacion) %>% 
    unite("cod", tipo_entidad, codigo_entidad, subtipo_negocio, codigo_negocio) %>% 
    mutate(transaccciones_netas = aportes_recibidos-retiros_redenciones+anulaciones) %>% 
    select(-aportes_recibidos, -retiros_redenciones, -anulaciones)

datos_xlsx <- read.xlsx("auxiliares/reporteRentabilidades.xlsx", detectDates = TRUE) %>% 
    as_tibble() %>% 
    select(Fecha.corte, 
           Tipo.Entidad,
           Cód..Entidad, 
           Cód..Negocio, 
           Subtipo.Negocio,
           Cons..id.Part.,
           Valor.fondo.al.cierre.del.día.t, 
           Núm..Invers., 
           Rentab..dia,
           Núm..unidades,
           Valor.unidad.para.las.operaciones.del.día.t,
           `Tipo.Part..<sup>1<sup/>`) %>% 
    filter(Subtipo.Negocio != "FONDOS DE CAPITAL PRIVADO") %>% 
    mutate(Fecha.corte = dmy(Fecha.corte),
           Núm..Invers. = as.numeric(Núm..Invers.),
           Rentab..dia = ((1+(as.numeric(Rentab..dia)/100))^(1/365)),
           across(c( Valor.fondo.al.cierre.del.día.t,
                     Núm..unidades,
                     Valor.unidad.para.las.operaciones.del.día.t),
                  ~ as.numeric(str_replace_all(., "[^0-9\\.]", ""))), 
           Subtipo.Negocio = case_when(
               Subtipo.Negocio == "FIC BURSATILES" ~ 1,
               Subtipo.Negocio == "FIC DE MERCADO MONETARIO" ~ 2,
               Subtipo.Negocio == "FIC INMOBILIARIAS" ~ 3,
               Subtipo.Negocio == "FIC DE TIPO GENERAL" ~ 1
           ),
           cod = str_c(Tipo.Entidad,Cód..Entidad,Subtipo.Negocio,Cód..Negocio, sep = "_"),
           tipo_participacion = str_c(`Tipo.Part..<sup>1<sup/>`, Cons..id.Part.)) %>% 
    select(Fecha.corte,Valor.fondo.al.cierre.del.día.t,Núm..Invers.,Rentab..dia,cod,tipo_participacion,Núm..unidades,Valor.unidad.para.las.operaciones.del.día.t) %>% 
    arrange(Fecha.corte) %>% 
    group_by(cod, tipo_participacion) %>% 
    mutate(precierre_fondo_dia_t = lag(Valor.fondo.al.cierre.del.día.t)* Rentab..dia,
           rendimientos_abonados = precierre_fondo_dia_t-lag(Valor.fondo.al.cierre.del.día.t)) %>% 
    slice(-1) %>% 
    select(Fecha.corte, cod, tipo_participacion, rendimientos_abonados, precierre_fondo_dia_t, Valor.fondo.al.cierre.del.día.t, Núm..Invers., Núm..unidades,
           Valor.unidad.para.las.operaciones.del.día.t) %>% 
    ungroup() %>% 
    rename(fecha_corte = Fecha.corte, valor_fondo_cierre_dia_t = Valor.fondo.al.cierre.del.día.t, numero_inversionistas = Núm..Invers., 
           numero_unidades_fondo_cierre = Núm..unidades, valor_unidad_operaciones = Valor.unidad.para.las.operaciones.del.día.t) %>% 
    semi_join(facts %>% 
                  filter(fecha_corte >= floor_date(today()%m-%months(1), "month")), 
              by =join_by(cod, tipo_participacion)) %>% 
    mutate(transaccciones_netas = valor_fondo_cierre_dia_t - precierre_fondo_dia_t) %>% 
    anti_join(facts, by =join_by(fecha_corte, cod, tipo_participacion))

facts_completos <- bind_rows(facts, datos_xlsx)

facts_ext_fecha <- facts_completos %>% 
    group_by(cod,
             tipo_participacion) %>% 
    pad_by_time(fecha_corte,
                "day") %>% 
    fill(valor_fondo_cierre_dia_t) %>%
    fill(numero_unidades_fondo_cierre) %>% 
    fill(valor_unidad_operaciones) %>% 
    fill(numero_inversionistas) %>% 
    mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
           transaccciones_netas = replace_na(transaccciones_netas,0),
           precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
                                           valor_fondo_cierre_dia_t,
                                           precierre_fondo_dia_t)) %>%
    arrange(cod,fecha_corte) %>% 
    mutate(rendimientos_abonados = round(rendimientos_abonados, 1),
           precierre_fondo_dia_t = round(precierre_fondo_dia_t, 1),
           crecimiento_dia = (precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados)-1),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia),
                                     0,
                                     crecimiento_dia)) %>% 
    ungroup()

# Filtros ######################################################################

## Activo (Con movimiento a max fecha) #########################################

facts_activo <- facts_ext_fecha %>% 
    group_by(cod, tipo_participacion) %>%
    mutate(max_fecha = max(fecha_corte)) %>% 
    ungroup() %>% 
    filter(max_fecha == max(fecha_corte)) %>% 
    select(-max_fecha)


## Largo (Días min con registros) ##############################################

facts_dias <- facts_activo %>% 
    group_by(cod, tipo_participacion) %>%
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n > 365) %>% 
    select(-n)

## Base (Participación con mayor número de inversionistas) #####################

fondos_base <- facts_dias %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    group_by(cod) %>% 
    filter(numero_inversionistas == max(numero_inversionistas)) %>% 
    filter(numero_inversionistas >0) %>% 
    filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
    ungroup() %>% 
    select(cod, tipo_participacion) %>% 
    distinct(cod, .keep_all = TRUE)

facts_base <- facts_dias %>% 
    semi_join(fondos_base, by = c("cod", "tipo_participacion"))





