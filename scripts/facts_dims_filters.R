
# Facts ########################################################################

get_fics_facts <- function(
        from = floor_date(today()-years(2), "year"),
        to =  today()    
    ){
    
    from <- ymd(from)
    to <- ymd(to)
    
    facts <- str_glue(
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
        URLencode() %>% 
        fromJSON() %>% 
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
        select(Fecha.corte,Valor.fondo.al.cierre.del.día.t,Núm..Invers.,
               Rentab..dia,cod,tipo_participacion,Núm..unidades,Valor.unidad.para.las.operaciones.del.día.t) %>% 
        arrange(Fecha.corte) %>% 
        group_by(cod, tipo_participacion) %>% 
        mutate(precierre_fondo_dia_t = lag(Valor.fondo.al.cierre.del.día.t)* Rentab..dia,
               rendimientos_abonados = precierre_fondo_dia_t-lag(Valor.fondo.al.cierre.del.día.t)) %>% 
        slice(-1) %>% 
        select(Fecha.corte, cod, tipo_participacion, rendimientos_abonados, 
               precierre_fondo_dia_t, Valor.fondo.al.cierre.del.día.t, Núm..Invers., Núm..unidades,
               Valor.unidad.para.las.operaciones.del.día.t) %>% 
        ungroup() %>% 
        rename(fecha_corte = Fecha.corte, 
               valor_fondo_cierre_dia_t = Valor.fondo.al.cierre.del.día.t, 
               numero_inversionistas = Núm..Invers., 
               numero_unidades_fondo_cierre = Núm..unidades, 
               valor_unidad_operaciones = Valor.unidad.para.las.operaciones.del.día.t) %>% 
        semi_join(facts %>% 
                      filter(fecha_corte >= floor_date(today()%m-%months(1), "month")), 
                  by =join_by(cod, tipo_participacion)) %>% 
        mutate(transaccciones_netas = valor_fondo_cierre_dia_t - precierre_fondo_dia_t) %>% 
        anti_join(facts, by =join_by(fecha_corte, cod, tipo_participacion))
    
    bind_rows(facts, datos_xlsx) %>%  
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
}


# Filtros ######################################################################

## Activo (Con movimiento a max fecha) #########################################

fics_activos <- function(datos){
    datos %>% 
        group_by(cod, tipo_participacion) %>%
        mutate(max_fecha = max(fecha_corte)) %>% 
        ungroup() %>% 
        filter(max_fecha == max(fecha_corte)) %>% 
        select(-max_fecha)
}

## Largo (Días min con registros) ##############################################

fics_dias <- function(datos, dias = 365){
    datos %>% 
        group_by(cod, tipo_participacion) %>%
        mutate(n = n()) %>% 
        ungroup() %>% 
        filter(n > 365) %>% 
        select(-n)
}


## Base (Participación con mayor número de inversionistas) #####################

fics_base <- function(datos){
    
    fondos_base <- datos %>% 
        filter(fecha_corte == max(fecha_corte)) %>% 
        group_by(cod) %>% 
        filter(numero_inversionistas == max(numero_inversionistas)) %>% 
        filter(numero_inversionistas >0) %>% 
        filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
        ungroup() %>% 
        select(cod, tipo_participacion) %>% 
        distinct(cod, .keep_all = TRUE)
    
    datos %>% 
        semi_join(fondos_base, by = c("cod", "tipo_participacion"))
}


# Dims #########################################################################

get_fics_dims <- function(from = floor_date(today()-years(2), "year"),
                          to =  today()    
){
    
    from <- ymd(from)
    to <- ymd(to)
    
    str_glue(
        "https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=
    SELECT distinct 
        tipo_entidad, 
        nombre_tipo_entidad,
        codigo_entidad, 
        nombre_entidad,
        subtipo_negocio, 
        codigo_negocio,
        nombre_patrimonio
    where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' 
        and subtipo_negocio not in('7') 
    LIMIT 100000000") %>% 
        URLencode() %>% 
        fromJSON() %>%  
        unite("cod", tipo_entidad, codigo_entidad, subtipo_negocio, codigo_negocio) %>% 
        mutate(
            nombre_tipo_entidad = case_when(
                str_starts(nombre_tipo_entidad, 
                           "SOCIEDADES ADMINISTRADORAS DE INVERSI") ~ "ADMINISTRADORAS",
                str_detect(nombre_tipo_entidad, "COMISIONISTAS")  ~ "COMISIONISTAS",
                str_detect(nombre_tipo_entidad, "FIDUCIARIA")  ~ "FIDUCIARIAS",
                TRUE ~ nombre_tipo_entidad,),
            cod_empresa = str_extract(cod, "\\d*_\\d*"),
            nombre_entidad = case_when(
                cod_empresa == "5_12" ~ "PREVISORA FIDUCUARIA",
                cod_empresa == "5_16" ~ "ALIANZA FIDUCIARIA",
                cod_empresa == "5_18" ~ "POPULAR FIDUCIARIA",
                cod_empresa == "5_20" ~ "CORFICOLOMBIANA FIDUCIARIA",
                cod_empresa == "5_21" ~ "OCCIDENTE FIDUCIARIA",
                cod_empresa == "5_22" ~ "BOGOTA FIDUCIARIA",
                cod_empresa == "5_23" ~ "ITAU FIDUCIARIA",
                cod_empresa == "5_25" ~ "SCOTIABANK COLPATRIA FIDUCIARIA",
                cod_empresa == "5_3" ~ "BBVA FIDUCIARIA",
                cod_empresa == "5_31" ~ "BANCOLOMBIA FIDUCIARIA",
                cod_empresa == "5_33" ~ "ACCION FIDUCIARIA",
                cod_empresa == "5_34" ~ "GNB SUDAMERIS FIDUCIARIA",
                cod_empresa == "5_38" ~ "FIDUCENTRAL FIDUCIARIA",
                cod_empresa == "5_39" ~ "FIDUAGRARIA FIDUCIARIA",
                cod_empresa == "5_40" ~ "FIDUCOLDEX FIDUCIARIA",
                cod_empresa == "5_42" ~ "DAVIVIENDA FIDUCIARIA",
                cod_empresa == "5_58" ~ "SURA FIDUCIARIA",
                cod_empresa == "5_59" ~ "CREDICORP FIDUCIARIA",
                cod_empresa == "5_6" ~ "COLMENA FIDUCIARIA",
                cod_empresa == "5_62" ~ "COOMEVA FIDUCIARIA",
                cod_empresa == "5_63" ~ "RENTA4G FIDUCIARIA",
                cod_empresa == "5_7" ~ "SKANDIA FIDUCIARIA",
                cod_empresa == "85_14" ~ "BTG PACTUAL COMISIONISTA",
                cod_empresa == "85_22" ~ "BANCOLOMBIA COMISIONISTA",
                cod_empresa == "85_26" ~ "DAVIVIENDA COMISIONISTA",
                cod_empresa == "85_27" ~ "BBVA COMISIONISTA",
                cod_empresa == "85_28" ~ "CREDICORP COMISIONISTA",
                cod_empresa == "85_3" ~ "ACCIONE Y VALORES COMISIONISTA",
                cod_empresa == "85_68" ~ "GLOBAL SECURITIES COMISIONISTA",
                cod_empresa == "85_73" ~ "CITIVALORES COMISIONISTA",
                cod_empresa == "85_88" ~ "ITAU COMISIONISTA",
                cod_empresa == "85_9" ~ "PROGRESION COMISIONISTA",
                cod_empresa == "85_91" ~ "GNB SUDAMERIS COMISIONISTA",
                cod_empresa == "85_97" ~ "LARRAIN VIAL COMISIONISTA",
                cod_empresa == "87_11" ~ "PROGRESION ADMINISTRADORA",
                TRUE ~ nombre_entidad),
            nombre_patrimonio = str_to_upper(nombre_patrimonio) %>% 
                str_replace_all(
                    "CARTERA COLECTIVA|CARTERA CON COMPARTIMENTOS|F.I.C.|CON PARTICIPACIONES DIFERENCIALES|FIC|FONDO DE INVERSI.*?N COLECTIVA|FONDO",
                    "") %>% 
                str_trim()
        ) %>% 
        select(-cod_empresa)
    
}

# Funciones extra ##############################################################

##  Rentabilidad Volatilidad ###################################################

rent_vol <- function(data, 
                     periodo = 30,
                     participacion = tipo_participacion,
                     fecha = fecha_corte){
    
    participacion <- enquo(participacion)
    fecha <- enquo(fecha)
    
    existing_cols <- names(data)
    
    data %>% 
        group_by(cod, !!participacion) %>% 
        arrange(cod, !!participacion, !!fecha) %>% 
        mutate(
            rent = slidify_vec(
                .x      = crecimiento_dia,
                .period = periodo,
                .f      = ~ (prod(.+1)^(365/periodo))-1,
                .align  = "rigth"),
            vol = slidify_vec(
                .x      = crecimiento_dia,
                .period = periodo,
                .f      = ~sd(.)*sqrt(365),
                .align  = "rigth")
        ) %>%
        ungroup() %>% 
        rename_with(
            .cols = setdiff(names(.), existing_cols),
            .fn   = ~ paste0(.x, "_", periodo)
        ) 
}
