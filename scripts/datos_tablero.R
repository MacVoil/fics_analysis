options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(openxlsx)
library(arrow)

source("scripts/facts_dims_filters.R")

fics_disponibles <- get_fics_facts(
    floor_date(today()-years(1), "year")
    ) %>% 
    fics_activos() %>% 
    fics_dias()

fics_dims <- get_fics_dims() %>% 
    filter(cod %in% fics_disponibles$cod)

write_parquet(fics_disponibles, "datos_tablero/fics_disponibles.parquet")
write_parquet(fics_dims, "datos_tablero/fics_dims.parquet")
