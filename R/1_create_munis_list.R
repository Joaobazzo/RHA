
# create munis_df


library(magrittr)
library(data.table)
library(geobr)
library(mapview)
library(readr)
rm(list=ls())
gc(reset=T)

# 1) read_data ------
all_rgint <- geobr::read_intermediate_region(simplified = T)
all_munis <- geobr::read_municipality(simplified = T)

my_rgint <- data.table::fread("data-raw/Saneamento/55_RGINTs_Projeto.txt")

my_rgint_dt <- openxlsx::read.xlsx("data-raw/ICE/Listagem_Municipios_Recortes_atualizada.xlsx")
data.table::setDT(my_rgint_dt)
names(my_rgint_dt) <- janitor::make_clean_names(names(my_rgint_dt))

# 2) process -----

munis_df <- all_rgint
data.table::setDT(munis_df)

munis_df <- munis_df[code_intermediate %in% unique(my_rgint$RG_Cod),]
munis_df[,code_intermediate := as.character(code_intermediate)]

## 2.1) RGINT & city -----
munis_df1 <- data.table::merge.data.table(
  , x = munis_df
  , y = my_rgint_dt[,.SD,.SDcols = c("codigo_municipio_completo",
                                     "regiao_geografica_intermediaria")]
  , by.y = "regiao_geografica_intermediaria"
  , by.x = "code_intermediate"
  , all.x = TRUE
  , all.y = FALSE)

data.table::setnames(x = munis_df1
                     ,old = "codigo_municipio_completo"
                     ,new = "code_muni")
munis_df1[1]

## 2.2) City -----

munis_df2 <- all_munis[all_munis$code_muni %in% unique(munis_df1$code_muni),]

data.table::setDT(munis_df2)

uniqueN(munis_df1$code_state)



## 2.3) Pop 2010 -----

# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
info2020 <- sidrar::info_sidra(x = 202)
### download CENSUS
popcenso <- lapply(c(1991, 2000, 2010),function(i){
  message(i)
  pop202 <- sidrar::get_sidra(x = 202
                              , variable = 93
                              , period = as.character(i)
                              , geo = "City"
                              , classific = c("c1"))
  
  # fix names
  data.table::setDT(pop202)
  names(pop202) <- janitor::make_clean_names(names(pop202))
  
  tmp_pop <- data.table::copy(pop202)[municipio_codigo %in% unique(munis_df1$code_muni),]
  return(tmp_pop)
}) %>% data.table::rbindlist()

## 2.4) Pop projection---------

sidrar::info_sidra(x = 6579)
pop_6579 <- sidrar::get_sidra(x = 6579
                              , variable = 9324
                              , period = as.character(c(2020))
                              , geo = "City")

# fix names
data.table::setDT(pop_6579)
names(pop_6579) <- janitor::make_clean_names(names(pop_6579))

pop2020 <- data.table::copy(pop_6579)[municipio_codigo %in% unique(munis_df1$code_muni),]

# Merge -----

munis_list <- list("intermediate_region" = munis_df1,
                   "municipality" = munis_df2,
                   "pop_censo" = popcenso,
                   "pop_proj" = pop2020)

readr::write_rds(munis_list,"data/munis_list.rds",compress="gz")
