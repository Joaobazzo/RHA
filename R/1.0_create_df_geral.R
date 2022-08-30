
# create munis_df


library(magrittr)
library(data.table)
library(geobr)
library(sidrar)
#library(mapview)
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

#munis_df <- munis_df[code_intermediate %in% unique(my_rgint$RG_Cod),]
munis_df[,code_intermediate := as.character(code_intermediate)]

my_rgint_dt <- my_rgint_dt[
  ,.SD
  ,.SDcols = c(codigo_municipio_completo
               ,nome_municipio
               ,uf
               ,nome_uf)
]

## 2.1) RGINT & city -----
munis_df1 <- data.table::merge.data.table(
  , x = munis_df
  , y = my_rgint_dt
  , by.y = "regiao_geografica_intermediaria"
  , by.x = "code_intermediate"
  , all.x = TRUE
  , all.y = FALSE)
munis_df1[1]
munis_df1 <- munis_df1[
  ,.SD
  ,.SDcols = c("codigo_municipio_completo"
               ,"nome_municipio"
               ,"code_state"
               ,"abbrev_state"
               ,"name_state"
               ,"name_region"
               ,"regiao_geografica_imediata"
               ,"nome_regiao_geografica_imediata"
               ,"code_intermediate"
               ,"name_intermediate"
               ,"mun_sudene"
               ,"mun_pisf"
               ,"mun_bsf"
               ,"mun_bpar"
               ,"regiao_total"
               ,"nordeste")
]

data.table::setnames(x = munis_df1
                     ,old = "codigo_municipio_completo"
                     ,new = "code_muni")
data.table::setnames(x = munis_df1
                     ,old = "nome_municipio"
                     ,new = "name_muni")
data.table::setnames(x = munis_df1
                     ,old = "regiao_geografica_imediata"
                     ,new = "code_rgi")
data.table::setnames(x = munis_df1
                     ,old = "nome_regiao_geografica_imediata"
                     ,new = "name_rgi")
data.table::setnames(x = munis_df1
                     ,old = "nordeste"
                     ,new = "semi_arido")
munis_df1[1]
df_geral[1]

readr::write_rds(munis_df1,"data/df_geral_muni.rds",compress="gz")
