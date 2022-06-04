
# load packages -----


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

munis_df <- munis_df[code_intermediate %in% unique(my_rgint$RG_Cod),]
munis_df[,code_intermediate := as.character(code_intermediate)]


my_rgint_dt[mun_bsf > 0 ,bacia := "BSF"]
my_rgint_dt[mun_pisf > 0,bacia := "PISF"]
my_rgint_dt[mun_bpar > 0,bacia := "BPAR"]
my_rgint_dt[mun_bpar > 0 & mun_pisf > 0,bacia := "BPAR & PISF"]
my_rgint_dt[mun_bpar > 0 &  mun_bsf > 0,bacia := "BPAR & BSF"]
my_rgint_dt[mun_bpar > 0 & mun_pisf > 0,bacia := "BSF & PISF"]
my_rgint_dt[mun_bpar > 0 & mun_bsf > 0 & mun_pisf > 0,bacia := "BSF & PISF & BPAR"]
table(my_rgint_dt$bacia)

## 2.1) RGINT & city -----
munis_df1 <- data.table::merge.data.table(
  , x = munis_df
  , y = my_rgint_dt[,.SD,.SDcols = c("codigo_municipio_completo",
                                     "regiao_geografica_intermediaria"
                                     ,"bacia"
                                     ,"regiao_geografica_imediata"
                                     ,"nome_regiao_geografica_imediata")]
  , by.y = "regiao_geografica_intermediaria"
  , by.x = "code_intermediate"
  , all.x = TRUE
  , all.y = FALSE)
# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
info2020 <- sidrar::info_sidra(x = 6784)

PibList <- readr::read_rds("data/pib_total.rds")

PibList1 <- data.table::copy(PibList)
#  merge with other infos
PibList1[munis_df1
            , on = c("municipio_codigo" = "codigo_municipio_completo")
            , ":="(
              code_intermediate = i.code_intermediate
              ,name_intermediate = i.name_intermediate
              ,code_imediate = i.regiao_geografica_imediata
              ,name_imediate = i.nome_regiao_geografica_imediata
              ,bacia = i.bacia
            )]

# save-----
readr::write_rds(x = PibList1
                 ,file = "data/pib_total_prep.rds"
                 ,compress = "gz")
