
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
info2020 <- sidrar::info_sidra(x = 1419)

VecLoop <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25
             , 26, 27, 28, 29, 31, 32, 33, 35, 41, 42
             , 43, 50, 51, 52, 53)

i = VecLoop[1]
popList <- lapply(VecLoop,function(i){
  message(sprintf("State %s",i))
  pop202 <- sidrar::get_sidra(x = 1378
                              , variable = 93
                              , period = "2010"
                              , geo = rep("City",1)
                              , classific = c("c287","c2")
                              , category = list(c(93070,93084 # faixa idade
                                                  ,93085,107453
                                                  ,111286,93087
                                                  ,93088,93089
                                                  ,93090,93091
                                                  ,93092,93093
                                                  ,93094,93095
                                                  ,93096,93097
                                                  ,93098,93099
                                                  ,93100)
                                                ,c(4,5) # homem/mulher
                              )
                              , geo.filter = list("State" = i)
  )
  return(data.table::as.data.table(pop202))
}) %>% data.table::rbindlist()


names(popList) <- janitor::make_clean_names(names(popList))

popList1 <- data.table::copy(popList)
#  merge with other infos
popList1[munis_df1
            , on = c("municipio_codigo" = "code_muni")
            , ":="(
              code_intermediate = i.code_intermediate
              ,name_intermediate = i.name_intermediate
              ,code_imediate = i.regiao_geografica_imediata
              ,name_imediate = i.nome_regiao_geografica_imediata
              ,bacia = i.bacia
            )]

# save-----
readr::write_rds(x = popList1
                 ,file = "data/IBGE_estrutura_etaria.rds"
                 ,compress = "gz")
