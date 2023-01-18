##Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(officedown)
library(flextable)
library(officer)

# AGRO ------------- 
### read source ----
source("R/5.0_main_source_new_data.R")

### Loop through function ------

#input_file <- readr::read_rds("data/munis_list.rds")
#pop_raw <- readr::read_rds("data/pop_proj_total_prep.rds")
#capitais_raw <- readr::read_rds("data/capitais.rds")
#muni_area <- readxl::read_xls("data-raw/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xls"
#                              ,sheet = "AR_BR_MUN_2021") %>% setDT()
df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral[,bacia := fcase(mun_pisf == 1,"PISF",
                         mun_bsf == 1,"BSF",
                         mun_bpar == 1, "BPAR")]

CodesMunis <- unique(df_geral[regiao_total == 1,code_muni])

dt_agro <- readr::read_rds("data/agro_table5457.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_silv <- readr::read_rds("data/agro_table5930.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_aqui <- readr::read_rds("data/agro_table3940.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_reb <- readr::read_rds("data/agro_table3939.rds")  %>% .[municipio_codigo %in% CodesMunis,]
dt_irr <- readr::read_rds("data/agro_table6858.rds")  %>% .[municipio_codigo %in% CodesMunis,]

vector_unique_code <- unique(
  df_geral[regiao_total == 1,code_intermediate]
)


dir.create("inst/rmarkdown/relatorio_agro")

lapply(seq_along(vector_unique_code)]# i = 2
       ,function(i){
         
         message(vector_unique_code[i])
         # prepare names 
         code_input <- vector_unique_code[i]
         file_name <- sprintf("inst/rmarkdown/relatorio_agro/%s.docx",code_input)
         
         # copy
         file.copy(from = "inst/rmarkdown/relatorio_pop.docx"
                   ,to = file_name)
         
         # apply
         arquivo_input <- normalizePath(file_name)
         function_rel_pib(arquivo_resultado = arquivo_input
                          ,s_input = code_input)
         return(NULL)
         
       })


# 2) read source ----
source("R/5.0_main_source_new_data.R")

# 3) Loop through function ------


df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral[,bacia := fcase(mun_pisf == 1,"PISF",
                         mun_bsf == 1,"BSF",
                         mun_bpar == 1, "BPAR")]

CodesMunis <- unique(df_geral[regiao_total == 1,code_muni])

dt_agro <- readr::read_rds("data/agro_table5457.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_silv <- readr::read_rds("data/agro_table5930.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_aqui <- readr::read_rds("data/agro_table3940.rds") %>% .[municipio_codigo %in% CodesMunis,]
dt_reb <- readr::read_rds("data/agro_table3939.rds")  %>% .[municipio_codigo %in% CodesMunis,]
dt_irr <- readr::read_rds("data/agro_table6858.rds")  %>% .[municipio_codigo %in% CodesMunis,]

vector_unique_code <- unique(
  df_geral[regiao_total == 1,code_intermediate]
)


lapply(seq_along(vector_unique_code)[-c(1:2)]# i = 2
       ,function(i){
         message(vector_unique_code[i])
         # i = 1
         # a) prepare names -----
         code_input <- vector_unique_code[i]
         file_name <- sprintf("inst/rmarkdown/relatorio_new_data/%s.docx",code_input)
         
         # copy
         file.copy(from = "inst/rmarkdown/relatorio_pop.docx"
                   ,to = file_name
         )
         # b) apply ----
         arquivo_input <- normalizePath(file_name)
         
         function_rel_pib(arquivo_resultado = arquivo_input
                          ,s_input = code_input)
         
         return(NULL)
         # 
       })