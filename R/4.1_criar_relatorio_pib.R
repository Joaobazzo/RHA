# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(officedown)
library(flextable)
library(officer)

# 2) read source ----
source("R/5.0_main_source.R")

# 3) Loop through function ------
input_file <- readr::read_rds("data/munis_list.rds")
pib_raw <- readr::read_rds("data/pib_total_prep.rds")
pop_raw <- readr::read_rds("data/pop_proj_total_prep.rds")
capitais_raw <- readr::read_rds("data/capitais.rds")
ivs_raw <- readr::read_rds("data/ivs_idhm_muni.rds")
muni_area <- readxl::read_xls("data-raw/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xls"
                              ,sheet = "AR_BR_MUN_2021") %>% setDT()
cnae_raw <- readr::read_rds("data/CNAE_empregos.rds")
df_geral <- readr::read_rds("data/df_geral_muni.rds")



vector_unique_code <- unique(
  df_geral[regiao_total == 1,code_intermediate]
)
df_geral[code_intermediate == "5301"]

lapply(seq_along(vector_unique_code)# i = 55
       ,function(i){
         message(vector_unique_code[i])
         # i = 1
         # a) prepare names -----
         code_input <- vector_unique_code[i]
         file.copy(from = "inst/rmarkdown/relatorio_pop.docx"
                   ,to = sprintf("inst/rmarkdown/relatorio_pib/%s.docx",code_input)
         )
         
         arquivo_input <- normalizePath(
           sprintf("inst/rmarkdown/relatorio_pib/%s.docx",code_input)
         )
         
         # b) apply ----
         function_rel_pib(arquivo_resultado = arquivo_input
                          ,s_input = code_input)
         
         return(NULL)
         # 
       })
