# Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(readr)
library(googlesheets4)

df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral <- df_geral[!is.na(code_muni),]
df_geral[mun_pisf == 1,bacia := "PISF"]
df_geral[mun_bsf  == 1,bacia := "BSF"]
df_geral[mun_bpar == 1,bacia := "BPAR"]
df_geral[,c("mun_pisf", 
            "mun_bsf",  
            "mun_bpar" ) := NULL]
link_gdocs <- "https://docs.google.com/spreadsheets/d/1v9Dq_wxFTHz6lZgqxjhHtOl_Ybqlwz08Ze4mYHKueGY/edit?usp=sharing"

dir.create("data/export")
dir.create("data/export/cnae_ibge/")



# 1) ATIVIDADE -----
rm(list = ls()[!(ls() %in% c("link_gdocs","df_geral"))])

## lista ativ ----
emp_dt_raw <- data.table::fread("data-raw/Empregos e remuneracao/ActivitySpace2019.csv")
setnames(emp_dt_raw,"Codigo_Municipio","code_muni")
emp_dt_raw <- emp_dt_raw[,.SD,.SDcols = c("code_muni", "CNAE20", "Cor")]

## compt ativ ----
compl_ativ <- readr::read_rds("data/complexidade/complexidade_muni_export/complx_ecn_ativ.rds")
setnames(compl_ativ,"Activ","CNAE20")
setnames(compl_ativ,"name_actv","Descricao")
compl_ativ <- compl_ativ[,.SD,.SDcols = c("CNAE20",
                                          "Cor",
                                          "soma_pesos_complex_ativ_sim")]

## emprego ativ-----
emp_ativ <- readr::read_rds("data/complexidade/soma_pesos_empregos_e_remuneracoes_atividades_sim.rds")
emp_ativ <- emp_ativ[,.SD,.SDcols = c(
  "CNAE20"
  ,"Descricao"
  ,"Grupo"
  ,"Cor"
  ,"soma_pesos_empregos_e_remuneracoes_atividades_sim")]

# 
## compt municipio ---- 
compt_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
compt_muni <- compt_muni[,.SD,.SDcols = c("code_muni"
                                          ,"soma_total_competitividade_municipal")]




## merge ---------------
emp_dt_raw[,code_muni := as.character(code_muni)]

df <- emp_dt_raw %>%
  merge.data.table(x = .
                   ,y = df_geral
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  merge.data.table(x = .
                   ,y = compt_muni
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  merge.data.table(x = .
                   ,y = emp_ativ
                   ,by = c("CNAE20","Cor")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,y = compl_ativ
                   ,by = c("CNAE20","Cor")
                   ,all = TRUE)

df <- df[!is.na(CNAE20)]

#df <- df[regiao_total == "RTP"]
df
## save ----
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()


openxlsx::write.xlsx(x = df
                    ,file = "data/export/cnae_ibge/CNAE_ATIV.xlsx")


#googlesheets4::gs4_auth()
#googlesheets4::write_sheet(data = df
#                           ,ss = link_gdocs
#                           ,sheet = "CNAE_ATIV") 
# 2) PRODUTO -----
rm(list = ls()[!(ls() %in% c("link_gdocs","df_geral"))])
gc(reset = TRUE)

## prod muni -----
imp_dt <- data.table::fread("data-raw/Importacao e exportacao/IMP_2020_MUN.csv")
exp_dt <- data.table::fread("data-raw/Importacao e exportacao/EXP_2020_MUN.csv")
dic_imp <- data.table::fread("data-raw/Importacao e exportacao/codigos_sh4.csv")

prod_muni <- rbind(imp_dt[,tipo := "importacao"]
                   ,exp_dt[,tipo := "exportacao"])
prod_muni <- prod_muni[,.SD[1],by = c("SH4","CO_MUN")]
prod_muni <- prod_muni[,.SD,.SDcols = c("SH4","CO_MUN")]

setnames(prod_muni,"SH4","prod")
setnames(prod_muni,"CO_MUN","code_muni")
prod_muni[1]
rm(imp_dt)
rm(exp_dt)
rm(dic_imp)
prod_muni[,code_muni := as.character(code_muni)]

## lista prod ----
dt_prod <- data.table::fread("data-raw/Importacao e exportacao/codigos_sh4.csv")
names(dt_prod) <- janitor::make_clean_names(names(dt_prod))
setnames(dt_prod,"co_sh4","prod")
setnames(dt_prod,"no_sh4_por","name_prod")

## compt prod ----
compl_prod <- readr::read_rds("data/complexidade/complexidade_muni_export/complx_ecn_prod.rds")

compl_prod <- compl_prod[,.SD,.SDcols = c("name_prod",
                                          "Prod",
                                          "soma_pesos_complex_prod_sim")]
setnames(compl_prod,"Prod","prod")

## compt municipio ---- 
compt_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
compt_muni <- compt_muni[,.SD,.SDcols = c("code_muni"
                                          ,"soma_total_competitividade_municipal")]
compt_muni[,code_muni := as.character(code_muni)]


## merge ---------------

df <-   prod_muni %>% 
  # relacao prod - cod_muni
  merge.data.table(x = .
                   ,y = df_geral
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  .[!is.na(name_muni),] %>% 
  # competitividade muni
  merge.data.table(x = .
                   ,y = compt_muni
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  
  # complexidade produto
  merge.data.table(x = .
                   ,y = compl_prod
                   ,by = c("prod")
                   ,all = TRUE)


df <- df[!is.na(prod)]
df <- df[!is.na(name_prod)]
df[is.na(soma_total_competitividade_municipal), soma_total_competitividade_municipal := 0]


## save ----
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()


openxlsx::write.xlsx(x = df
                     ,file = "data/export/cnae_ibge/CNAE_PROD.xlsx")


#googlesheets4::gs4_auth()
#googlesheets4::write_sheet(data = df[1:1000,]
#                           ,ss = link_gdocs
#                           ,sheet = "CNAE_PROD") 
# 3) IBGE -----
rm(list = ls()[!(ls() %in% c("link_gdocs","df_geral"))])
gc(reset = TRUE)

## muni  produto_cod -----
dt3 <- read_rds("data/agro_table5457.rds")
dt3 <- dt3[,.SD,.SDcols = c("municipio_codigo"
                            ,"produto_das_lavouras_temporarias_e_permanentes"
                            ,"produto_das_lavouras_temporarias_e_permanentes_codigo")]
setnames(dt3
         ,old = c("produto_das_lavouras_temporarias_e_permanentes"
                  ,"produto_das_lavouras_temporarias_e_permanentes_codigo")
         ,new = c("produto","cod_produto"))
dt3 <- dt3[produto != "Total"]
dt3 <- unique(dt3)

cod_prod <- dt3
setnames(cod_prod,"municipio_codigo","code_muni")
rm(dt3)


## compt municipio ---- 
compt_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
compt_muni <- compt_muni[,.SD,.SDcols = c("code_muni"
                                          ,"soma_total_competitividade_municipal")]
compt_muni <- compt_muni[,.SD[1],by = .(code_muni)]

## prod pesos -----
ibge1 <- readr::read_rds("data/complexidade/complexidade_muni_export/agro_ibge.rds")
ibge1 <- ibge1[,.SD,.SDcols = c("produto","produto_cod","soma_pesos_agropecuaria")]
ibge1 <- ibge1[!is.na(produto_cod)]

## lista prod ----
ibge_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
ibge_muni[,total_agro := as.numeric(soma_pesos_classes_agricultura)]
ibge_muni <- ibge_muni[,.SD,.SDcols = c("code_muni","total_agro")]
ibge_muni <- unique(ibge_muni)

# merge
df <-   cod_prod  %>% 
  # municipios
  merge.data.table(x = .
                   ,y = df_geral
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  .[!is.na(name_muni),] %>% 
  # soma_pesos_agropecuaria
  merge.data.table(x = .
                   ,y = ibge_muni
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  # pesos produto ibge
  merge.data.table(x = .
                   ,y = ibge1[,produto := NULL]
                   ,by.x = "cod_produto"
                   ,by.y = "produto_cod"
                   ,all = TRUE) %>%
  # competividade by muni
  merge.data.table(x = .
                   ,y = compt_muni
                   ,by = "code_muni"
                   ,all = TRUE) 



# fix corrections
df <- df[!is.na(soma_total_competitividade_municipal)]
df <- df[!is.na(cod_produto)]
df[is.na(soma_pesos_agropecuaria), soma_pesos_agropecuaria := 0]
df[is.na(total_agro), total_agro := 0]

Colsdiff <- setdiff(names(df),names(df_geral))
Colsdiff

df <- df[,.SD,.SDcols = c(names(df_geral),Colsdiff)]
## save ----
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()

openxlsx::write.xlsx(x = df
                     ,file = "data/export/cnae_ibge/IBGE_AGRO.xlsx")

#googlesheets4::gs4_auth()
#googlesheets4::write_sheet(data = df
#                           ,ss = link_gdocs
#                           ,sheet = "IBGE_AGRO") 
# 4) IBGE AGRO -----
rm(list = ls()[!(ls() %in% c("link_gdocs","df_geral"))])
library(readr)
gc(reset = TRUE)

## muni  produto_cod -----
# agro_table3939.rds
dt1 <- read_rds("data/agro_table3939.rds")
dt1 <- dt1[,.SD,.SDcols = c("municipio_codigo","tipo_de_rebanho","tipo_de_rebanho_codigo")]
setnames(x = dt1
         ,old = c("tipo_de_rebanho","tipo_de_rebanho_codigo")
         ,new = c("produto","cod_produto"))
dt1 <- dt1[produto != "Total"]
dt1 <- unique(dt1)

# agro_table3940.rds
dt2 <- read_rds("data/agro_table3940.rds")
dt2 <- dt2[,.SD,.SDcols = c("municipio_codigo"
                          ,"tipo_de_produto_da_aquicultura"
                          ,"tipo_de_produto_da_aquicultura_codigo")]
setnames(dt2
         ,old = c("tipo_de_produto_da_aquicultura","tipo_de_produto_da_aquicultura_codigo")
         ,new = c("produto","cod_produto"))
dt2 <- dt2[produto != "Total"]

# agro_table5930.rds
dt3 <- read_rds("data/agro_table5930.rds")
dt3 <- dt3[,.SD,.SDcols = c("municipio_codigo"
                            ,"especie_florestal"
                            ,"especie_florestal_codigo")]
setnames(dt3
         ,old = c("especie_florestal","especie_florestal_codigo")
         ,new = c("produto","cod_produto"))
dt3 <- dt3[produto != "Total"]

cod_prod <- rbind(dt1,dt2,dt3)
cod_prod <- unique(cod_prod)
setnames(cod_prod,"municipio_codigo","code_muni")
rm(dt1)
rm(dt2)
rm(dt3)

## compt municipio ---- 
compt_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
compt_muni <- compt_muni[,.SD,.SDcols = c("code_muni"
                                          ,"soma_total_competitividade_municipal")]
compt_muni <- compt_muni[,.SD[1],by = .(code_muni)]

## prod pesos -----
ibge1 <- readr::read_rds("data/complexidade/complexidade_muni_export/agro_ibge_others.rds")
ibge1 <- ibge1[,.SD,.SDcols = c("tipo","cod","soma_pesos_competitividade_produto_rural_sim")]
ibge1 <- ibge1[!is.na(cod)]
setnames(ibge1,c("tipo",'cod'),c("produto",'produto_cod'))

## lista prod ----
ibge_muni <- readr::read_rds("data/complexidade/soma_total_competitividade_municipal.rds")
ibge_muni[,total_agro := 
            sum(
              as.numeric(soma_pesos_classes_rebanhos)
              , as.numeric(soma_pesos_classes_agricultura)
              , as.numeric(soma_pesos_classes_silvicultura) 
              , as.numeric(soma_pesos_classes_aquicultura)
              , na.rm = TRUE), by = .(code_muni)]


ibge_muni <- ibge_muni[,.SD,.SDcols = c("code_muni","total_agro")]

# merge
df <-   cod_prod  %>% 
  # municipios
  merge.data.table(x = .
                   ,y = df_geral
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  .[!is.na(name_muni),] %>% 
  # soma_pesos_agropecuaria
  merge.data.table(x = .
                   ,y = ibge_muni
                   ,by = c("code_muni")
                   ,all = TRUE) %>%
  # pesos produto ibge
  merge.data.table(x = .
                   ,y = ibge1[,produto := NULL]
                   ,by.x = "cod_produto"
                   ,by.y = "produto_cod"
                   ,all = TRUE) %>%
  # competividade by muni
  merge.data.table(x = .
                   ,y = compt_muni
                   ,by = "code_muni"
                   ,all = TRUE) 



# fix corrections
df <- df[!is.na(soma_total_competitividade_municipal)]
df <- df[!is.na(cod_produto)]
df[is.na(soma_pesos_competitividade_produto_rural_sim), soma_pesos_competitividade_produto_rural_sim := 0]
df[is.na(total_agro), total_agro := 0]

Colsdiff <- setdiff(names(df),names(df_geral))
Colsdiff

df <- df[,.SD,.SDcols = c(names(df_geral),Colsdiff)]
## save ----
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()


openxlsx::write.xlsx(x = df
                     ,file = "data/export/cnae_ibge/IBGE_AGRO_OUTROS.xlsx")

# End-----