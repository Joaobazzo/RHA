# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(googlesheets4)

my_division <- function(a,b){round(100 * a / b , 2)}
df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral <- df_geral[!is.na(code_muni) & regiao_total == 1,]
df_geral[mun_pisf == 1,bacia := "PISF"]
df_geral[mun_bsf == 1,bacia := "BSF"]
df_geral[mun_bpar == 1,bacia := "BPAR"]

#ativ_grupo_raw <- data.table::fread("data-raw/planilhas_basilio/Atividades.csv")

link_gdocs <- "https://docs.google.com/spreadsheets/d/1bgs9cHwGUHu75YjdPZSgjOZ1w6kWKI0MqdE2GFwHUu4/edit?usp=sharing"


# EMPREGO - TAM. ESTAB ------
rm(list = ls()[!(ls() %in% c("link_gdocs","my_division","df_geral","ativ_grupo_raw"))])
gc(reset  = TRUE)

ativ_grupo_raw <- data.table::fread("data-raw/planilhas_basilio/Atividades.csv")
ativ_grupo_raw

emp_dt_raw <- data.table::fread("data-raw/RAIS/RAIS_ESTAB_PUB.txt",encoding = "Latin-1")
names(emp_dt_raw) <- janitor::make_clean_names(names(emp_dt_raw))

emp_dt <- copy(emp_dt_raw)[qtd_vinculos_ativos > 0]
emp_dt[]

emp_dt <- emp_dt[,.SD,.SDcols = c("cnae_2_0_classe"
                                  ,"cnae_95_classe"
                                  ,"qtd_vinculos_clt"
                                  ,"qtd_vinculos_ativos"
                                  ,"natureza_juridica"
                                  ,"municipio"
                                  ,"uf")]

emp_dt[,range_vinculos_ativos := data.table::fcase(
  qtd_vinculos_ativos <= 20,"empregos_1_20",
  qtd_vinculos_ativos <= 49,"empregos_21_49",
  qtd_vinculos_ativos <= 99,"empregos_50_99",
  qtd_vinculos_ativos <= 500,"empregos_100_500",
  qtd_vinculos_ativos > 500,"empregos_500"
)]

emp_dt <- emp_dt[,
                 list(
                   "qtd_vinculos_clt" = sum(qtd_vinculos_clt,na.rm = TRUE)
                   ,"qtd_vinculos_ativos" = sum(qtd_vinculos_ativos,na.rm = TRUE)
                   ,"num_estab" = .N
                 )
                 ,by = .(cnae_2_0_classe
                         ,municipio, uf,range_vinculos_ativos)]
# merge 
emp_dt[,cnae_2_0_classe := as.character(cnae_2_0_classe)]
ativ_grupo_raw[,CNAE := as.character(CNAE)]
emp_dt <- emp_dt[ativ_grupo_raw
                 , on = c("cnae_2_0_classe" = "CNAE")]
emp_dt <- emp_dt[!is.na(municipio)]
# cod_ibge
df_geral[,code_rais := stringr::str_sub(as.character(code_muni),1,6)]
emp_dt[,municipio :=as.character(municipio)]
emp_dt <- emp_dt[df_geral
                 , on = c("municipio" = "code_rais")]
## RGINT  -----
setnames(emp_dt,"cnae_2_0_classe","CNAE20")

emp_rgint_empregos <- data.table::copy(emp_dt) %>% 
  .[,
    list(
      qtd_vinculos_ativos = sum(qtd_vinculos_ativos, na.rm = TRUE) 
      , num_estab = sum(num_estab, na.rm = TRUE)
    )
    , by = .(code_intermediate,range_vinculos_ativos,CNAE20,Descricao)]
emp_rgint_empregos

emp_rgint_empregos <- 
  data.table::setorder(x = emp_rgint_empregos[!is.na(code_intermediate)]
                       ,code_intermediate,range_vinculos_ativos,-num_estab)
emp_rgint_empregos[,N := 1:.N,by = .(code_intermediate,range_vinculos_ativos)]
emp_rgint_empregos[,N := fifelse(N<=20,1,0),by = .(code_intermediate,range_vinculos_ativos)]

emp_rgint_empregos_estab <-  dcast(data = emp_rgint_empregos
                                   , formula = CNAE20 + Descricao ~
                                     paste0("atv_top20_",range_vinculos_ativos,"_RGINT_",code_intermediate)
                                   , fill = 0
                                   , value.var = "N"
)


# EMPREGO - REM PER CAPITA -----
ativ_grupo_raw <- data.table::fread("data-raw/planilhas_basilio/Atividades.csv")
ativ_grupo_raw

emp_dt_raw <- data.table::fread("data-raw/Empregos e remuneracao/ActivitySpace2019.csv")
emp_dt_name <- data.table::fread("data-raw/Empregos e remuneracao/classeCnae.csv")

data.table::uniqueN(emp_dt_name$Cor)
data.table::uniqueN(emp_dt_raw$Cor)

data.table::setnames(emp_dt_raw,old = "Codigo_Municipio",new = "code_muni")
emp_dt_raw[,code_muni := as.character(code_muni)]

# merge data
emp_dt_raw <- emp_dt_raw[df_geral, on = "code_muni"]
emp_dt_raw <- emp_dt_raw[ativ_grupo_raw, on = c("CNAE20" = "CNAE")]
emp_dt_raw[,i.Cor := NULL]

## Estado -----

emp_state_empregos <- data.table::copy(emp_dt_raw) %>% 
  .[,
    list(
      Soma_Remun = sum(Soma_Remun, na.rm = TRUE) 
      , Contagem = sum(Contagem, na.rm = TRUE)
      , Rem_capita = sum(Soma_Remun, na.rm = TRUE) / 
        sum(Contagem, na.rm = TRUE)
    )
    , by = .(abbrev_state,CNAE20,Descricao)]

emp_state_empregos <- 
  data.table::setorder(x = emp_state_empregos[!is.na(abbrev_state)]
                       ,abbrev_state,-Rem_capita)
emp_state_empregos[,N := 1:.N,by = .(abbrev_state)]
emp_state_empregos[,N := fifelse(N<=20,1,0),by = .(abbrev_state)]

emp_state_empregos <-  dcast(data = emp_state_empregos
                             , formula = CNAE20 + Descricao ~
                               paste0("atv_top20_remun_capita_ESTADO_",abbrev_state)
                             , fill = 0
                             , value.var = "N"
)

##  Rgint----

emp_rgint_empregos <- data.table::copy(emp_dt_raw) %>% 
  .[,
    list(
      Soma_Remun = sum(Soma_Remun, na.rm = TRUE) 
      , Contagem = sum(Contagem, na.rm = TRUE)
      , Rem_capita = sum(Soma_Remun, na.rm = TRUE) / 
        sum(Contagem, na.rm = TRUE)
    )
    , by = .(Regiao_Intermediaria,CNAE20,Descricao)]

emp_rgint_empregos <- 
  data.table::setorder(x = emp_rgint_empregos[!is.na(Regiao_Intermediaria)]
                       ,Regiao_Intermediaria,-Rem_capita)
emp_rgint_empregos[,N := 1:.N,by = .(Regiao_Intermediaria)]
emp_rgint_empregos[,N := fifelse(N<=20,1,0),by = .(Regiao_Intermediaria)]


emp_rgint_empregos <-  dcast(data = emp_rgint_empregos
                             , formula = CNAE20 + Descricao ~
                               paste0("atv_top20_remun_capita_RGINT_",Regiao_Intermediaria)
                             , fill = 0
                             , value.var = "N"
)


##  Bacia----

emp_bacia_empregos <- data.table::copy(emp_dt_raw) %>% 
  .[,
    list(
      Soma_Remun = sum(Soma_Remun, na.rm = TRUE) 
      , Contagem = sum(Contagem, na.rm = TRUE)
      , Rem_capita = sum(Soma_Remun, na.rm = TRUE) / 
        sum(Contagem, na.rm = TRUE)
    )
    , by = .(bacia,CNAE20,Descricao)]

emp_bacia_empregos <- 
  data.table::setorder(x = emp_bacia_empregos[!is.na(bacia)]
                       ,bacia,-Rem_capita)
emp_bacia_empregos[,N := 1:.N,by = .(bacia)]
emp_bacia_empregos[,N := fifelse(N <= 20,1,0),by = .(bacia)]
emp_bacia_empregos[,Contagem := NULL]

emp_bacia_empregos <-  dcast(data = emp_bacia_empregos
                             , formula = CNAE20 + Descricao ~
                               paste0("atv_top20_remun_capita_BACIA_",bacia)
                             , fill = 0
                             , value.var = "N"
)

emp_bacia_empregos

##  RTP -----

emp_rtp_empregos <- data.table::copy(emp_dt_raw) %>% 
  .[,REGIAO_TOTAL := NULL] %>% 
  .[,REGIAO_TOTAL := "RTP"] %>% 
  .[,
    list(
      Soma_Remun = sum(Soma_Remun, na.rm = TRUE) 
      , Contagem = sum(Contagem, na.rm = TRUE)
      , Rem_capita = sum(Soma_Remun, na.rm = TRUE) / 
        sum(Contagem, na.rm = TRUE)
    )
    , by = .(REGIAO_TOTAL,CNAE20,Descricao)]

emp_rtp_empregos <- 
  data.table::setorder(x = emp_rtp_empregos[!is.na(REGIAO_TOTAL)]
                       ,REGIAO_TOTAL,-Rem_capita)
emp_rtp_empregos[,N := 1:.N,by = .(REGIAO_TOTAL)]
emp_rtp_empregos[,N := fifelse(N <= 20,1,0),by = .(REGIAO_TOTAL)]


emp_rtp_empregos <-  dcast(data = emp_rtp_empregos
                           , formula = CNAE20  + Descricao ~
                             paste0("atv_top20_remun_capita_",REGIAO_TOTAL)
                           , fill = 0
                           , value.var = "N"
)

## Merge ----

dt_out <- copy(emp_rgint_empregos_estab) %>%
  .[,CNAE20 := as.integer(CNAE20)] %>% 
  merge.data.table(x = .
                   ,emp_rgint_empregos
                   ,by = c( "CNAE20","Descricao")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,emp_state_empregos
                   ,by = c( "CNAE20","Descricao")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,y = emp_bacia_empregos
                   ,by = c( "CNAE20","Descricao")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,y = emp_rtp_empregos
                   ,by = c( "CNAE20","Descricao")
                   ,all = TRUE)

# adjust
dt_out[is.na(dt_out)] <- 0
dt_out <- dt_out[ativ_grupo_raw,on = c("CNAE20" = "CNAE","Descricao")]
atv_names <- c("CNAE20","Descricao","Grupo","Cor")
all_names <- names(dt_out)[!(names(dt_out) %in% atv_names)]
dt_out <- dt_out[,.SD,.SDcols = c(atv_names,all_names)]


## save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = dt_out
                           ,ss = link_gdocs
                           ,sheet = "COMP_ATIV_EMPREGOS_EMPRESAS") 
# EXPORTACAO ----
rm(list = ls()[!(ls() %in% c("emp_rgint_empregos_estab","link_gdocs","my_division","df_geral"))])
gc(reset  = TRUE)

imp_dt <- data.table::fread("data-raw/Importacao e exportacao/IMP_2020_MUN.csv")
exp_dt <- data.table::fread("data-raw/Importacao e exportacao/EXP_2020_MUN.csv")
dic_imp <- data.table::fread("data-raw/Importacao e exportacao/codigos_sh4.csv")

comex_dt <- rbind(imp_dt[,tipo := "importacao"]
                  ,exp_dt[,tipo := "exportacao"])

comex_dt[dic_imp,on = c("SH4" = "CO_SH4"),name_sh4_por := i.NO_SH4_POR]
comex_dt[,CO_MUN := as.character(CO_MUN)]
comex_dt <- comex_dt[df_geral,on = c("CO_MUN" = "code_muni")]

comex_dt <- comex_dt[!is.na(CO_MUN) & !is.na(tipo)]

comex_dt[1]
comex_dt$CO_ANO %>% unique()
comex_dt$CO_MES %>% unique()
comex_dt$SG_UF_MUN %>% unique()
comex_dt$CO_MUN %>% uniqueN()
comex_dt$code_intermediate %>% uniqueN()

## Rgint ------
# total comex
comex_rgint_dt <- copy(comex_dt)[, VL_FOB := sum(as.numeric(VL_FOB),na.rm = TRUE)
                                 ,by = .(code_intermediate,tipo,name_sh4_por)]
comex_rgint_dt <- comex_rgint_dt[, .SD[1]
                                 ,by = .(code_intermediate,tipo,name_sh4_por)]
comex_rgint_dt[,VL_FOB := VL_FOB/10^6]
# remove
comex_rgint_dt[,":="(name_muni =  NULL,CO_MUN = NULL
                     ,CO_ANO = NULL,CO_MES = NULL
                     ,CO_PAIS = NULL,SG_UF_MUN = NULL
                     ,KG_LIQUIDO = NULL)]
comex_rgint_dt[1:2]
comex_rgint_dt[code_intermediate == 2101]

# ordena | calcula top20
comex_rgint_dt <- 
  data.table::setorder(x = comex_rgint_dt[!is.na(code_intermediate)]
                       ,code_intermediate,tipo,-VL_FOB)
comex_rgint_dt[,N := 1:.N,by = .(code_intermediate,tipo)]
comex_rgint_dt[,N := fifelse(N<=20,1,0),by = .(code_intermediate,tipo)]
comex_rgint_dt[code_intermediate == 2101]

comex_rgint_dt <-  dcast(data = comex_rgint_dt
                         , formula = SH4 + name_sh4_por ~
                           paste0("prod_top20_",tipo,"_RGINT_",code_intermediate)
                         , fill = 0
                         , value.var = "N"
)
## Estado ------
# total comex
comex_state_dt <- copy(comex_dt)[, VL_FOB := sum(as.numeric(VL_FOB),na.rm = TRUE)
                                 ,by = .(abbrev_state,tipo,name_sh4_por)]
comex_state_dt <- comex_state_dt[, .SD[1]
                                 ,by = .(abbrev_state,tipo,name_sh4_por)]
comex_state_dt[,VL_FOB := VL_FOB/10^6]
# remove
comex_state_dt[,":="(name_muni =  NULL,CO_MUN = NULL
                     ,CO_ANO = NULL,CO_MES = NULL
                     ,CO_PAIS = NULL,SG_UF_MUN = NULL
                     ,KG_LIQUIDO = NULL)]
comex_state_dt[1:2]
comex_state_dt[abbrev_state == "MA"]

# ordena | calcula top20
comex_state_dt <- 
  data.table::setorder(x = comex_state_dt[!is.na(abbrev_state)]
                       ,abbrev_state,tipo,-VL_FOB)
comex_state_dt[,N := 1:.N,by = .(abbrev_state,tipo)]
comex_state_dt[,N := fifelse(N<=20,1,0),by = .(abbrev_state,tipo)]
comex_state_dt[abbrev_state == "MA"]

comex_state_dt <-  dcast(data = comex_state_dt
                         , formula = SH4 + name_sh4_por ~
                           paste0("prod_top20_",tipo,"_ESTADO_",abbrev_state)
                         , fill = 0
                         , value.var = "N"
)

comex_state_dt
## BACIA ------
# total comex
comex_bacia_dt <- copy(comex_dt)[!is.na(bacia), VL_FOB := sum(as.numeric(VL_FOB),na.rm = TRUE)
                                 ,by = .(bacia,tipo,name_sh4_por)]
comex_bacia_dt <- comex_bacia_dt[, .SD[1]
                                 ,by = .(bacia,tipo,name_sh4_por)]
comex_bacia_dt[,VL_FOB := VL_FOB/10^6]
# remove
comex_bacia_dt[,":="(name_muni =  NULL,CO_MUN = NULL
                     ,CO_ANO = NULL,CO_MES = NULL
                     ,CO_PAIS = NULL,SG_UF_MUN = NULL
                     ,KG_LIQUIDO = NULL)]
comex_bacia_dt[1:2]
comex_bacia_dt[bacia == "BPAR"]

# ordena | calcula top20
comex_bacia_dt <- 
  data.table::setorder(x = comex_bacia_dt[!is.na(bacia)]
                       ,bacia,tipo,-VL_FOB)
comex_bacia_dt[,N := 1:.N,by = .(bacia,tipo)]
comex_bacia_dt[,N := fifelse(N<=20,1,0),by = .(bacia,tipo)]
comex_bacia_dt[bacia == "BPAR"]

comex_bacia_dt <-  dcast(data = comex_bacia_dt
                         , formula = SH4 + name_sh4_por ~
                           paste0("prod_top20_",tipo,"_BACIA_",bacia)
                         , fill = 0
                         , value.var = "N"
)
## RTP ------
# total comex
comex_rtp_dt <- copy(comex_dt) %>% 
  .[,REGIAO_TOTAL := "RTP"] %>% 
  .[,VL_FOB := sum(as.numeric(VL_FOB),na.rm = TRUE)
    ,by = .(REGIAO_TOTAL,tipo,name_sh4_por)] %>% 
  .[, .SD[1],by = .(REGIAO_TOTAL,tipo,name_sh4_por)]

comex_rtp_dt[,VL_FOB := VL_FOB/10^6]
# remove
comex_rtp_dt[,":="(name_muni =  NULL,CO_MUN = NULL
                   ,CO_ANO = NULL,CO_MES = NULL
                   ,CO_PAIS = NULL,SG_UF_MUN = NULL
                   ,KG_LIQUIDO = NULL)]
comex_rtp_dt[1:2]


# ordena | calcula top20
comex_rtp_dt <- 
  data.table::setorder(x = comex_rtp_dt[!is.na(REGIAO_TOTAL)]
                       ,REGIAO_TOTAL,tipo,-VL_FOB)
comex_rtp_dt[,N := 1:.N,by = .(REGIAO_TOTAL,tipo)]
comex_rtp_dt[,N := fifelse(N<=20,1,0),by = .(REGIAO_TOTAL,tipo)]

comex_rtp_dt <-  dcast(data = comex_rtp_dt
                       , formula = SH4 + name_sh4_por ~
                         paste0("prod_top20_",tipo,"_",REGIAO_TOTAL)
                       , fill = 0
                       , value.var = "N"
)

comex_rtp_dt
## Merge ----

dt_out <- copy(comex_dt) %>%
  .[,.SD[1], by = c( "SH4","name_sh4_por")] %>% 
  .[,.SD,.SDcols = c( "SH4","name_sh4_por")] %>% 
  .[,SH4 := as.integer(SH4)] %>% 
  merge.data.table(x = .
                   ,comex_rgint_dt
                   ,by = c( "SH4","name_sh4_por")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,comex_state_dt
                   ,by = c( "SH4","name_sh4_por")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,comex_bacia_dt
                   ,by = c( "SH4","name_sh4_por")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,y = comex_rtp_dt
                   ,by = c( "SH4","name_sh4_por")
                   ,all = TRUE) 

### save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = dt_out
                           ,ss = link_gdocs
                           ,sheet = "PROD_BALANCA_COMERCIAL") 

# AGRO ----
rm(list = ls()[!(ls() %in% c("link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

agro <- readr::read_rds("data/agro_table5457.rds")
agro$municipio_codigo %>% uniqueN()
agro$variavel %>% unique()
agro$produto_das_lavouras_temporarias_e_permanentes %>% unique()

agro <- data.table::copy(agro)[!is.na(valor) &
                                 produto_das_lavouras_temporarias_e_permanentes != "Total"]
agro <- dcast(agro
              ,formula = municipio_codigo + municipio +
                produto_das_lavouras_temporarias_e_permanentes + 
                produto_das_lavouras_temporarias_e_permanentes_codigo ~ 
                variavel + unidade_de_medida
              ,value.var = "valor") 
names(agro) <- janitor::make_clean_names(names(agro))

# agro_muni
agro_muni <- copy(agro)[,
                        
                        list(muni_valor_da_producao_mil_reais = 
                               sum(valor_da_producao_mil_reais,na.rm = TRUE))
                        
                        ,by = municipio_codigo]
# merge
agro <- agro[df_geral
             , on = c("municipio_codigo" = "code_muni")]

setnames(agro
         ,old = c("produto_das_lavouras_temporarias_e_permanentes"
                  ,"produto_das_lavouras_temporarias_e_permanentes_codigo")
         ,new = c("produto","produto_cod"))
## Rgint-----
agro_rgint_dt <- copy(agro) %>% 
  .[,sum(valor_da_producao_mil_reais,na.rm = TRUE),by = .(code_intermediate,
                                                          produto,
                                                          produto_cod)]
agro_rgint_dt <- data.table::setorder(x = agro_rgint_dt[!is.na(code_intermediate)]
                                      ,code_intermediate,-V1)
agro_rgint_dt[,N := 1:.N,by = .(code_intermediate)]
agro_rgint_dt[,N := fifelse(N<=20,1,0),by = .(code_intermediate,produto)]
agro_rgint_dt[code_intermediate == "2101"]

agro_rgint_dt <-  dcast(data = agro_rgint_dt[!is.na(code_intermediate)]
                        , formula = produto + produto_cod ~
                          paste0("prod_top20_valor_agro_prod_RGINT_",code_intermediate)
                        , fill = 0
                        , value.var = "N"
)
## Bacia-----
agro_bacia_dt <- copy(agro) %>% 
  .[,sum(valor_da_producao_mil_reais,na.rm = TRUE),by = .(bacia,
                                                          produto,
                                                          produto_cod)]
agro_bacia_dt <- data.table::setorder(x = agro_bacia_dt[!is.na(bacia)]
                                      ,bacia,-V1)
agro_bacia_dt[,N := 1:.N,by = .(bacia)]
agro_bacia_dt[,N := fifelse(N<=20,1,0),by = .(bacia,produto)]


agro_bacia_dt <-  dcast(data = agro_bacia_dt[!is.na(bacia)]
                        , formula = produto + produto_cod ~
                          paste0("prod_top20_valor_agro_prod_BACIA_",bacia)
                        , fill = 0
                        , value.var = "N"
)
## Estado-----
agro_state_dt <- copy(agro) %>% 
  .[,sum(valor_da_producao_mil_reais,na.rm = TRUE),by = .(abbrev_state,
                                                          produto,
                                                          produto_cod)]
agro_state_dt <- data.table::setorder(x = agro_state_dt[!is.na(abbrev_state)]
                                      ,abbrev_state,-V1)
agro_state_dt[,N := 1:.N,by = .(abbrev_state)]
agro_state_dt[,N := fifelse(N<=20,1,0),by = .(abbrev_state,produto)]
agro_state_dt[abbrev_state == "MA"]

agro_state_dt <-  dcast(data = agro_state_dt[!is.na(abbrev_state)]
                        , formula = produto + produto_cod ~
                          paste0("prod_top20_valor_agro_prod_ESTADO_",abbrev_state)
                        , fill = 0
                        , value.var = "N"
)

## Merge ----

dt_out_agro <- copy(agro) %>%
  .[,.SD[1], by = c( "produto","produto_cod")] %>% 
  .[,.SD,.SDcols = c( "produto","produto_cod")] %>% 
  merge.data.table(x = .
                   ,agro_rgint_dt
                   ,by = c( "produto","produto_cod")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,agro_bacia_dt
                   ,by = c( "produto","produto_cod")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,agro_state_dt
                   ,by = c( "produto","produto_cod")
                   ,all = TRUE) 

### save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = dt_out_agro[!is.na(produto)]
                           ,ss = link_gdocs
                           ,sheet = "PROD_RURAL_AGRO") 

# AGRO other data -----

silv <- readr::read_rds("data/agro_table5930.rds"); silv[1]
aqui <- readr::read_rds("data/agro_table3940.rds"); aqui[1]
reb <- readr::read_rds("data/agro_table3939.rds"); reb[1]

# rename
setnames(silv,old = c("especie_florestal_codigo","especie_florestal")
         ,new = c("cod","tipo"))
setnames(aqui,old = c("tipo_de_produto_da_aquicultura_codigo","tipo_de_produto_da_aquicultura")
         ,new = c("cod","tipo"))
setnames(reb,old = c("tipo_de_rebanho_codigo","tipo_de_rebanho")
         ,new = c("cod","tipo"))

pec_dt <- list(silv
               ,aqui[variavel == "Valor da produção"]
               ,reb) %>% 
  rbindlist()
pec_dt <- pec_dt[df_geral,on = c("municipio_codigo" = "code_muni")]
pec_dt <- pec_dt[tipo != "Total",]

pec_dt[variavel == "Valor da produção",variavel := "aqui_valor_prod"]
pec_dt[variavel == "Efetivo dos rebanhos",variavel := "reban_prod_cabeca"]
pec_dt[variavel == "Área total existente em 31/12 dos efetivos da silvicultura"
       ,variavel := "silv_prod_area"]
#setnames(pec_dt,old = c("variavel", "variavel_codigo")
#         ,new = c("produto", "produto_codigo"))

## Rgint-----
pec_rgint_dt <- copy(pec_dt) %>% 
  .[,sum(valor,na.rm = TRUE),by = .(code_intermediate,
                                    tipo,
                                    cod,
                                    variavel)]
pec_rgint_dt <- data.table::setorder(x = pec_rgint_dt[!is.na(code_intermediate)]
                                     ,code_intermediate,tipo,-V1)
pec_rgint_dt[,N := 1:.N,by = .(code_intermediate,tipo)]
pec_rgint_dt[,N := fifelse(N >= 1,1,0),by = .(code_intermediate,tipo)]
pec_rgint_dt[code_intermediate == "2101"]

pec_rgint_dt <-  dcast(data = pec_rgint_dt[!is.na(code_intermediate)]
                       , formula = tipo + cod ~
                         paste0("prod_",variavel,"_RGINT_",code_intermediate)
                       , fill = 0
                       , value.var = "N"
)

pec_rgint_dt
## Bacia-----
pec_bacia_dt <- copy(pec_dt) %>% 
  .[,sum(valor,na.rm = TRUE),by = .(bacia,
                                    tipo,
                                    cod,
                                    variavel)]
pec_bacia_dt <- data.table::setorder(x = pec_bacia_dt[!is.na(bacia)]
                                     ,bacia,tipo,-V1)
pec_bacia_dt[,N := 1:.N,by = .(bacia,tipo)]
pec_bacia_dt[,N := fifelse(N >= 1,1,0),by = .(bacia,tipo)]
pec_bacia_dt

pec_bacia_dt <-  dcast(data = pec_bacia_dt[!is.na(bacia)]
                       , formula = tipo + cod ~
                         paste0("prod_",variavel,"_BACIA_",bacia)
                       , fill = 0
                       , value.var = "N")

## Estado-----
pec_estado_dt <- copy(pec_dt) %>% 
  .[,sum(valor,na.rm = TRUE),by = .(abbrev_state,
                                    tipo,
                                    cod,
                                    variavel)]
pec_estado_dt <- data.table::setorder(x = pec_estado_dt[!is.na(abbrev_state)]
                                      ,abbrev_state,tipo,-V1)
pec_estado_dt[,N := 1:.N,by = .(abbrev_state,tipo)]
pec_estado_dt[,N := fifelse(N >= 1,1,0),by = .(abbrev_state,tipo)]
pec_estado_dt

pec_estado_dt <-  dcast(data = pec_estado_dt[!is.na(abbrev_state)]
                        , formula = tipo + cod ~
                          paste0("prod_",variavel,"_ESTADO_",abbrev_state)
                        , fill = 0
                        , value.var = "N")
pec_estado_dt

## Merge ----

dt_out_pec <- copy(pec_dt) %>%
  .[,.SD[1], by = c( "tipo","cod")] %>% 
  .[,.SD,.SDcols = c( "tipo","cod")] %>% 
  merge.data.table(x = .
                   ,pec_rgint_dt
                   ,by = c( "tipo","cod")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,pec_bacia_dt
                   ,by = c( "tipo","cod")
                   ,all = TRUE) %>% 
  merge.data.table(x = .
                   ,pec_estado_dt
                   ,by = c( "tipo","cod")
                   ,all = TRUE) 
### save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = dt_out_pec[!is.na(tipo)]
                           ,ss = link_gdocs
                           ,sheet = "PROD_RURAL_PECUARIA") 

# Lista Atividade ------
rm(list = ls()[!(ls() %in% c("link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

ativ_grupo_raw <- data.table::fread("data-raw/planilhas_basilio/Atividades.csv")

ica_BPAR <- data.table::fread("data/complexidade/ASICA_BPAR.csv")
ica_BSF <-  data.table::fread("data/complexidade/ASICA_BSF.csv")
ica_PISF <- data.table::fread("data/complexidade/ASICA_PISF.csv")
ica_RTP <-  data.table::fread("data/complexidade/ASICA_Regiao.csv")
setnames(ica_RTP,"ACI","ICA")

# rbind
ica_dt <- list(
  ica_BPAR[Ano == 2019][,local := "bacia_BPAR"]
  , ica_BSF[Ano == 2019][,local := "bacia_BSF"]
  ,ica_PISF[Ano == 2019][,local := "bacia_PISF"]
  , ica_RTP[Ano == 2019][,local := "RTP"]
) %>% data.table::rbindlist()

# merge
ica_dt[1]
ica_dt[ativ_grupo_raw,on = c("Activ" = "CNAE"),name_actv := i.Descricao]

# create classe
ica_dt[,a_ranks := rank(ICA, ties.method = "first"), by = local]
ica_dt[,classe_ICA := cut(x = a_ranks
                          ,breaks =  quantile(a_ranks, probs= seq(0,10,2)/10)
                          , include.lowest=TRUE
                          , labels=FALSE), by = local]
ica_dt[,a_ranks := NULL]
ica_dt[, classe_ICA := fcase(classe_ICA %in% c(1,2),0.25,
                             classe_ICA %in% c(3),0.50,
                             classe_ICA %in% c(4,5),0.75)]
## Local -----
ica_local_dt <- copy(ica_dt) 
ica_local_dt <- data.table::setorder(x = ica_local_dt[!is.na(local)]
                                     ,local,name_actv,-classe_ICA)

ica_local_dt

ica_local_dt <-  dcast(data = ica_local_dt[!is.na(local)]
                       , formula = name_actv + Activ ~
                         paste0("classe_ICA_",local)
                       , fill = 0
                       , value.var = "classe_ICA")
ica_local_dt
# Lista AtividadeDiv -----
ica_div_BPAR <- data.table::fread("data/complexidade/ListaAtividadesDiv_BHRP.csv")
ica_div_BSF <-  data.table::fread("data/complexidade/ListaAtividadesDiv_BHSF.csv")
ica_div_PISF <- data.table::fread("data/complexidade/ListaAtividadesDiv_PISF.csv")
ica_div_RTP <-  data.table::fread("data/complexidade/ListaAtividadesDiv_Regiao.csv"
                                  ,encoding = "Latin-1")
ica_div_BPAR[1]
ica_div_BSF[1]
ica_div_PISF[1]
ica_div_RTP[1]
# rbind
ica_div_dt <- list(
  ica_div_BPAR[,Regiao := NULL][,local := "bacia_BPAR"]
  , ica_div_BSF[,Regiao := NULL][,local := "bacia_BSF"]
  , ica_div_PISF[,Regiao := NULL][,local := "bacia_PISF"]
  , ica_div_RTP[,Regiao := NULL][,local := "RTP"]
) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
ica_div_dt[, Phi := NULL]

ica_div_dt <- ica_div_dt[
  ,.SD
  ,.SDcols = c("ActivBase","ActivDiv","NomeBase","NomeDiv","local")]

ica_div_dt[1:2]

## Local -----
ica_local_div_dt <- copy(ica_div_dt) 
ica_local_div_dt <- melt(ica_local_div_dt
                         ,measure.vars = c("NomeBase","NomeDiv")
                         ,id.vars = c("ActivDiv" ,"ActivBase","local")
                         ,value.name = "Nome"
                         ,variable.name = "Tipo")
ica_local_div_dt[,Activ := fifelse(Tipo == "NomeBase",ActivBase,ActivDiv)]
ica_local_div_dt[,":="(ActivDiv = NULL,ActivBase = NULL)]
ica_local_div_dt[,Tipo := gsub("Nome","",Tipo)]


ica_local_div_dt <- dcast(data = ica_local_div_dt[!is.na(Activ)]
                          , formula = Nome + Activ ~
                            paste0("classe_ativ_",Tipo,"_",local)
                          , fill = 0)


### merge ----
ica_dt_out <- ica_dt %>% 
  .[,.SD[1],by = .(name_actv,Activ)] %>% 
  .[,.SD,.SDcols = c("name_actv","Activ")] %>% 
  .[ica_local_dt
    ,on = c("name_actv","Activ")] %>% 
  .[ica_local_div_dt
    ,on = c("name_actv" = "Nome","Activ")] 
  
  
### save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = ica_local_dt
                           ,ss = link_gdocs
                           ,sheet = "P_COMPLEX_ATIV") 
# Lista Produto ------
rm(list = ls()[!(ls() %in% c("link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

dic_imp <- data.table::fread("data-raw/Importacao e exportacao/codigos_sh4.csv")
icp_BPAR <- data.table::fread("data/complexidade/PSICP_BHRP.csv")
icp_BSF <-  data.table::fread("data/complexidade/PSICP_BHSF.csv")
icp_PISF <- data.table::fread("data/complexidade/PSICP_PISF.csv")
icp_RTP <-  data.table::fread("data/complexidade/PSICP_Regiao.csv")

# rbind
icp_dt <- list(
  icp_BPAR[Ano == 2019][,local := "bacia_BPAR"]
  , icp_BSF[Ano == 2019][,local := "bacia_BSF"]
  , icp_PISF[Ano == 2019][,local := "bacia_PISF"]
  , icp_RTP[Ano == 2019][,local := "RTP"]
) %>% data.table::rbindlist()

# merge
icp_dt[1]
dic_imp[1]
icp_dt[dic_imp,on = c("Prod" = "CO_SH4"),name_prod := i.NO_SH4_POR]

# create classe
icp_dt[,a_ranks := rank(ICP, ties.method = "first"), by = local]
icp_dt[,classe_ICP := cut(x = a_ranks
                          ,breaks =  quantile(a_ranks, probs= seq(0,10,2)/10)
                          , include.lowest=TRUE
                          , labels=FALSE), by = local]
icp_dt[,a_ranks := NULL]
icp_dt[, classe_ICP := fcase(classe_ICP %in% c(1,2),0.25,
                             classe_ICP %in% c(3),0.50,
                             classe_ICP %in% c(4,5),0.75)]
## Bacia-----
icp_local_dt <- copy(icp_dt) 
icp_local_dt <- data.table::setorder(x = icp_local_dt[!is.na(local)]
                                     ,local,name_prod,-classe_ICP)

icp_local_dt <-  dcast(data = icp_local_dt[!is.na(local)]
                       , formula = name_prod + Prod ~
                         paste0("classe_ICP_",local)
                       , fill = 0
                       , value.var = "classe_ICP")
icp_local_dt

# Lista ProdutoDiv -----
icp_div_BPAR <- data.table::fread("data/complexidade/ListaProdutosDiv_BHRP.csv")
icp_div_BSF <-  data.table::fread("data/complexidade/ListaProdutosDiv_BHSF.csv")
icp_div_PISF <- data.table::fread("data/complexidade/ListaProdutosDiv_PISF.csv")
#icp_div_RTP <-  data.table::fread("data/complexidade/ListaPr"
#                                  ,encoding = "Latin-1")
icp_div_BPAR[1]
icp_div_BSF[1]
icp_div_PISF[1]


# rbind
icp_div_dt <- list(
  icp_div_BPAR[,Regiao := NULL][,local := "bacia_BPAR"]
  , icp_div_BSF[,Regiao := NULL][,local := "bacia_BSF"]
  , icp_div_PISF[,Regiao := NULL][,local := "bacia_PISF"]
  #, icp_div_RTP[,Regiao := NULL][,local := "RTP"]
) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
icp_div_dt[, Phi := NULL]

icp_div_dt <- icp_div_dt[
  ,.SD
  ,.SDcols = c("ProdBase","ProdDiv","NomeBase","NomeDiv","local")]

icp_div_dt[1:2]

## Local -----
icp_local_div_dt <- copy(icp_div_dt) 
icp_local_div_dt <- melt(icp_local_div_dt
                         ,measure.vars = c("NomeBase","NomeDiv")
                         ,id.vars = c("ProdDiv" ,"ProdBase","local")
                         ,value.name = "Nome"
                         ,variable.name = "Tipo")
icp_local_div_dt[,Prod := fifelse(Tipo == "NomeBase",ProdBase,ProdDiv)]
icp_local_div_dt[,":="(ProdDiv = NULL,ProdBase = NULL)]
icp_local_div_dt[,Tipo := gsub("Nome","",Tipo)]


icp_local_div_dt <- dcast(data = icp_local_div_dt[!is.na(Prod)]
                          , formula = Nome + Prod ~
                            paste0("classe_prod_",Tipo,"_",local)
                          , fill = 0)
icp_local_div_dt
### merge ----
icp_dt_out <- icp_dt %>% 
  .[,.SD[1],by = .(Prod,name_prod)] %>% 
  .[,.SD,.SDcols = c("name_prod","Prod")] %>% 
  .[icp_local_dt
    ,on = c("name_prod","Prod")] %>% 
  .[icp_local_div_dt
    ,on = c("name_prod" = "Nome","Prod")] 

icp_dt_out
### save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = icp_local_dt
                           ,ss = link_gdocs
                           ,sheet = "P_COMPLEX_PROD") 
