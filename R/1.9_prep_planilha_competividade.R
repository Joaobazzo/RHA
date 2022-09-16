# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(readr)
library(googlesheets4)

df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral <- df_geral[!is.na(code_muni) & regiao_total == 1,]
df_geral[mun_pisf == 1,bacia := "PISF"]
df_geral[mun_bsf == 1,bacia := "BSF"]
df_geral[mun_bpar == 1,bacia := "BPAR"]
df_geral[,regiao_total := "RTP"]

colToRem <- c("name_muni", "code_state","name_state"
              ,"name_region","code_rgi","name_rgi" 
              ,"name_intermediate","mun_sudene","mun_pisf","mun_bsf"
              ,"mun_bpar", "semi_arido")

link_gdocs <- "https://docs.google.com/spreadsheets/d/1bgs9cHwGUHu75YjdPZSgjOZ1w6kWKI0MqdE2GFwHUu4/edit?usp=sharing"

# Read ----
rm(list = ls()[!(ls() %in% c("colToRem","link_gdocs","df_geral"))])
gc(reset = TRUE)

## 1 - polit rural ----
dt1 <- read_rds("data/complexidade/complexidade_muni_prep_data/POLIT_RURAL.rds")
dt1 <- dt1[,(colToRem) := NULL]

dt1[,c("abbrev_state","code_intermediate", "regiao_total", "bacia") := NULL]


## 2 - pib -----
dt2 <- read_rds("data/complexidade/complexidade_muni_prep_data/pib.rds")
dt2 <- dt2[,(colToRem) := NULL]

names(dt2)
dt2 <- dt2[,.SD,.SDcols = c("code_muni"
                            ,"classe_area_vulneraveis"
                            ,"classe_area_destaque"
                            ,"peso_VAB_COMBINACAO"
                            ,"classe_ICEM_Ativ"
                            ,"peso_ICEM_Prod"
                            ,"peso_VAB_Adm_pp"
                            ,"pont_total_pib"
                            ,"classe_VAB_Agro_pp"
                            ,"peso_VAB_COMB_Agro_Serv_altos"
                            ,"classe_VAB_Ind_pp"
                            ,"peso_VAB_COMB_Ind_Serv_altos"
                            ,"peso_VAB_COMB_Agro_Ind_altos"
)]

setnames(dt2,"peso_ICEM_Prod","classe_ICEM_Prod_alto")

dt2[,classe_ICEM_Prod_alto := fifelse(classe_ICEM_Prod_alto == 0.75,0.5,0)]
dt2[,pont_total_pib := classe_area_vulneraveis +
      classe_area_destaque + 
      classe_ICEM_Ativ]

## 3 - agro ----

dt3 <- read_rds("data/complexidade/complexidade_muni_prep_data/AGRO.rds")
dt3 <- dt3[,(colToRem) := NULL]

# valor agro
valor_agro_Estado <- dt3[,sum(muni_valor_da_producao_mil_reais,na.rm = TRUE),by = .(abbrev_state)]
valor_agro_Bacia <- dt3[,sum(muni_valor_da_producao_mil_reais,na.rm = TRUE),by = .(bacia)]
valor_agro_RTP <- dt3[,sum(muni_valor_da_producao_mil_reais,na.rm = TRUE),by = .(regiao_total)] 
valor_agro_RGINT <- dt3[,sum(muni_valor_da_producao_mil_reais,na.rm = TRUE),by = .(code_intermediate)] 

dt3[valor_agro_Estado, on = "abbrev_state"    ,valor_agro_Estado := i.V1]
dt3[valor_agro_Bacia, on = "bacia"            ,valor_agro_Bacia := i.V1]
dt3[valor_agro_RTP, on = "regiao_total"       ,valor_agro_RTP := i.V1]
dt3[valor_agro_RGINT, on = "code_intermediate",valor_agro_RGINT := i.V1]

dt3[ ,p_RGINT_perc_valor_agro_Estado := valor_agro_RGINT / valor_agro_Estado]
dt3[ ,p_RGINT_perc_valor_agro_Bacia :=  valor_agro_RGINT / valor_agro_Bacia]
dt3[ ,p_RGINT_perc_valor_agro_RTP :=    valor_agro_RGINT / valor_agro_RTP]

# valor muni_valor_silv
valor_silv_Estado <- dt3[,sum(muni_valor_silv,na.rm = TRUE),by = .(abbrev_state)]
valor_silv_Bacia <- dt3[,sum(muni_valor_silv,na.rm = TRUE),by = .(bacia)]
valor_silv_RTP <- dt3[,sum(muni_valor_silv,na.rm = TRUE),by = .(regiao_total)] 
valor_silv_RGINT <- dt3[,sum(muni_valor_silv,na.rm = TRUE),by = .(code_intermediate)] 

dt3[valor_silv_Estado, on = "abbrev_state"    ,valor_silv_Estado := i.V1]
dt3[valor_silv_Bacia, on = "bacia"            ,valor_silv_Bacia := i.V1]
dt3[valor_silv_RTP, on = "regiao_total"       ,valor_silv_RTP := i.V1]
dt3[valor_silv_RGINT, on = "code_intermediate",valor_silv_RGINT := i.V1]

dt3[ ,p_RGINT_perc_silv_Estado :=  valor_silv_RGINT / valor_silv_Estado]
dt3[ ,p_RGINT_perc_silv_Bacia  :=  valor_silv_RGINT / valor_silv_Bacia]
dt3[ ,p_RGINT_perc_silv_RTP    :=  valor_silv_RGINT / valor_silv_RTP]

# valor muni_valor_aqui
valor_aqui_Estado <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(abbrev_state)]
valor_aqui_Bacia <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(bacia)]
valor_aqui_RTP <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(regiao_total)] 
valor_aqui_RGINT <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(code_intermediate)] 

dt3[valor_aqui_Estado, on = "abbrev_state"    ,valor_aqui_Estado := i.V1]
dt3[valor_aqui_Bacia, on = "bacia"            ,valor_aqui_Bacia := i.V1]
dt3[valor_aqui_RTP, on = "regiao_total"       ,valor_aqui_RTP := i.V1]
dt3[valor_aqui_RGINT, on = "code_intermediate",valor_aqui_RGINT := i.V1]

dt3[ ,p_RGINT_perc_aqui_Estado :=  valor_aqui_RGINT / valor_aqui_Estado]
dt3[ ,p_RGINT_perc_aqui_Bacia  :=  valor_aqui_RGINT / valor_aqui_Bacia]
dt3[ ,p_RGINT_perc_aqui_RTP    :=  valor_aqui_RGINT / valor_aqui_RTP]

# valor reban_mais_prod_cabeca

valor_reb_Estado <- dt3[,sum(valor_reban_mais_prod_cabeca,na.rm = TRUE),by = .(abbrev_state)]
valor_reb_Bacia <- dt3[,sum(valor_reban_mais_prod_cabeca,na.rm = TRUE),by = .(bacia)]
valor_reb_RTP <- dt3[,sum(valor_reban_mais_prod_cabeca,na.rm = TRUE),by = .(regiao_total)] 
valor_reb_RGINT <- dt3[,sum(valor_reban_mais_prod_cabeca,na.rm = TRUE),by = .(code_intermediate)] 

dt3[valor_reb_Estado, on = "abbrev_state"    ,valor_reb_Estado := i.V1]
dt3[valor_reb_Bacia, on = "bacia"            ,valor_reb_Bacia := i.V1]
dt3[valor_reb_RTP, on = "regiao_total"       ,valor_reb_RTP := i.V1]
dt3[valor_reb_RGINT, on = "code_intermediate",valor_reb_RGINT := i.V1]

dt3[ ,p_RGINT_perc_reb_Estado := valor_reb_RGINT / valor_reb_Estado]
dt3[ ,p_RGINT_perc_reb_Bacia  := valor_reb_RGINT / valor_reb_Bacia]
dt3[ ,p_RGINT_perc_reb_RTP    := valor_reb_RGINT / valor_reb_RTP]


names(dt3)
# classe_muni_valor_da_producao

dt3[, classe_muni_valor_da_producao := {
  my_ranks <- rank(muni_valor_da_producao_mil_reais, ties.method = "first")
  decile <- cut(my_ranks
                , quantile(my_ranks, probs=0:5/5)
                , include.lowest=TRUE
                , labels= FALSE)
  decile <- as.character(decile)
}]

# classe_agro_valor_da_producao_RGINT_perc_Estado

dt3[, classe_agro_valor_da_producao_RGINT_perc_Estado := {
  
  mybreaks <- as.numeric(quantile(
    p_RGINT_perc_valor_agro_Estado
    ,probs =  0:5/6))
  
  if(uniqueN(mybreaks)<6){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
  }
  
  decile <- cut(x =  p_RGINT_perc_valor_agro_Estado
                , breaks =  mybreaks
                , include.lowest=TRUE
                , labels= c("0.25","0.25","0.5","0.75","0.75"))
  
  decile <- as.character(decile)
}, by = .(abbrev_state)]

# classe_agro_valor_da_producao_RGINT_perc_Bacia

dt3[!is.na(p_RGINT_perc_valor_agro_Bacia)
    , classe_agro_valor_da_producao_RGINT_perc_Bacia := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_valor_agro_Bacia
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_valor_agro_Bacia
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(bacia)]

# classe_agro_valor_da_producao_RGINT_perc_RTP

dt3[!is.na(p_RGINT_perc_valor_agro_RTP)
    , classe_agro_valor_da_producao_RGINT_perc_RTP := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_valor_agro_RTP
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_valor_agro_RTP
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(regiao_total)]


# classe_muni_valor_silv

dt3[, classe_muni_valor_silv := as.character( 
  cut(muni_valor_silv
      , breaks = Hmisc::wtd.quantile(
        x = muni_valor_silv
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

# classe_muni_valor_silv_RGINT_perc_Estado

dt3[!is.na(p_RGINT_perc_silv_Estado)
    , classe_muni_valor_silv_RGINT_perc_Estado := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_silv_Estado
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_silv_Estado
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(abbrev_state)]


# classe_muni_valor_silv_RGINT_perc_Bacia
dt3[!is.na(p_RGINT_perc_silv_Bacia)
    , classe_muni_valor_silv_RGINT_perc_Bacia := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_silv_Bacia
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_silv_Bacia
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(bacia)]


# classe_muni_valor_silv_RGINT_perc_ RTP
dt3[!is.na(p_RGINT_perc_silv_RTP)
    , classe_muni_valor_silv_RGINT_perc_RTP := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_silv_RTP
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_silv_RTP
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(regiao_total)]

# classe_muni_valor_aqui
dt3[, classe_muni_valor_aqui := as.character( 
  cut(muni_valor_aqui
      , breaks = Hmisc::wtd.quantile(
        x = muni_valor_aqui
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]


# classe_muni_valor_aqui_RGINT_perc_Estado
dt3[!is.na(p_RGINT_perc_aqui_Estado)
    , classe_muni_valor_aqui_RGINT_perc_Estado := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_aqui_Estado
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_aqui_Estado
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(abbrev_state)]

# classe_muni_valor_aqui_RGINT_perc_Bacia
dt3[!is.na(p_RGINT_perc_aqui_Bacia)
    , classe_muni_valor_aqui_RGINT_perc_Bacia := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_aqui_Bacia
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_aqui_Bacia
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(bacia)]

# classe_muni_valor_aqui_RGINT_perc_RTP
dt3[!is.na(p_RGINT_perc_aqui_RTP)
    , classe_muni_valor_aqui_RGINT_perc_RTP := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_aqui_RTP
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_aqui_RTP
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(regiao_total)]

# classe_muni_valor_rebanho
dt3[, classe_muni_valor_rebanho := as.character( 
  cut(p_rgint_bacia_area_aqui
      , breaks = Hmisc::wtd.quantile(
        x = p_rgint_bacia_area_aqui
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]


# classe_muni_valor_reb_RGINT_perc_Estado
dt3[!is.na(p_RGINT_perc_reb_Estado)
    , classe_muni_valor_reb_RGINT_perc_Estado := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_reb_Estado
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_reb_Estado
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(abbrev_state)]

# classe_muni_valor_reb_RGINT_perc_Bacia
dt3[!is.na(p_RGINT_perc_reb_Bacia)
    , classe_muni_valor_reb_RGINT_perc_Bacia := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_reb_Bacia
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_reb_Bacia
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(bacia)]

# classe_muni_valor_reb_RGINT_perc_RTP
dt3[!is.na(p_RGINT_perc_reb_RTP)
    , classe_muni_valor_reb_RGINT_perc_RTP := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_reb_RTP
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_reb_RTP
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(regiao_total)]
dt3[,c("abbrev_state", "code_intermediate", "regiao_total", "bacia") := NULL]

# soma de tudo

dt3[is.na(dt3)] <- 0

dt3[,soma_pesos_classes_agricultura := 
      as.numeric(classe_muni_valor_da_producao) + 
      as.numeric(classe_agro_valor_da_producao_RGINT_perc_Estado) + 
      as.numeric(classe_agro_valor_da_producao_RGINT_perc_Bacia) +
      as.numeric(classe_agro_valor_da_producao_RGINT_perc_RTP)]

dt3[,soma_pesos_classes_silvicultura := 
      as.numeric(classe_muni_valor_silv) +
      as.numeric(classe_muni_valor_silv_RGINT_perc_Estado) +
      as.numeric(classe_muni_valor_silv_RGINT_perc_Bacia) +
      as.numeric(classe_muni_valor_silv_RGINT_perc_RTP)]

dt3[,soma_pesos_classes_aquicultura := 
      as.numeric(classe_muni_valor_aqui) +
      as.numeric(classe_muni_valor_aqui_RGINT_perc_Estado) +
      as.numeric(classe_muni_valor_aqui_RGINT_perc_Bacia) +
      as.numeric(classe_muni_valor_aqui_RGINT_perc_RTP)]

dt3[,soma_pesos_classes_rebanhos := 
      as.numeric(classe_muni_valor_rebanho) +
      as.numeric(classe_muni_valor_reb_RGINT_perc_Estado) +
      as.numeric(classe_muni_valor_reb_RGINT_perc_Bacia) +
      as.numeric(classe_muni_valor_reb_RGINT_perc_RTP)]

## 4 - populacao ----

dt4 <- read_rds("data/complexidade/complexidade_muni_prep_data/POPULACAO.rds")
dt4 <- dt4[,(colToRem) := NULL]


dt4[, classe_perc_urbana_2010 := as.character( 
  cut(perc_urbana_2010
      , breaks = Hmisc::wtd.quantile(
        x = perc_urbana_2010
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

dt4[, classe_perc_rural_2010 := as.character( 
  cut(perc_rural_2010
      , breaks = Hmisc::wtd.quantile(
        x = perc_rural_2010
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

dt4[,c("abbrev_state", "code_intermediate") := NULL]

## 5 - balanca comercial ----

dt5 <- read_rds("data/complexidade/complexidade_muni_prep_data/BALANCA_COMERCIAL.rds")
dt5 <- dt5[,(colToRem) := NULL]
dt5 <- dt5[!is.na(total_filtro_7)]
dt5[,regiao_total := "regiao_total"]


dt5[, classe_exportacao := as.character( 
  cut(exportacao
      , breaks = Hmisc::wtd.quantile(
        x = exportacao
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

# Estado

dt5[!is.na(p_RGINT_perc_exp_Estado)
    , classe_exp_RGINT_perc_exp_Estado := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_exp_Estado
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_exp_Estado
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(abbrev_state)]

# Bacia

dt5[!is.na(p_RGINT_perc_exp_Bacia)
    , classe_exp_RGINT_perc_exp_Bacia := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_exp_Bacia
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_exp_Bacia
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(bacia)]

# RTP

dt5[!is.na(p_RGINT_perc_exp_RTP)
    , classe_exp_RGINT_perc_exp_RTP := {
      
      mybreaks <- as.numeric(quantile(
        p_RGINT_perc_exp_RTP
        ,probs =  0:5/6))
      
      if(uniqueN(mybreaks)<6){
        mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
      }
      
      decile <- cut(x =  p_RGINT_perc_exp_RTP
                    , breaks =  mybreaks
                    , include.lowest=TRUE
                    , labels= c("0.25","0.25","0.5","0.75","0.75"))
      
      decile <- as.character(decile)
    }, by = .(regiao_total)]


dt5[,c("abbrev_state", "code_intermediate", "regiao_total", "bacia") := NULL]

dt5[is.na(dt5)] <- 0
dt5[,total_classe_comex := 
      as.numeric(classe_exportacao)  + 
      as.numeric(classe_exp_RGINT_perc_exp_Estado)  +
      as.numeric(classe_exp_RGINT_perc_exp_Bacia ) +
      as.numeric(classe_exp_RGINT_perc_exp_RTP)
]

## 6 - aparato institucional ----

dt6 <- read_rds("data/complexidade/complexidade_muni_prep_data/POLIT_RURAL.rds")
dt6 <- dt6[,(colToRem) := NULL]
dt6 <- dt6[regiao_total == 1,]

dt6[1]
dt6[,c("abbrev_state", "code_intermediate", "regiao_total", "bacia") := NULL]

## 7 - educacao -----

dt7 <- read_rds("data/complexidade/complexidade_muni_prep_data/EDUCACAO.rds")
dt7[,(colToRem) := NULL]
dt7 <- dt7[regiao_total == 1,]
dt7[,c("abbrev_state", "code_intermediate", "regiao_total") := NULL]

# classe_inv_educ_infantil
dt7[, classe_inv_educ_infantil := {
  mybreaks <- quantile(x = as.numeric(Inv_educ_infantil)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(Inv_educ_infantil)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_inv_educ_basica
dt7[!is.na(Inv_educ_basica), classe_inv_educ_basica := as.character( 
  cut(Inv_educ_basica
      , breaks = Hmisc::wtd.quantile(
        x = Inv_educ_basica
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

# classe_inv_ensino_funmental
dt7[, classe_inv_ensino_funmental := {
  
  mybreaks <- quantile(x = as.numeric(Inv_ensino_funmental)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(Inv_ensino_funmental
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
}]

# classe_inv_ensino_medio
dt7[Inv_ensino_medio > 0, classe_inv_ensino_medio := {
  mybreaks <- as.numeric(quantile(
    Inv_ensino_medio
    ,probs =  0:5/6
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<6){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 6)
  }
  
  decile <- cut(x =  Inv_ensino_medio
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c("0.25","0.25","0.5","0.75","0.75"))
  
  decile <- as.character(decile)
}]

dt7[ Inv_ensino_medio > 0,"classe_inv_ensino_medio"]

# classe_inv_educ_profissional

dt7[Inv_educ_profissional > 0, classe_inv_educ_profissional := {
  mybreaks <- as.numeric(quantile(
    Inv_educ_profissional
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  Inv_educ_profissional
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
}]

dt7[ Inv_educ_profissional > 0,"classe_inv_educ_profissional"]

dt7[is.na(dt7)] <- 0
dt7[, soma_pesos_educacao := 
      as.numeric(classe_inv_educ_infantil) +
      as.numeric(classe_inv_educ_basica) +
      as.numeric(classe_inv_ensino_funmental) +
      as.numeric(classe_inv_ensino_medio) +
      as.numeric(classe_inv_educ_profissional)
]


## 8 - fne -----

dt8 <- read_rds("data/complexidade/complexidade_muni_prep_data/FNE.rds")
dt8 <- dt8[regiao_total == 1]

dt8[, classe_inv_rural := {
  mybreaks <- quantile(x = as.numeric(inv_rural)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(inv_rural)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_inv_urbana

dt8[, classe_inv_urbana := {
  mybreaks <- quantile(x = as.numeric(inv_urbano)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(inv_urbano)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_inv_pj

dt8[, classe_inv_pj := {
  mybreaks <- quantile(x = as.numeric(inv_pj)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(inv_pj)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
}]

# classe_inv_comercio

dt8[, classe_inv_comercio := {
  mybreaks <- quantile(x = as.numeric(inv_comercio)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(inv_comercio)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
}]

# classe_num_contratos

dt8[, classe_num_contratos := {
  mybreaks <- quantile(x = as.numeric(num_contratos)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(num_contratos)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_inv_turismo

dt8[, classe_inv_turismo := {
  mybreaks <- as.numeric(quantile(
    inv_turismo
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  inv_turismo
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

# classe_investimento

dt8[is.na(dt8)] <- 0
dt8[,inv_total := as.numeric(inv_pf) +  as.numeric(inv_pj)]
dt8[, classe_investimento := {
  mybreaks <- as.numeric(quantile(
    inv_total
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  inv_total
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
}]

dt8[, soma_pesos_fne := 
      as.numeric(classe_num_contratos) +
      as.numeric(classe_investimento) +
      as.numeric(classe_inv_pj)+
      as.numeric(classe_inv_comercio) + 
      as.numeric(classe_inv_turismo)]

# classe_inv_agricola
dt8[, classe_inv_agricola := {
  mybreaks <- as.numeric(quantile(
    inv_agr
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  inv_agr
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

# classe_inv_agroindustria
dt8[, classe_inv_agroindustria := {
  mybreaks <- as.numeric(quantile(
    inv_agroind
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  inv_agroind
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

# classe_inv_pecuaria
dt8[, classe_inv_pecuaria := {
  mybreaks <- as.numeric(quantile(
    inv_pec
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  inv_pec
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

# soma_classes_fne_rural
dt8[is.na(dt8)] <- 0

dt8[,soma_classes_fne_rural := 
      as.numeric(classe_inv_agricola)+
      as.numeric(classe_inv_agroindustria)+
      as.numeric(classe_inv_pecuaria)]

## 9 - complexidade_economica -----

dt9 <-  data.table::fread("data/complexidade/ListaProdutosDiv_Municipio.csv")

dt9 <- dt9[,.N,by = .(Municipio)]
dt9[,classe_prod_base_sim := 0.5]
setnames(dt9,"Municipio","code_muni")

dt9 <- dt9[,.SD,.SDcols = c("code_muni","classe_prod_base_sim")]

dt9

dt9[,ativ_div := classe_prod_base_sim]


dt9[1]

##  10 - politica urbana -----

dt10 <- read_rds("data/complexidade/complexidade_muni_prep_data/URBANISMO.rds")
dt10 <- dt10[,(colToRem) := NULL]


dt10[,c("abbrev_state", "code_intermediate", "regiao_total") := NULL]


dt10[, classe_controle_urb := {
  mybreaks <- as.numeric(quantile(
    total_con_urb
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  total_con_urb
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

## 11/12/13 - aparato institucional ----


dt11 <- openxlsx::read.xlsx("data-raw/aparato_institucional/Sedes da Embrapa.xlsx"
                            ,sheet = "Projeto")
setDT(dt11)
names(dt11) <- janitor::make_clean_names(names(dt11))
dt11[1]
setnames(dt11,"codigo_dos_municipios","code_muni")
dt11 <- dt11[,.SD,.SDcols = "code_muni"]
dt11[,code_muni := gsub("\t","",code_muni)]
dt11[,classe_presenca_Embrapa := 0.5]


dt12 <- openxlsx::read.xlsx("data-raw/aparato_institucional/SistemaS e Sebrae.xlsx"
                            ,sheet = "Sheet1")
setDT(dt12)
names(dt12) <- janitor::make_clean_names(names(dt12))
setnames(dt12,"codigo_da_cidade","code_muni")

dt12 <- dt12[,.SD[1],by = .(code_muni)]
dt12 <- dt12[,.SD,.SDcols = "code_muni"]
dt12[,classe_presenca_SistemaS := 0.5]

# agencias de fomento em todo os brasil
dt13 <- openxlsx::read.xlsx("data-raw/aparato_institucional/Lista de Agências de Fomento e Bancos de Desenvolvimento.xlsx")
setDT(dt13)


dt13 <- copy(df_geral)
dt13 <- dt13[,.SD,.SDcols = c('code_muni')]
dt13[,classe_presenca_agencias_de_fomento := 0.5]
## 14 - empregos / empresas -----

dt14 <- read_rds("data/complexidade/complexidade_muni_prep_data/EMPREGO.rds")
dt14

# classe_rem_RGINT_perc_rem_Estado
# classe_rem_RGINT_perc_rem_Bacia
# classe_rem_RGINT_perc_rem_RTP

rem_Estado <- dt14[,sum(remun_total),by = .(abbrev_state)]
rem_Bacia <- dt14[,sum(remun_total),by = .(bacia)]
rem_RTP <-dt14[,sum(remun_total),by = .(regiao_total)] 
rem_RGINT <- dt14[,sum(remun_total),by = .(code_intermediate)] 

dt14[rem_Estado, on = "abbrev_state",rem_Estado := i.V1]
dt14[rem_Bacia, on = "bacia",rem_Bacia := i.V1]
dt14[rem_RTP, on = "regiao_total",rem_RTP := i.V1]
dt14[rem_RGINT, on = "code_intermediate",rem_RGINT := i.V1]

dt14[ ,p_RGINT_perc_rem_Estado := rem_RGINT / rem_Estado]
dt14[ ,p_RGINT_perc_rem_Bacia :=  rem_RGINT / rem_Bacia]
dt14[ ,p_RGINT_perc_rem_RTP :=    rem_RGINT / rem_RTP]

# RTP
dt14[, classe_RGINT_perc_rem_RTP := {
  mybreaks <- quantile(x = as.numeric(p_RGINT_perc_rem_RTP)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(p_RGINT_perc_rem_RTP)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
},by = regiao_total]

# Bacia
dt14[, classe_RGINT_perc_rem_Bacia := {
  mybreaks <- quantile(x = as.numeric(p_RGINT_perc_rem_Bacia)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(p_RGINT_perc_rem_Bacia)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
},by = bacia]

# Estado
dt14[!is.na(abbrev_state), classe_RGINT_perc_rem_Estado := {
  mybreaks <- as.numeric(quantile(
    p_RGINT_perc_rem_Estado
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  p_RGINT_perc_rem_Estado
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
},by = abbrev_state]

# classe_N_empresas

dt14[, classe_N_empresas := {
  mybreaks <- quantile(x = as.numeric(N_empresas)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(N_empresas)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

emp_Estado <- dt14[,sum(N_empresas),by = .(abbrev_state)]
emp_Bacia  <- dt14[,sum(N_empresas),by = .(bacia)]
emp_RTP    <-dt14[,sum(N_empresas),by = .(regiao_total)] 
emp_RGINT  <- dt14[,sum(N_empresas),by = .(code_intermediate)] 

dt14[emp_Estado, on = "abbrev_state",emp_Estado := i.V1]
dt14[emp_Bacia, on = "bacia",emp_Bacia := i.V1]
dt14[emp_RTP, on = "regiao_total",emp_RTP := i.V1]
dt14[emp_RGINT, on = "code_intermediate",emp_RGINT := i.V1]

dt14[ ,p_RGINT_perc_emp_Estado := emp_RGINT / emp_Estado]
dt14[ ,p_RGINT_perc_emp_Bacia :=  emp_RGINT / emp_Bacia]
dt14[ ,p_RGINT_perc_emp_RTP :=    emp_RGINT / emp_RTP]

# RTP
dt14[, classe_RGINT_perc_emp_RTP := {
  mybreaks <- quantile(x = as.numeric(p_RGINT_perc_emp_RTP)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(p_RGINT_perc_emp_RTP)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
},by = regiao_total]

# Bacia
dt14[, classe_RGINT_perc_emp_Bacia := {
  mybreaks <- quantile(x = as.numeric(p_RGINT_perc_emp_Bacia)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(p_RGINT_perc_emp_Bacia)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
},by = bacia]

# Estado
dt14[!is.na(abbrev_state), classe_RGINT_perc_emp_Estado := {
  mybreaks <- as.numeric(quantile(
    p_RGINT_perc_emp_Estado
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  p_RGINT_perc_emp_Estado
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
},by = abbrev_state]

# rem_capita


dt14[, classe_remun_capita_br := {
  mybreaks <- quantile(x = as.numeric(remun_capita)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(remun_capita)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]


# total_classe_empregos_empresas

dt14[is.na(dt14)] <- 0
dt14[,total_classe_empregos_empresas := 
       as.numeric(classe_remun_capita_br)+
       as.numeric(classe_RGINT_perc_rem_Estado)+
       as.numeric(classe_RGINT_perc_rem_Bacia)+
       as.numeric(classe_RGINT_perc_rem_RTP)+
       as.numeric(classe_N_empresas)+
       as.numeric(classe_RGINT_perc_emp_Estado)+
       as.numeric(classe_RGINT_perc_emp_Bacia)+
       as.numeric(classe_RGINT_perc_emp_RTP)
]

# remove
rm( emp_Estado )
rm( emp_Bacia  )
rm( emp_RTP    )
rm( emp_RGINT  )
rm( rem_Estado )
rm( rem_Bacia  )
rm( rem_RTP    )
rm( rem_RGINT  )

## 15/16 - meio ambiente -----

dt15 <- read_rds("data/complexidade/complexidade_muni_prep_data/DES_NATURAL.rds")
dt15 <- read_rds("data/complexidade/complexidade_muni_prep_data/MEIO_AMBIENTE.rds")

dt15[,tx_an_10_19 := AN_ha2019 / AN_ha2010]
dt15[,tx_nt_10_19 := NT_per2019 / NT_per2010]

dt15[, classe_tx_nt_10_19 := {
  mybreaks <- quantile(x = as.numeric(tx_nt_10_19)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(tx_nt_10_19)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

dt15[, classe_tx_an_10_19 := {
  mybreaks <- quantile(x = as.numeric(tx_an_10_19)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(tx_an_10_19)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]


dt15[,soma_pesos_classes_ma := classe_tx_an_10_19  + classe_tx_nt_10_19]

# desastres naturais

dt16 <- read_rds("data/complexidade/complexidade_muni_prep_data/DES_NATURAL.rds")

dt16[, classe_N_desastres_naturais := {
  mybreaks <- as.numeric(quantile(
    N_EVENTOS
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  N_EVENTOS
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

setnames(dt16,"CD_MUN","code_muni")

## 17 - recursos hidricos ----

dt17 <- read_rds("data/complexidade/complexidade_muni_prep_data/REC_HIDRICO.rds")


## 18 - INFRA_EE -----

dt18 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_EE.rds")
dt18 <- dt18[regiao_total == 1]

# DEC
dt18[, classe_DEC := {
  mybreaks <- quantile(x = as.numeric(DECAPTURADO)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(DECAPTURADO)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# FEC
dt18[, classe_FEC := {
  mybreaks <- quantile(x = as.numeric(FECAPTURADO)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(FECAPTURADO)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# soma
dt18[is.na(dt18)] <- 0

dt18[, soma_pesos_classes_ee := as.numeric(classe_FEC) + as.numeric(classe_DEC)]

## 19 - INFRA_TELE -----

dt19 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_TELE.rds")

# classe_D_TM
dt19[, classe_D_TM := {
  mybreaks <- quantile(x = as.numeric(D_TM)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(D_TM)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_D_BLF
dt19[, classe_D_BLF := {
  mybreaks <- quantile(x = as.numeric(D_BLF)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(D_BLF)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# soma
dt19[is.na(dt19)] <- 0

dt19[, soma_pesos_classes_tele := as.numeric(classe_D_TM) + as.numeric(classe_D_BLF)]


## 20 - INFRA_SAN ----


dt20 <- read_rds("data/complexidade/complexidade_muni_prep_data/SANEAMENTO.rds")
dt20 <- dt20[regiao_total == 1]

names(dt20) <- names(dt20) %>% 
  gsub("SanEsgo","",.) %>% 
  gsub("SanResi","",.) %>% 
  gsub("SanAgua","",.) %>% 
  gsub("SanPluv","",.)

# SanResiCa005 | SanResiCa008 | SanResiUP027 | SanResiUP029
dt20[, classe_SanResiCa005 := fifelse(Ca005 == "Sim",1,0)]
dt20[, classe_SanResiCa008 := fifelse(Ca008 == "Sim",1,0)]
dt20[, classe_SanResiUP027 := fifelse(UP027 == "Sim",1,0)]
dt20[, classe_SanResiUP029 := fifelse(UP029 == "Sim",1,0)]

# classe_IN022
dt20[, classe_IN022 := {
  mybreaks <- quantile(x = as.numeric(IN022)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(IN022)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_IN049
dt20[, classe_IN049 := {
  mybreaks <- quantile(x = as.numeric(IN049)
                       , probs=seq(0, 1, by=0.2)
                       , na.rm=T)
  mybreaks <- as.numeric(mybreaks)
  
  cut(as.numeric(IN049)
      , breaks = mybreaks
      , include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75))
  
}]

# classe_N046
dt20[, classe_IN046 := {
  mybreaks <- as.numeric(quantile(
    IN046
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  IN046
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]


# classe_IN014
dt20[, classe_SanResiIN014 := {
  mybreaks <- as.numeric(quantile(
    IN014
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  IN014
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

## 20.a - INFRA_SAN (2) -----

dt21 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_SAN.rds")
dt21 <- dt21[regiao_total == 1]


# classe_IN023
dt21[, classe_IN023 := {
  mybreaks <- as.numeric(quantile(
    IN023
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  IN023
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]
# classe_IN024
dt21[, classe_IN024 := {
  mybreaks <- as.numeric(quantile(
    IN024
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  IN024
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]

# merge SAN
dt21_san <-merge.data.table(
  x = dt20[,.SD,.SDcols = c(
    'code_muni'
    ,'classe_IN022'
    ,'classe_IN049'
    ,'classe_IN046'
    ,'classe_SanResiCa005'
    ,'classe_SanResiCa008'
    ,'classe_SanResiIN014'
    ,'classe_SanResiUP027'
    ,'classe_SanResiUP029'
  )]
  ,y = dt21[,.SD,.SDcols = c(
    'code_muni'
    ,'classe_IN023'
    ,'classe_IN024'
  )]  ,by = c("code_muni")  ,all = TRUE)

dt21_san[is.na(dt21_san)] <- 0
dt21_san[,soma_pesos_classes_san_res := 
           as.numeric(classe_IN022) +
           as.numeric(classe_IN049) +
           as.numeric(classe_IN023) +
           as.numeric(classe_IN024) +
           as.numeric(classe_IN046) +
           as.numeric(classe_SanResiCa005) +
           as.numeric(classe_SanResiCa008) +
           as.numeric(classe_SanResiIN014) +
           as.numeric(classe_SanResiUP027) +
           as.numeric(classe_SanResiUP029)]

rm(dt20)
rm(dt21)
dt20 <- dt21_san
rm(dt21_san)

# df_geral ----

# as.character
df_geral[,code_muni := as.character(code_muni)]
dt1[,code_muni := as.character(code_muni)]
dt2[,code_muni := as.character(code_muni)]
dt3[,code_muni := as.character(code_muni)]
dt4[,code_muni := as.character(code_muni)]
dt5[,code_muni := as.character(code_muni)]
dt6[,code_muni := as.character(code_muni)]
dt7[,code_muni := as.character(code_muni)]
dt8[,code_muni := as.character(code_muni)]
dt9[,code_muni := as.character(code_muni)]
dt10[,code_muni := as.character(code_muni)]
dt11[,code_muni := as.character(code_muni)]
dt12[,code_muni := as.character(code_muni)]
dt13[,code_muni := as.character(code_muni)]
dt14[,code_muni := as.character(code_muni)]
dt15[,code_muni := as.character(code_muni)]
dt16[,code_muni := as.character(code_muni)]
dt17[,code_muni := as.character(code_muni)]
dt18[,code_muni := as.character(code_muni)]
dt19[,code_muni := as.character(code_muni)]
dt20[,code_muni := as.character(code_muni)]
break()
df <- df_geral[,code_muni := as.character(code_muni)] %>%
  # POLITICA RURAL
  merge.data.table(x = .
                   ,y = dt1[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_num_estab_agr_familiar'
                     , 'classe_area_irrig_orient_tec'
                     , 'classe_num_estab_orient_total'
                     , 'classe_num_estab_orient_outrasNAOgov'
                     , 'classe_orint_tec_cooperativas'
                     , 'classe_orint_tec_empresas'
                     , 'classe_orint_tec_gov'
                     , 'classe_orint_tec_ong'
                     , 'classe_orint_tec_proprio_produtor'
                     , 'classe_orint_tec_sistemaS'
                     , 'soma_classes_pol_rurais')]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # PIB
  merge.data.table(x = .
                   ,y = dt2[,.SD,.SDcols = c(
                     'code_muni'
                     , 'classe_area_vulneraveis'
                     , 'classe_area_destaque'
                     , 'peso_VAB_COMBINACAO'
                     , 'classe_ICEM_Ativ'
                     , 'classe_ICEM_Prod_alto'
                     , 'peso_VAB_Adm_pp'
                     , 'pont_total_pib'
                     , 'classe_VAB_Agro_pp'
                     , 'peso_VAB_COMB_Agro_Serv_altos'
                     , 'classe_VAB_Ind_pp'
                     , 'peso_VAB_COMB_Ind_Serv_altos'
                     , 'peso_VAB_COMB_Agro_Ind_altos'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # MEIO_AMBIENTE | DESASTRES NATURAIS
  merge.data.table(x = .
                   ,y = dt16[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_N_desastres_naturais'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # MEIO_AMBIENTE | REC HIDRCO
  merge.data.table(x = .
                   ,y = dt17[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_comprom_rh'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # POLIT_URBANA
  merge.data.table(x = .
                   ,y = dt10[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_controle_urb'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # APARATO INSTITUCIONAL | EMBRAPA
  merge.data.table(x = .
                   ,y = dt11
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # APARATO INSTITUCIONAL | SistemaS
  merge.data.table(x = .
                   ,y = dt12
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # APARATO INSTITUCIONAL | Agencia Fomento
  merge.data.table(x = .
                   ,y = dt13
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # EMPREGO E EMPRESAS
  merge.data.table(x = .
                   ,y = dt14[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_remun_capita_br'
                     ,'classe_RGINT_perc_rem_RTP' 
                     ,'classe_RGINT_perc_rem_Bacia' 
                     ,'classe_RGINT_perc_rem_Estado' 
                     ,'classe_N_empresas'
                     ,'classe_RGINT_perc_emp_RTP' 
                     ,'classe_RGINT_perc_emp_Bacia' 
                     ,'classe_RGINT_perc_emp_Estado'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # BALANCA COMERCIAL 
  merge.data.table(x = .
                   ,y = dt5[,.SD,.SDcols = c(
                     "code_muni"
                     ,"classe_exportacao"
                     ,"classe_exp_RGINT_perc_exp_Estado"
                     ,"classe_exp_RGINT_perc_exp_Bacia"
                     ,"classe_exp_RGINT_perc_exp_RTP"
                     ,"total_classe_comex"
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # COMPLEXIDADE ECONOMICA
  merge.data.table(x = .
                   ,y = dt9
                   ,by = c("code_muni")
                   ,all = TRUE) %>% 
  # EDUCACAO
  merge.data.table(x = .
                   ,y = dt7[,.SD,.SDcols = c(
                     "code_muni"
                     ,"classe_inv_educ_infantil"
                     ,"classe_inv_educ_basica"
                     ,"classe_inv_ensino_funmental"
                     ,"classe_inv_ensino_medio"
                     ,"classe_inv_educ_profissional"
                     ,"soma_pesos_educacao"
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE)  %>% 
  # INFRA_EE
  merge.data.table(x = .
                   ,y = dt18[,.SD,.SDcols = c(
                     "code_muni"
                     ,"classe_DEC"
                     ,"classe_FEC"
                     ,"soma_pesos_classes_ee"
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE)  %>% 
  # INFRA_TELE
  merge.data.table(x = .
                   ,y = dt19[,.SD,.SDcols = c(
                     "code_muni"
                     ,"classe_D_BLF"
                     ,"classe_D_TM"
                     ,"soma_pesos_classes_tele"
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE)   %>% 
  # INFRA_SAN 1 | 2
  merge.data.table(x = .
                   ,y = dt20
                   ,by = c("code_muni")
                   ,all = TRUE)  %>% 
  # AGRO
  merge.data.table(x = .
                   ,y = dt3[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_muni_valor_da_producao'
                     ,'classe_agro_valor_da_producao_RGINT_perc_Estado'
                     ,'classe_agro_valor_da_producao_RGINT_perc_Bacia'
                     ,'classe_agro_valor_da_producao_RGINT_perc_RTP'
                     ,'soma_pesos_classes_agricultura'
                     ,'classe_muni_valor_silv'
                     ,'classe_muni_valor_silv_RGINT_perc_Estado'
                     ,'classe_muni_valor_silv_RGINT_perc_Bacia'
                     ,'classe_muni_valor_silv_RGINT_perc_RTP'
                     ,'soma_pesos_classes_silvicultura'
                     ,'classe_muni_valor_aqui'
                     ,'classe_muni_valor_aqui_RGINT_perc_Estado'
                     ,'classe_muni_valor_aqui_RGINT_perc_Bacia'
                     ,'classe_muni_valor_aqui_RGINT_perc_RTP'
                     ,'soma_pesos_classes_aquicultura'
                     ,'classe_muni_valor_rebanho'
                     ,'classe_muni_valor_reb_RGINT_perc_Estado'
                     ,'classe_muni_valor_reb_RGINT_perc_Bacia'
                     ,'classe_muni_valor_reb_RGINT_perc_RTP'
                     ,'soma_pesos_classes_rebanhos'
                   )]
                   ,by = c("code_muni")
                   ,all = TRUE)  %>% 
  # FNE
  merge.data.table(x = .
                   ,y = dt8[,.SD,.SDcols = c(
                     'code_muni'
                     ,'classe_inv_rural'
                     ,'classe_inv_urbana'
                     ,'classe_num_contratos'
                     ,'classe_investimento'
                     ,'classe_inv_pj'
                     ,'classe_inv_comercio'
                     ,'classe_inv_turismo'
                     ,'soma_pesos_fne'
                     ,'classe_inv_agricola'
                     ,'classe_inv_agroindustria'
                     ,'classe_inv_pecuaria'
                     ,'soma_classes_fne_rural'
                     )]
                   ,by = c("code_muni")
                   ,all = TRUE)

# save----
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "Comp.MUNICIPIO") 
# End ----