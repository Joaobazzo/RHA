# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(readr)
library(googlesheets4)

df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral <- df_geral[!is.na(code_muni) ,]
df_geral[mun_pisf == 1,bacia := "PISF"]
df_geral[mun_bsf == 1,bacia := "BSF"]
df_geral[mun_bpar == 1,bacia := "BPAR"]
#df_geral[,regiao_total := NULL]
#df_geral[,regiao_total := "RTP"]

colToRem <- c("name_muni", "code_state","name_state"
              ,"name_region","code_rgi","name_rgi" 
              ,"name_intermediate","mun_sudene","mun_pisf","mun_bsf"
              ,"mun_bpar", "semi_arido")
colToRem <- setdiff(names(df_geral),c("code_muni","regiao_total"
                                      ,"abbrev_state","bacia","code_intermediate"))

link_gdocs <- "https://docs.google.com/spreadsheets/d/1WbmvP0qgg6iHnu2o0H-4k0dXQ9wUX10HeBKIht7Nl5M/edit?usp=sharing"

is_regiao_total <- FALSE
if(is_regiao_total) df_geral <- df_geral[regiao_total == 1,]

# Read ----
rm(list = ls()[!(ls() %in% c("colToRem","link_gdocs","df_geral","is_regiao_total"))])
gc(reset = TRUE)

## 1 - polit rural ----
dt1 <- read_rds("data/complexidade/complexidade_muni_prep_data/POLIT_RURAL.rds")
dt1 <- dt1[,(colToRem) := NULL]
if(is_regiao_total) dt1 <- dt1[regiao_total == 1,]
dt1[,c("abbrev_state","code_intermediate", "regiao_total", "bacia") := NULL]


## 2 - pib -----
dt2 <- read_rds("data/complexidade/complexidade_muni_prep_data/pib.rds")
if(is_regiao_total) dt2 <- dt2[regiao_total == 1,]
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

#setnames(dt2,"peso_ICEM_Prod","classe_ICEM_Prod_alto")

dt2[,classe_VAB_Agro_pp := fcase(
  classe_VAB_Agro_pp == "Alta",0.75,
  classe_VAB_Agro_pp == "Média",0.5,
  classe_VAB_Agro_pp == "Baixa",0.25
)]
dt2[,classe_VAB_Ind_pp := fcase(
  classe_VAB_Ind_pp == "Alta",0.75,
  classe_VAB_Ind_pp == "Média",0.5,
  classe_VAB_Ind_pp == "Baixa",0.25
)]
dt2[,classe_ICEM_Prod_alto := fifelse(peso_ICEM_Prod == 0.75,0.5,0)]
dt2[,pont_total_pib := classe_area_vulneraveis +
      classe_area_destaque + 
      classe_ICEM_Ativ]

## 3 - agro ----

dt3 <- read_rds("data/complexidade/complexidade_muni_prep_data/AGRO.rds")
if(is_regiao_total) dt3 <- dt3[regiao_total == 1,]
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
valor_aqui_Bacia  <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(bacia)]
valor_aqui_RTP    <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(regiao_total)] 
valor_aqui_RGINT  <- dt3[,sum(muni_valor_aqui,na.rm = TRUE),by = .(code_intermediate)] 

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
    , classe_muni_valor_rebanho_RGINT_perc_Estado := {
      
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
    , classe_muni_valor_rebanho_RGINT_perc_Bacia := {
      
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
    , classe_muni_valor_rebanho_RGINT_perc_RTP := {
      
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
      as.numeric(classe_muni_valor_rebanho_RGINT_perc_Estado) +
      as.numeric(classe_muni_valor_rebanho_RGINT_perc_Bacia) +
      as.numeric(classe_muni_valor_rebanho_RGINT_perc_RTP)]

rm(valor_aqui_Estado); rm(valor_agro_Estado)
rm(valor_aqui_Bacia ); rm(valor_agro_Bacia )
rm(valor_aqui_RTP   ); rm(valor_agro_RTP   )
rm(valor_aqui_RGINT ); rm(valor_agro_RGINT )
rm(valor_reb_Estado) ; rm(valor_silv_Estado) 
rm(valor_reb_Bacia ) ; rm(valor_silv_Bacia ) 
rm(valor_reb_RTP   ) ; rm(valor_silv_RTP   ) 
rm(valor_reb_RGINT ) ; rm(valor_silv_RGINT ) 

## 4 - populacao ----

dt4 <- read_rds("data/complexidade/complexidade_muni_prep_data/POPULACAO.rds")
if(is_regiao_total) dt4 <- dt4[code_muni %in% df_geral$code_muni,]
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
if(is_regiao_total) dt5 <- dt5[code_muni %in% df_geral$code_muni,]
dt5[,(colToRem) := NULL]
dt5[,regiao_total := NULL]
dt5 <- dt5[!is.na(total_filtro_7)]
dt5[!is.na(bacia),regiao_total := "regiao_total"]


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
if(is_regiao_total) dt6 <- dt6[regiao_total == 1,]
dt6 <- dt6[,(colToRem) := NULL]
dt6 <- dt6

dt6[1]
dt6[,c("abbrev_state", "code_intermediate", "regiao_total", "bacia") := NULL]

## 7 - educacao -----

dt7 <- read_rds("data/complexidade/complexidade_muni_prep_data/EDUCACAO.rds")
if(is_regiao_total) dt7 <- dt7[regiao_total == 1,]
dt7[,(colToRem) := NULL]

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
      sum(as.numeric(classe_inv_educ_infantil) 
          , as.numeric(classe_inv_educ_basica) 
          , as.numeric(classe_inv_ensino_funmental) 
          , as.numeric(classe_inv_ensino_medio) 
          , as.numeric(classe_inv_educ_profissional)
          ,na.rm = TRUE)
    , by = .(code_muni)
]


## 8 - fne -----

dt8 <- read_rds("data/complexidade/complexidade_muni_prep_data/FNE.rds")
if(is_regiao_total) dt8 <- dt8[regiao_total == 1,]
dt8[,(colToRem) := NULL]

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
      sum(as.numeric(classe_inv_agricola)
          ,as.numeric(classe_inv_agroindustria)
          ,as.numeric(classe_inv_pecuaria)
          ,na.rm = TRUE)
    ,by = .(code_muni)]
dt8[,(colToRem) := NULL]

## 9 - complexidade_economica -----

dt9 <-  data.table::fread("data/complexidade/ListaProdutosDiv_Municipio.csv")
if(is_regiao_total){
  dt9 <- dt9[Municipio %in% 
               df_geral[regiao_total == 1,code_muni],]
}
dt9 <- dt9[,.N,by = .(Municipio)]
dt9[,classe_prod_base_sim := 0.5]
dt9[,classe_ativ_base_sim := 0.5]
setnames(dt9,"Municipio","code_muni")

dt9 <- dt9[,.SD,.SDcols = c("code_muni","classe_ativ_base_sim","classe_prod_base_sim")]

dt9

dt9[,ativ_div := classe_prod_base_sim]
dt9[,prod_base := classe_prod_base_sim]
dt9[,prod_div := classe_prod_base_sim]

dt9[,pont_total_alta_complexidade := sum(
  classe_prod_base_sim
  ,prod_base
  ,prod_div
  ,na.rm = TRUE
),code_muni]


dt9

## 9a - complexidade_economica  kr0-----


##  10 - politica urbana -----

# controle urbano
dt10b <- read_rds("data/complexidade/complexidade_muni_prep_data/URBANISMO.rds")
if(is_regiao_total) dt10b <- dt10b[regiao_total == 1,]
dt10b[,(colToRem) := NULL]


dt10b[,c("abbrev_state", "code_intermediate", "regiao_total") := NULL]


dt10b[, classe_controle_urb := {
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

dt10b[,total_con_urb := NULL]


# nivel integracao

dt10a <- openxlsx::read.xlsx("data/dados_maira/Variavel de Integração Regional.xlsx") 
setDT(dt10a)
dt10a <- dt10a %>% 
  setnames(.,"Cod_Ibge","code_muni") %>% 
  .[,.SD,.SDcols = c("code_muni","NotaFinal")] %>% 
  .[,NotaFinal := as.numeric(NotaFinal)]

dt10a[!is.na(NotaFinal), classe_nivel_integr := {
  mybreaks <- as.numeric(quantile(
    NotaFinal
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  NotaFinal
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]
dt10a[,NotaFinal := NULL]
dt10a[,code_muni := as.character(code_muni)]

# merge
dt10 <- data.table::merge.data.table(x = dt10a
                                     ,y = dt10b
                                     ,by = "code_muni")
dt10[,soma_pesos_classes_pol_urb := sum(
  as.numeric(classe_nivel_integr)
  ,as.numeric(classe_controle_urb)
  ,na.rm = TRUE
),by = .(code_muni)]

rm(dt10a);rm(dt10b)

## 11/12/13/13a - aparato institucional ----


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
dt12 <- dt12[,.SD,.SDcols = c("code_muni","entidade")]
dt12[,classe_presenca_SistemaS := 0.5]
dt12[,classe_presenca_SEBRAE := fifelse(entidade == "SEBRAE",0.5,0)]

# agencias de fomento em todo os brasil
dt13 <- openxlsx::read.xlsx("data-raw/aparato_institucional/Lista de Agências de Fomento e Bancos de Desenvolvimento.xlsx")
setDT(dt13)


dt13 <- copy(df_geral)
dt13 <- dt13[,.SD,.SDcols = c('code_muni')]
dt13[,classe_presenca_agencias_de_fomento := 0.5]

# codevasp
st1 <- openxlsx::read.xlsx("data/dados_maira/map_0203_mun_01sr_cdv_lei_14053_06_2021.xlsx") %>% 
  setDT()
st2 <- openxlsx::read.xlsx("data/dados_maira/map_0211_mun_09sr_cdv_lei_14053_10_2021.xlsx") %>% 
  setDT()
st3 <- openxlsx::read.xlsx("data/dados_maira/map_0203_mun_01sr_cdv_lei_14053_06_2021.xlsx") %>% 
  setDT()
st_code <- unique(c(st1$Geocódigo,st2$Geocódigo,st3$Geocódigo))
rm(st1);rm(st2);rm(st3)
dt13a <- copy(df_geral)
dt13a <- dt13a[name_region == "Nordeste",classe_presenca_codevasf := 1]
dt13a <- dt13a[,.SD,.SDcols = c('code_muni','classe_presenca_codevasf')]
dt13a[code_muni %in% st_code,classe_presenca_codevasf := 1]
dt13a[is.na(classe_presenca_codevasf), classe_presenca_codevasf := 0]

## 14 - empregos / empresas -----

dt14 <- read_rds("data/complexidade/complexidade_muni_prep_data/EMPREGO.rds")
if(is_regiao_total) dt14 <- dt14[regiao_total == 1,]
dt14[,(colToRem) := NULL]
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
dt14[, classe_rem_RGINT_perc_rem_RTP := {
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
dt14[, classe_rem_RGINT_perc_rem_Bacia := {
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
dt14[!is.na(abbrev_state), classe_rem_RGINT_perc_rem_Estado := {
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
dt14[, classe_empresas_RGINT_perc_empresas_RTP := {
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
dt14[, classe_empresas_RGINT_perc_empresas_Bacia := {
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
dt14[!is.na(abbrev_state), classe_empresas_RGINT_perc_empresas_Estado := {
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
dt14[,total_classe_empregos_empresas := sum(
  as.numeric(classe_remun_capita_br),
  as.numeric(classe_rem_RGINT_perc_rem_Estado),
  as.numeric(classe_rem_RGINT_perc_rem_Bacia),
  as.numeric(classe_rem_RGINT_perc_rem_RTP),
  as.numeric(classe_N_empresas),
  as.numeric(classe_empresas_RGINT_perc_empresas_Estado),
  as.numeric(classe_empresas_RGINT_perc_empresas_Bacia),
  as.numeric(classe_empresas_RGINT_perc_empresas_RTP)
  ,na.rm = TRUE)
  ,by = .(code_muni)]

# remove
rm( emp_Estado )
rm( emp_Bacia  )
rm( emp_RTP    )
rm( emp_RGINT  )
rm( rem_Estado )
rm( rem_Bacia  )
rm( rem_RTP    )
rm( rem_RGINT  )
dt14[,(c("abbrev_state", "code_intermediate", "regiao_total","bacia")) := NULL]
## 15 - meio ambiente -----
dt15a <- openxlsx::read.xlsx("data/dados_maira/unidades conservacao.xlsx")
setDT(dt15a)
names(dt15a) <- janitor::make_clean_names(names(dt15a))
dt15a[,tx_uc_21_10 := (uc_2021_ha - uc_2010_ha)/ uc_2010_ha]
# baixo
dt15a[tx_uc_21_10 == 0,classe_area_uc_tx_cresc_2010_2021 := 0]
# zero
dt15a[uc_2010_ha == 0 & uc_2021_ha == 0
      ,classe_area_uc_tx_cresc_2010_2021 := 0]
# alto
dt15a[uc_2010_ha == 0 & uc_2021_ha > uc_2010_ha
      ,classe_area_uc_tx_cresc_2010_2021 := 0]
# medio
dt15a[is.na(classe_area_uc_tx_cresc_2010_2021),classe_area_uc_tx_cresc_2010_2021 := 
        fcase(
          tx_uc_21_10 > 50,0.75,
          tx_uc_21_10 > 10,0.5,
          tx_uc_21_10 > 0,0.25
        )]

# prep
dt15a <- dt15a %>% 
  setnames(.,"codigo_municipio_completo","code_muni") %>% 
  .[,.SD,.SDcols = c("code_muni","classe_area_uc_tx_cresc_2010_2021")] %>% 
  .[,code_muni := as.character(code_muni)]

# uso natural
dt15 <- read_rds("data/complexidade/complexidade_muni_prep_data/MEIO_AMBIENTE.rds")
if(is_regiao_total) dt15 <- dt15[regiao_total == 1,]

dt15[,tx_an_10_19 := AN_ha2019 / AN_ha2010] %>% 
  .[,tx_nt_10_19 := NT_per2019 / NT_per2010] %>% 
  .[, classe_tx_nt_10_19 := {
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

dt15 <- dt15[dt15a, on = "code_muni"]


dt15[,soma_pesos_classes_ma := sum(
  as.numeric(classe_tx_nt_10_19) 
  ,as.numeric(classe_area_uc_tx_cresc_2010_2021)
  ,na.rm = TRUE)
  ,by = .(code_muni)]

dt15 <- dt15[,.SD
             ,.SDcols = c("code_muni","classe_tx_nt_10_19"
                          ,"classe_area_uc_tx_cresc_2010_2021","soma_pesos_classes_ma")]


## 16 - desastres naturais ------------

dt16 <- read_rds("data/complexidade/complexidade_muni_prep_data/DES_NATURAL.rds")

dt16[, classe_N_desastres_naturais := fcase(
  N_EVENTOS <= 2,-3,
  N_EVENTOS <= 4,-5,
  N_EVENTOS <= 6,-7)]

setnames(dt16,"CD_MUN","code_muni")
dt16[,N_EVENTOS := NULL]

## 17 - recursos hidricos ----

dt17 <- read_rds("data/complexidade/complexidade_muni_prep_data/REC_HIDRICO.rds")
if(is_regiao_total) dt17 <- dt17[regiao_total == 1,]
dt17 <- dt17[,.SD,.SDcols = c("code_muni","classe_comprom_rh")]

temp_man <- openxlsx::read.xlsx("data/dados_maira/AtlasAguas_vulnerabilidadeMananciais.xlsx")
setDT(temp_man)
names(temp_man) <- janitor::make_clean_names(names(temp_man))
temp_man$classificacao_manancial %>% unique()

temp_man <- temp_man[,.SD,.SDcols = c("codigo_ibge","classificacao_manancial")] %>% 
  setnames(.,'codigo_ibge','code_muni') %>% 
  .[,code_muni := as.character(code_muni)] %>% 
  .[,classe_vuln_manancial_abast_pub := fcase(
    classificacao_manancial %like% "Não",0,
    classificacao_manancial %like% "Baixa",0,
    classificacao_manancial %like% "Média",-1,
    classificacao_manancial %like% "Alta",-2
  )] %>% 
  .[,classificacao_manancial := NULL]
temp_man[1]

dt17 <- copy(df_geral)[,code_muni := as.character(code_muni)] %>%
  .[,.SD,.SDcols = "code_muni"] %>% 
  merge.data.table(x = .,y = dt17,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = temp_man,by = "code_muni",all = TRUE) 

dt17[is.na(dt17)] <- 0
## 18 - INFRA_EE -----

dt18 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_EE.rds")
if(is_regiao_total) dt18 <- dt18[regiao_total == 1,]
dt18[,(colToRem) := NULL]
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

dt18[,(c("abbrev_state", "code_intermediate", "regiao_total")) := NULL]
## 19 - INFRA_TELE -----

dt19 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_TELE.rds")
if(is_regiao_total) dt19 <- dt19[code_muni %in% df_geral$code_muni,]

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
dt19[,(colToRem) := NULL]

## 20 - INFRA_SAN ----

# variavel de recuperacao de materia organica

dt20_var31 <- fread("data/dados_maira/MunSanResi.csv",dec = ",")
dt20_var31 <- dt20_var31[!is.na(SanResiIN031)] %>% 
  setnames(.,"Cód","code_muni") %>% 
  .[,.SD,.SDcols = c("code_muni","SanResiIN031")] %>% 
  .[!is.na(SanResiIN031), classe_SanResiIN031 := {
    mybreaks <- quantile(x = as.numeric(SanResiIN031)
                         , probs=seq(0, 1, by=0.2)
                         , na.rm=T)
    mybreaks <- as.numeric(mybreaks)
    
    cut(as.numeric(SanResiIN031)
        , breaks = mybreaks
        , include.lowest= TRUE
        , labels= c(0.25,0.25,0.5,0.75,0.75))
    
  }]
dt20_var31[,SanResiIN031 := NULL]


dt20 <- read_rds("data/complexidade/complexidade_muni_prep_data/SANEAMENTO.rds")
if(is_regiao_total) dt20 <- dt20[regiao_total == 1,]

names(dt20) <- names(dt20) %>% 
  gsub("SanEsgo","",.) %>% 
  gsub("SanResi","",.) %>% 
  gsub("SanAgua","",.) %>% 
  gsub("SanPluv","",.)

# SanResiCa005 SanResiCa008 SanResiUP027 SanResiUP029
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

dt20[,(colToRem) := NULL]
## 20.a - INFRA_SAN (2) -----

dt21 <- read_rds("data/complexidade/complexidade_muni_prep_data/INFRA_SAN.rds")
if(is_regiao_total) dt21 <- dt21[regiao_total == 1,]


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
  )]  ,by = "code_muni"  ,all = TRUE)

dt21_san[is.na(dt21_san)] <- 0
dt21_san <- dt21_san[dt20_var31,on = "code_muni"]
dt21_san[,soma_pesos_classes_san_res := 
           sum(as.numeric(classe_IN022) 
               ,as.numeric(classe_IN049) 
               ,as.numeric(classe_IN023) 
               ,as.numeric(classe_IN024) 
               ,as.numeric(classe_IN046) 
               ,as.numeric(classe_SanResiCa005) 
               ,as.numeric(classe_SanResiCa008) 
               ,as.numeric(classe_SanResiIN014) 
               ,as.numeric(classe_SanResiIN031) 
               ,as.numeric(classe_SanResiUP027) 
               ,as.numeric(classe_SanResiUP029)
               ,na.rm = TRUE),by = .(code_muni)]

rm(dt20_var31);rm(dt20);rm(dt21)
dt20 <- dt21_san
rm(dt21_san)

## 21 APL / ROTAS  ----
dt21 <- openxlsx::read.xlsx("data-raw/APLs/APL por municipio - atualizado.xlsx")
setDT(dt21)
dt21 <- copy(df_geral) %>% 
  .[,.SD,.SDcols = 'code_muni'] %>% 
  .[code_muni %in% unique(dt21$Código.Município),classe_presenca_apl := 1] %>% 
  .[is.na(classe_presenca_apl),classe_presenca_apl := 0]
dt21[1]

tmp <- openxlsx::read.xlsx("data-raw/APLs/Rotas e Listagem de municípios Completa_ROTAS OK!.xlsx")
setDT(tmp)

tmp <- copy(df_geral) %>% 
  .[,.SD,.SDcols = 'code_muni'] %>% 
  .[code_muni %in% unique(tmp$Código.Município.Completo),classe_presenca_rotas := 1] %>% 
  .[is.na(classe_presenca_rotas),classe_presenca_rotas := 0]

dt21 <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                         ,y = dt21,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = tmp,by = "code_muni",all = TRUE)

dt21[,soma_classes_apls_e_rotas_sim := sum(
  classe_presenca_apl
  ,classe_presenca_rotas
  ,na.rm = TRUE
),by = code_muni]

rm(tmp)
## 23 CTI ------

# classe VAB serv alto 
dt23a <- read_rds("data/complexidade/complexidade_muni_prep_data/pib.rds")
if(is_regiao_total) dt23a <- dt23a[regiao_total == 1,]
dt23a <- dt23a[,(colToRem) := NULL]
dt23a <- dt23a[,.SD,.SDcols = c("code_muni","classe_VAB_Serv_pp")]
dt23a <- dt23a[,classe_vab_serv_alto := fifelse(classe_VAB_Serv_pp == "Alta",1,0)]
dt23a[,classe_VAB_Serv_pp := NULL]

# graduacao
dt23b <- readxl::read_xls("data/dados_maira/CTI_Educacao/CTIGrad_mun.xls")
setDT(dt23b)
names(dt23b) <- janitor::make_clean_names(names(dt23b))
setnames(dt23b,'codigo_municipio_completo','code_muni')
dt23b <- dt23b[,.SD,.SDcols = c("code_muni","cti_grad")] %>% 
  .[cti_grad > 0 ,classe_presenca_ies := 1] %>% 
  .[cti_grad == 0 ,classe_presenca_ies := 0] %>% 
  .[,cti_grad := NULL] %>% 
  .[,classe_presenca_curso_graduacao := fifelse(classe_presenca_ies == 1,1,0)]

# pos graduacao
dt23c <- readxl::read_xls("data/dados_maira/CTI_Educacao/CTIPGrad_mun.xls")
setDT(dt23c)
names(dt23c) <- janitor::make_clean_names(names(dt23c))
setnames(dt23c,'codigo_municipio_completo','code_muni')
dt23c <- dt23c[,.SD,.SDcols = c("code_muni","ctip_grad")] %>% 
  .[ctip_grad > 0 ,classe_presenca_ies_pos_grad := 1] %>% 
  .[ctip_grad == 0 ,classe_presenca_ies_pos_grad := 0] %>% 
  .[,ctip_grad := NULL] %>% 
  .[,classe_presenca_artigo_publicados := fifelse(classe_presenca_ies_pos_grad == 1,1,0)]

# patente
dt23d <- readxl::read_xls("data/dados_maira/CTI_Educacao/CTIRegPS_mun.xls")
setDT(dt23d)
names(dt23d) <- janitor::make_clean_names(names(dt23d))
setnames(dt23d,'codigo_municipio_completo','code_muni')
dt23d <- dt23d[,.SD,.SDcols = c("code_muni","cti_soft")] %>% 
  .[cti_soft > 0 ,classe_presenca_patente := 1] %>% 
  .[cti_soft == 0 ,classe_presenca_patente := 0] %>% 
  .[,cti_soft := NULL]

# merge
dt23 <- dt23a[dt23b,on = "code_muni"] %>% 
  .[dt23c,on = "code_muni"] %>% 
  .[dt23d,on = "code_muni"] 

rm(dt23a);rm(dt23b);rm(dt23c);rm(dt23d)

dt23[,soma_pesos_cti_sim := 
       sum(
         classe_vab_serv_alto
         ,classe_presenca_ies
         ,classe_presenca_curso_graduacao
         ,classe_presenca_ies_pos_grad
         ,classe_presenca_artigo_publicados
         ,classe_presenca_patente
         ,na.rm = TRUE
       ),by = code_muni]

if(is_regiao_total) dt23 <- dt23[code_muni %in% df_geral$code_muni,]
dt23[,(colToRem) := NULL]
## 24 licenciamento ambiental -----------------
dt24 <- openxlsx::read.xlsx("data/dados_maira/LicAmbiental.xlsx"
                            ,sheet = "Planilha2")
setDT(dt24)
setnames(dt24,"CD_MUN","code_muni")
names_leg <- unique(dt24$LEGENDA)
names_leg
dt24[,classe_tempo_licenciamento := fcase(
  LEGENDA == names_leg[1],1,
  LEGENDA == names_leg[2],0.0,
  LEGENDA == names_leg[3],0.75,
  LEGENDA == names_leg[4],0.50,
  LEGENDA == names_leg[5],0.25
)]
dt24[,MÉDIA_MES := NULL]
dt24[,LEGENDA := NULL]

if(is_regiao_total) dt24 <- dt24[code_muni %in% df_geral$code_muni,]
dt24[,classe_presenca_de_licenciamento_ambiental := fifelse(
  classe_tempo_licenciamento == 0,0,1)]

## 25 logistica ------

dt25 <- fread("data/complexidade/ListaProdutosDiv_Municipio.csv")

dt25[, classe_icp_muni := {
  mybreaks <- as.numeric(quantile(
    ICPBase
    ,probs =  0:3/3
    , na.rm = TRUE))
  
  if(uniqueN(mybreaks)<4){
    mybreaks <- mybreaks + seq(0.001,0.005,length.out = 4)
  }
  
  decile <- cut(x =  ICPBase
                , breaks =  mybreaks
                , include.lowest = TRUE
                , labels= c(0.25,0.5,0.75))
  decile <- as.character(decile)
  list("decile" = decile)
  
}]
dt25_rgint <- dt25[,.SD[1],by = .(RegiaoIntermediaria,classe_icp_muni)] %>% 
  .[,.SD,.SDcols = c("RegiaoIntermediaria","classe_icp_muni")]%>% 
  .[,max(classe_icp_muni),by = RegiaoIntermediaria] %>% 
  .[,classe_qualificacao_subjetiva_RGINT := 
      fifelse(V1 == 0.75,1,0)] %>% 
  .[,V1 := NULL] %>% 
  setnames(.,"RegiaoIntermediaria","code_intermediate")

dt25_uf <- dt25[,.SD[1],by = .(UF,classe_icp_muni)]%>% 
  .[,.SD,.SDcols = c("UF","classe_icp_muni")] %>% 
  .[,max(classe_icp_muni),by = UF] %>% 
  .[,classe_qualificacao_subjetiva_UF := 
      fifelse(V1 == 0.75,1,0)] %>% 
  .[,V1 := NULL] %>% 
  setnames(.,"UF","code_state")

dt25_rtp <- dt25[,.SD[1],by = .(Municipio,classe_icp_muni)]%>% 
  .[,Municipio := as.character(Municipio)] %>% 
  .[df_geral,on = c("Municipio" = "code_muni"),regiao_total := i.regiao_total] %>% 
  .[,.SD,.SDcols = c("regiao_total","classe_icp_muni")] %>% 
  .[,max(classe_icp_muni),by = regiao_total] %>% 
  .[,classe_qualificacao_subjetiva_regiao_total := 
      fifelse(V1 == 0.75,1,0)] %>% 
  .[,V1 := NULL]


df_geral[,code_intermediate := as.character(code_intermediate)]
dt25_rgint[,code_intermediate := as.character(code_intermediate)]


dt25 <- merge.data.table(x = df_geral[,.SD,.SDcols = c("code_muni"
                                                       ,"code_intermediate"
                                                       ,"code_state"
                                                       ,"regiao_total")]
                         ,y = dt25_rgint,by = "code_intermediate",all = TRUE) %>% 
  merge.data.table(x = .,y = dt25_uf,by = "code_state",all = TRUE) %>% 
  merge.data.table(x = .,y = dt25_rtp,by = "regiao_total",all = TRUE)

rm(dt25_rgint);rm(dt25_uf);rm(dt25_rtp)

dt25 <- dt25[,.SD,.SDcols = c("code_muni"
                              ,"classe_qualificacao_subjetiva_RGINT"
                              ,"classe_qualificacao_subjetiva_UF"
                              ,"classe_qualificacao_subjetiva_regiao_total")]
dt25[,soma_pesos_classes_log := 
       sum(
         classe_qualificacao_subjetiva_RGINT
         ,classe_qualificacao_subjetiva_UF
         ,classe_qualificacao_subjetiva_regiao_total
         ,na.rm = TRUE
       ),by = .(code_muni)]

## * areas agenda -----

dt2[,code_muni := as.character(code_muni)] # pib
dt8[,code_muni := as.character(code_muni)] # complex 

dt_area_agenda <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                                   ,y = dt2,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt8,by = "code_muni",all = TRUE)

dt_area_agenda <- dt_area_agenda %>% 
  .[,.SD,.SDcols = c("code_muni"
                     ,"classe_area_vulneraveis", "peso_VAB_Adm_pp"
                     ,"classe_area_destaque","peso_VAB_COMBINACAO"
                     ,"classe_ICEM_Ativ","peso_ICEM_Prod")]

dt_area_agenda[,pont_total_pib := sum(
  as.numeric(classe_area_vulneraveis)
  , as.numeric(peso_VAB_Adm_pp)
  , as.numeric(classe_area_destaque)
  , as.numeric(peso_VAB_COMBINACAO)
  , as.numeric(classe_ICEM_Ativ)
  , as.numeric(peso_ICEM_Prod)
  , na.rm = TRUE
),by = .(code_muni)]

## * areas urbanas -----

dt2[,code_muni := as.character(code_muni)]
dt8[,code_muni := as.character(code_muni)]
dt4[,code_muni := as.character(code_muni)]

dt_area_urbana <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                                   ,y = dt8,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt2,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt4,by = "code_muni",all = TRUE)

dt_area_urbana <- dt_area_urbana %>% 
  .[,.SD,.SDcols = c("code_muni"
                     ,"classe_VAB_Ind_pp", "classe_perc_urbana_2010"
                     ,"peso_VAB_COMB_Ind_Serv_altos","classe_inv_urbana")]

dt_area_urbana[,pont_total_area_urbana := sum(
  as.numeric(classe_VAB_Ind_pp)
  , as.numeric(classe_perc_urbana_2010)
  , as.numeric(peso_VAB_COMB_Ind_Serv_altos)
  , as.numeric(classe_inv_urbana)
  , na.rm = TRUE
),by = .(code_muni)]

## * areas rurais -----

dt2[,code_muni := as.character(code_muni)]
dt8[,code_muni := as.character(code_muni)]
dt4[,code_muni := as.character(code_muni)]

dt_area_rural <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                                  ,y = dt8,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt2,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt4,by = "code_muni",all = TRUE)

dt_area_rural <- dt_area_rural %>% 
  .[,.SD,.SDcols = c("code_muni"
                     ,"classe_VAB_Agro_pp", "classe_perc_rural_2010"
                     ,"peso_VAB_COMB_Agro_Serv_altos","classe_inv_rural")]

dt_area_rural[,pont_total_area_rural := sum(
  as.numeric(classe_VAB_Agro_pp)
  , as.numeric(classe_perc_rural_2010)
  , as.numeric(peso_VAB_COMB_Agro_Serv_altos)
  , as.numeric(classe_inv_rural)
  , na.rm = TRUE
),by = .(code_muni)]

### * areas alta complexidade----

dt2[,code_muni := as.character(code_muni)] # pib
dt8[,code_muni := as.character(code_muni)] # pib
dt9[,code_muni := as.character(code_muni)] # complexidade

dt_alta_complx <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                                   ,y = dt8,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt2,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt9,by = "code_muni",all = TRUE)

dt_alta_complx[,classe_prod_base_sim_e_ou_classe_ativ_base_sim := fifelse(
  classe_prod_base_sim == 0.5,0.5,0)]

dt_alta_complx[,prod_div_e_ou_ativ_div := fifelse(
  ativ_div == 0.5,0.5,0)]

dt_alta_complx <- dt_alta_complx %>% 
  .[,.SD,.SDcols = c("code_muni"
                     ,"peso_VAB_COMB_Agro_Ind_altos", "classe_ICEM_Prod_alto"
                     ,"classe_prod_base_sim_e_ou_classe_ativ_base_sim"
                     ,"prod_div_e_ou_ativ_div")]

dt_alta_complx[,pont_total_alta_complexidade := sum(
  as.numeric(peso_VAB_COMB_Agro_Ind_altos)
  , as.numeric(classe_ICEM_Prod_alto)
  , as.numeric(classe_prod_base_sim_e_ou_classe_ativ_base_sim)
  , as.numeric(prod_div_e_ou_ativ_div)
  , na.rm = TRUE
),by = .(code_muni)]

dt_alta_complx[is.na(dt_alta_complx)] <- 0


### * areas baixo e alto risco  -----

dt15[,code_muni := as.character(code_muni)] # meio ambiente
dt16[,code_muni := as.character(code_muni)] # desastres
dt17[,code_muni := as.character(code_muni)] # rec hidricos (I)

dt_area_alto_risco <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                                       ,y = dt15,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt16,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt17,by = "code_muni",all = TRUE)

dt_area_alto_risco[,classe_N_desastres_naturais_nulo := 
                     fifelse(is.na(classe_N_desastres_naturais),1,0)]
dt_area_alto_risco[,classe_comprom_rh_muito_baixo := 
                     fifelse(classe_comprom_rh == 1,1,0)]

dt_area_alto_risco <- dt_area_alto_risco %>% 
  .[,.SD,.SDcols = c("code_muni"
                     ,"classe_comprom_rh"
                     , "classe_vuln_manancial_abast_pub"
                     ,"classe_N_desastres_naturais_nulo","classe_comprom_rh_muito_baixo")]

dt_area_alto_risco[,pont_total_area_alto_risco := sum(
  as.numeric(classe_comprom_rh)
  , as.numeric(classe_vuln_manancial_abast_pub)
  , na.rm = TRUE
),by = .(code_muni)]

dt_area_alto_risco[,pont_total_area_baixo_risco := sum(
  as.numeric(classe_N_desastres_naturais_nulo)
  , as.numeric(classe_comprom_rh_muito_baixo)
  , na.rm = TRUE
),by = .(code_muni)]

dt_area_alto_risco[,pont_total_area_risco := sum(
  as.numeric(pont_total_area_baixo_risco)
  , as.numeric(pont_total_area_alto_risco)
  , na.rm = TRUE
),by = .(code_muni)]
### * areas aparato merge ----


dt6[,code_muni := as.character(code_muni)]  
dt11[,code_muni := as.character(code_muni)] 
dt12[,code_muni := as.character(code_muni)] 
dt13[,code_muni := as.character(code_muni)] 
dt13a[,code_muni := as.character(code_muni)] 

dt_aparato <- merge.data.table(x = df_geral[,.SD,.SDcols = "code_muni"]
                               ,y = dt6,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt11,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt12,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt13a,by = "code_muni",all = TRUE) %>%  
  merge.data.table(x = .,y = dt13,by = "code_muni",all = TRUE)

dt_aparato <- dt_aparato %>% 
  .[,.SD,.SDcols = c("code_muni"
                     , "classe_orint_tec_sistemaS"
                     , "classe_orint_tec_proprio_produtor"
                     , "classe_orint_tec_cooperativas"
                     , "classe_orint_tec_empresas"
                     , "classe_orint_tec_ong"
                     , "classe_orint_tec_gov"
                     , "classe_presenca_SEBRAE"
                     , "classe_presenca_SistemaS"
                     , "classe_presenca_Embrapa"
                     , "classe_presenca_codevasf"
                     , "classe_presenca_agencias_de_fomento"
  )]

dt_aparato[is.na(dt_aparato)] <- 0

dt_aparato[,total_classe_presenca_institucional := sum(
  as.numeric(classe_orint_tec_sistemaS)
  , as.numeric(classe_orint_tec_proprio_produtor)
  , as.numeric(classe_orint_tec_cooperativas)
  , as.numeric(classe_orint_tec_empresas)
  , as.numeric(classe_orint_tec_ong)
  , as.numeric(classe_orint_tec_gov)
  , as.numeric(classe_presenca_SEBRAE)
  , as.numeric(classe_presenca_SistemaS)
  , as.numeric(classe_presenca_Embrapa)
  , as.numeric(classe_presenca_codevasf)
  , as.numeric(classe_presenca_agencias_de_fomento)
  , na.rm = TRUE
),by = .(code_muni)]


### * areas soma TOTAL URBANO TOTAL RURAL ------
dt_total_filtros_rur_urb <- df_geral[,code_muni := as.character(code_muni)] %>%
  merge.data.table(x = .,y = dt2,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_area_rural,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_area_urbana,by = "code_muni",all = TRUE) %>%
  merge.data.table(x = .,y = dt_alta_complx,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_area_alto_risco,by = "code_muni",all = TRUE)
dt_total_filtros_rur_urb[,total_filtros_123467_rural := 
                           sum(
                             pont_total_pib
                             ,pont_total_area_rural
                             ,pont_total_alta_complexidade
                             ,pont_total_area_risco
                             , na.rm = TRUE
                           ),by = .(code_muni)]
dt_total_filtros_rur_urb[,total_filtros_123567_urbana := 
                           sum(
                             pont_total_pib
                             ,pont_total_area_urbana
                             ,pont_total_alta_complexidade
                             ,pont_total_area_risco
                             , na.rm = TRUE
                           ),by = .(code_muni)]

dt_total_filtros_rur_urb <- dt_total_filtros_rur_urb[,.SD,.SDcols = c(
  "code_muni","total_filtros_123467_rural","total_filtros_123567_urbana")]

# class DEPENDENTE -- compt municipio ----

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
dt13a[,code_muni := as.character(code_muni)]
dt14[,code_muni := as.character(code_muni)]
dt15[,code_muni := as.character(code_muni)]
dt16[,code_muni := as.character(code_muni)]
dt17[,code_muni := as.character(code_muni)]
dt18[,code_muni := as.character(code_muni)]
dt19[,code_muni := as.character(code_muni)]
dt20[,code_muni := as.character(code_muni)]
dt21[,code_muni := as.character(code_muni)]
dt23[,code_muni := as.character(code_muni)]
dt24[,code_muni := as.character(code_muni)]
dt25[,code_muni := as.character(code_muni)]
dt_area_urbana[,code_muni := as.character(code_muni)]
dt_area_rural[,code_muni := as.character(code_muni)]
dt_alta_complx[,code_muni := as.character(code_muni)]
dt_area_alto_risco[,code_muni := as.character(code_muni)]
dt_aparato[,code_muni := as.character(code_muni)]
dt_total_filtros_rur_urb[,code_muni := as.character(code_muni)]

#### merge ----
NewColtoRem <- c("abbrev_state","bacia","regiao_total","code_intermediate")
dt_competiv_muni <- df_geral[,code_muni := as.character(code_muni)] %>%
  # areas aparato
  merge.data.table(x = .,y = dt_aparato[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # EMPREGO E EMPRESAS
  merge.data.table(x = .,y = dt14[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # BALANCA COMERCIAL 
  merge.data.table(x = .,y = dt5[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # AGLOMERACOES PROD
  merge.data.table(x = .,y = dt21[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # COMPLEXIDADE ECONOMICA
  merge.data.table(x = .,y = dt9[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # EDUCACAO
  merge.data.table(x = .,y = dt7[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  # CTI
  merge.data.table(x = .,y = dt23[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # FNE
  merge.data.table(x = .,y = dt8[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # licenciamento ambiental
  merge.data.table(x = .,y = dt24[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # INFRA_EE
  merge.data.table(x = .,y = dt18[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  # INFRA_TELE
  merge.data.table(x = .,y = dt19[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)   %>% 
  # INFRA_SAN 1 2
  merge.data.table(x = .,y = dt20[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  # Politica Urbana
  merge.data.table(x = .,y = dt10[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  # MEIO AMBIENTE
  merge.data.table(x = .,y = dt15[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # Logistica
  merge.data.table(x = .,y = dt25[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  # agro
  merge.data.table(x = .,y = dt3[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  # POLITICA RURAL
  merge.data.table(x = .,y = dt1[,.SD,.SDcols = c('code_muni',
                                                  'classe_area_irrig_orient_tec'
                                                  ,'classe_num_estab_agr_familiar'
                                                  ,'classe_num_estab_orient_outrasNAOgov'
                                                  , 'classe_num_estab_orient_total' 
                                                  , 'soma_classes_pol_rurais'
  )]
  ,by = "code_muni",all = TRUE) %>% 
  # DADOS CLASSIFICACO INDEPENDENTE
  merge.data.table(x = .,y = dt_area_agenda[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_area_urbana[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_area_alto_risco[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)  %>% 
  merge.data.table(x = .,y = dt_area_rural[,(NewColtoRem) := NULL][,classe_inv_rural := NULL]
                   ,by = "code_muni",all = TRUE)  %>% 
  merge.data.table(x = .,y = dt_alta_complx[,(NewColtoRem) := NULL][,pont_total_alta_complexidade := NULL]
                   ,by = "code_muni",all = TRUE) %>% 
  merge.data.table(x = .,y = dt_total_filtros_rur_urb[,(NewColtoRem) := NULL],by = "code_muni",all = TRUE)

break()
dt_competiv_muni <- dt_competiv_muni[,.SD,.SDcols = c(
  names(df_geral)
  # CLASSIFICACAO INDEPENDENTE 
  ## CLASSIFICAÇÃO DAS AGENDAS
  , 'classe_area_vulneraveis' 
  , 'peso_VAB_Adm_pp'
  , 'classe_area_destaque' 
  , 'peso_VAB_COMBINACAO'
  , 'classe_ICEM_Ativ'
  , 'peso_ICEM_Prod' 
  , 'pont_total_pib' 
  ### CLASSIFICAÇÃO ÁREAS RURAIS
  , 'classe_VAB_Agro_pp'
  , 'classe_perc_rural_2010'
  , 'peso_VAB_COMB_Agro_Serv_altos'
  , 'classe_inv_rural'
  , 'pont_total_area_rural'
  ### CLASSIFICAÇÃO ÁREAS URBANAS
  , 'classe_VAB_Ind_pp'
  , 'classe_perc_urbana_2010'
  , 'peso_VAB_COMB_Ind_Serv_altos' 
  , 'inv_urbano'
  , 'pont_total_area_urbana'
  ### CLASSIFICAÇÃO ÁREAS ALTA COMPLEXIDADE
  , 'peso_VAB_COMB_Agro_Ind_altos'
  , 'classe_ICEM_Prod_alto'
  , 'classe_prod_base_sim_e_ou_classe_ativ_base_sim'
  , 'prod_div_e_ou_ativ_div'
  , 'pont_total_alta_complexidade'
  ### CLASSIFICAÇÃO ÁREAS BAIXO E ALTO RISCO
  , 'classe_N_desastres_naturais_nulo'
  , 'classe_comprom_rh'
  , 'classe_vuln_manancial_abast_pub'
  , 'classe_N_desastres_naturais_nulo'
  , 'classe_comprom_rh_muito_baixo'
  ### FILTROS
  , 'pont_total_area_alto_risco'
  , 'pont_total_area_baixo_risco'
  , 'pont_total_area_risco'
  , 'total_filtros_123467_rural'
  , 'total_filtros_123567_urbana'
  # CLASSIFICAÇÕES DEPENDENTES - COMPETITIVIDADE
  ## COMPETITIVIDADE MUNICÍPIO
  ### PRESENCA INSTITUCIONAL
  , 'classe_orint_tec_sistemaS'
  , 'classe_orint_tec_proprio_produtor'
  , 'classe_orint_tec_cooperativas'
  , 'classe_orint_tec_empresas'
  , 'classe_orint_tec_ong'
  , 'classe_orint_tec_gov'
  , 'classe_presenca_SEBRAE'
  , 'classe_presenca_SistemaS'
  , 'classe_presenca_Embrapa'
  , 'classe_presenca_codevasf'
  , 'classe_presenca_agencias_de_fomento'
  , 'total_classe_presenca_institucional'
  ### Empregos e Empresas
  , 'classe_remun_capita_br'
  , 'classe_rem_RGINT_perc_rem_Estado'
  , 'classe_rem_RGINT_perc_rem_Bacia'
  , 'classe_rem_RGINT_perc_rem_RTP'
  , 'classe_N_empresas'
  , 'classe_empresas_RGINT_perc_empresas_Estado'
  , 'classe_empresas_RGINT_perc_empresas_Bacia'
  , 'classe_empresas_RGINT_perc_empresas_RTP'
  , 'total_classe_empregos_empresas'
  ### Balança Comercial
  , 'classe_exportacao'
  , 'classe_exp_RGINT_perc_exp_Estado'
  , 'classe_exp_RGINT_perc_exp_Bacia'
  , 'classe_exp_RGINT_perc_exp_RTP'
  , 'total_classe_comex'
  ### Aglomerações Produtivas
  , 'classe_presenca_apl'
  , 'classe_presenca_rotas'
  , 'soma_classes_apls_e_rotas_sim'
  ### Complexidade Econômica
  #, 'classe_grau_div_ativ_mun_kr0'
  #, 'classe_grau_div_prod_mun_km0'
  #, 'soma_pesos_grau_div_mun'
  ### Educação
  , 'classe_inv_educ_infantil'
  , 'classe_inv_educ_basica'
  , 'classe_inv_ensino_funmental'
  , 'classe_inv_ensino_medio'
  , 'classe_inv_educ_profissional'
  , 'soma_pesos_educacao'
  ### CT&I
  , 'classe_vab_serv_alto'
  , 'classe_presenca_ies'
  , 'classe_presenca_ies_pos_grad'
  , 'classe_presenca_curso_graduacao'
  , 'classe_presenca_artigo_publicados'
  , 'classe_presenca_patente'
  , 'soma_pesos_cti_sim'
  ### FNE
  , 'classe_num_contratos'
  , 'classe_investimento'
  , 'classe_inv_pj'
  , 'classe_inv_comercio'
  , 'classe_inv_turismo'
  , 'soma_pesos_fne'
  ### Licenciamentos Ambientais
  , 'classe_tempo_licenciamento'
  , 'classe_presenca_de_licenciamento_ambiental'
  ### Infraestrutura Telecomunicações
  , 'classe_D_BLF'
  , 'classe_D_TM'
  , 'soma_pesos_classes_tele'
  ### Infraestrutura Saneamento e Resíduos Sólidos
  , 'classe_IN022'
  , 'classe_IN049' 
  , 'classe_IN023'
  , 'classe_IN024' 
  , 'classe_IN046'
  , 'classe_SanResiCa005'
  , 'classe_SanResiCa008'
  , 'classe_SanResiIN014'
  , 'classe_SanResiIN031'
  , 'classe_SanResiUP027'
  , 'classe_SanResiUP029'
  , 'soma_pesos_classes_san_res'
  ### Infraestrutura Energia Elétrica
  , 'classe_DEC'
  , 'classe_FEC'
  , 'soma_pesos_classes_ee'
  ### Infraestrutura Logística
  , 'classe_qualificacao_subjetiva_RGINT'
  , 'classe_qualificacao_subjetiva_UF'
  , 'classe_qualificacao_subjetiva_regiao_total'
  , 'soma_pesos_classes_log'
  ### Políticas Urbanas
  , 'classe_controle_urb'
  , 'classe_nivel_integr'
  , 'soma_pesos_classes_pol_urb'
  ### Meio Ambiente
  , 'classe_tx_nt_10_19'
  , 'classe_area_uc_tx_cresc_2010_2021'
  , 'soma_pesos_classes_ma'
  ## COMPETITIVIDADE ÁREAS RURAIS
  ### Agropecuária, Silvicultura e Aquicultura
  , 'classe_muni_valor_da_producao'
  , 'classe_agro_valor_da_producao_RGINT_perc_Estado'
  , 'classe_agro_valor_da_producao_RGINT_perc_Bacia'
  , 'classe_agro_valor_da_producao_RGINT_perc_RTP'
  , 'soma_pesos_classes_agricultura'
  , 'classe_muni_valor_silv'
  , 'classe_muni_valor_silv_RGINT_perc_Estado'
  , 'classe_muni_valor_silv_RGINT_perc_Bacia'
  , 'classe_muni_valor_silv_RGINT_perc_RTP'
  , 'soma_pesos_classes_silvicultura'
  , 'classe_muni_valor_aqui'
  , 'classe_muni_valor_aqui_RGINT_perc_Estado'
  , 'classe_muni_valor_aqui_RGINT_perc_Bacia'
  , 'classe_muni_valor_aqui_RGINT_perc_RTP'
  , 'soma_pesos_classes_aquicultura'
  , 'classe_muni_valor_rebanho'
  , 'classe_muni_valor_rebanho_RGINT_perc_Estado'
  , 'classe_muni_valor_rebanho_RGINT_perc_Bacia'
  , 'classe_muni_valor_rebanho_RGINT_perc_RTP'
  , 'soma_pesos_classes_rebanhos'
  ### FNE - RURAL
  , 'classe_inv_agricola'
  , 'classe_inv_agroindustria'
  , 'classe_inv_pecuaria'
  , 'soma_classes_fne_rural'
  ### Políticas Rurais
  , 'classe_area_irrig_orient_tec'
  , 'classe_num_estab_agr_familiar' 
  , 'classe_num_estab_orient_total'
  , 'classe_num_estab_orient_outrasNAOgov' 
  , 'soma_classes_pol_rurais'
)]



dt_competiv_muni[is.na(dt_competiv_muni)] <- 0

dt_competiv_muni[,soma_totais_competitiv_area_rural := sum(
  soma_classes_pol_rurais
  , soma_classes_fne_rural
  , soma_pesos_classes_rebanhos
  , soma_pesos_classes_aquicultura
  , soma_pesos_classes_silvicultura
  , soma_pesos_classes_agricultura
  , na.rm = TRUE
),by = .(code_muni)]

dt_competiv_muni[,soma_totais_competitiv_mun := sum(
  soma_pesos_educacao
  , soma_pesos_cti_sim
  , soma_pesos_fne
  , soma_pesos_classes_tele
  , soma_pesos_classes_san_res
  , soma_pesos_classes_ee
  , soma_pesos_classes_log
  , soma_pesos_classes_pol_urb
  , soma_pesos_classes_ma
  , na.rm = TRUE
),by = .(code_muni)]

dt_competiv_muni[,TOTAL_RANK_MUN := sum(
  soma_totais_competitiv_mun
  ,total_filtros_123567_urbana
  ,na.rm = TRUE),by = .(code_muni)]

dt_competiv_muni[,TOTAL_RANK_MUN_RURAL := sum(
  soma_totais_competitiv_mun
  ,soma_totais_competitiv_area_rural
  ,na.rm = TRUE),by = .(code_muni)]
dt_competiv_muni[1]

# set new order

fixCols <- c('code_muni','name_muni','TOTAL_RANK_MUN','TOTAL_RANK_MUN_RURAL')
DiffCols <- setdiff(names(dt_competiv_muni),fixCols)
NewOrderCol <- c(fixCols,DiffCols)
dt_competiv_muni <- dt_competiv_muni[,.SD,.SDcols = NewOrderCol]

#### SAVE -----
dt_competiv_muni <- dt_competiv_muni %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = dt_competiv_muni
                           ,ss = link_gdocs
                           ,sheet = "RANK_COMPETITIVIDADE") 
