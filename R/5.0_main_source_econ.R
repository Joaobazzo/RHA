# function input data ----

function_rel_pib <- function(arquivo_resultado,s_input){ # s_input = "2104"

# prep to exclude -----
gc(reset = TRUE)
library(readr)
library(ggplot2)
library(data.table)
library(magrittr)
library(officedown)
library(flextable)
library(officer)

#
my_division <- function(a,b,n_round = 2){round(100 * a / b , n_round)}
formato_bonitinho <- function(x) {
 # if(is.na(x)) return(NA)
  #diff_nchar <- nchar(x) - nchar(round(x))
  formatC(
    x,
    , format = "f"
    , big.mark =  "."
    , big.interval = 3
    , decimal.mark = ","
    #, digits = ifelse(diff_nchar>2,2,diff_nchar)
    #, width = nchar(round(x))+3
  )
}


# INTRO -----
## 
s_name_rgint  <- df_geral[code_intermediate == s_input,unique(name_intermediate)]
s_name_state  <- df_geral[code_intermediate == s_input,unique(name_state)]
s_abbrev_state <- df_geral[code_intermediate == s_input,unique(abbrev_state)]
s_code_state <- df_geral[code_intermediate == s_input,unique(code_state)]
s_name_region <- df_geral[code_intermediate == s_input,unique(name_region)]
s_name_bacia <- df_geral[code_intermediate == s_input,unique(bacia)][1]
if(is.na(s_name_bacia)){has_bacia <- FALSE;s_name_bacia == "PISF"}else{has_bacia <- TRUE}


s_code_munis_rgint <- df_geral[code_intermediate == s_input,code_muni]
s_code_munis_state <- df_geral[name_state == s_name_state,code_muni]
s_code_munis_bacia <- df_geral[bacia == s_name_bacia,code_muni]
s_code_munis_rtp <- df_geral[regiao_total == 1,code_muni]
s_code_munis_region <- df_geral[name_region == s_name_region,code_muni]



# 5.1.1 ICE ----

top_n_ice <- function(f_dt,df_to_merge,f_code_muni){
  vec <- data.table::copy(f_dt) %>% 
    .[code_muni %in% f_code_muni,] %>% 
    .[order(ICE,decreasing = TRUE),] %>% 
    .[,ICE := round(ICE,3)]
  tmp_dt <- df_to_merge[code_muni %in% vec$code_muni,] %>% 
    .[,.SD,.SDcols = c("name_muni","code_muni")] %>% 
    .[vec,on = "code_muni"] %>% 
    .[,code_muni := NULL]
  return(tmp_dt)
}

qdxx <- top_n_ice(df_to_merge = df_geral
                   ,f_dt = dt_PSICE
                   ,f_code_muni = s_code_munis_rgint)
qdxx[]
names(qdxx) <- c("Município","Índice de Complexidade Econômica (2020)")

filter_muni <- function(dt,f_muni,sdcols = NULL){
  tmp <-  dt[code_muni %in% f_muni,]
  if(!is.null(sdcols)) tmp <- tmp[,.SD,.SDcols = c(sdcols)]  
  return(tmp)
}
# muni
tmp1_qd24 <- filter_muni(dt_comex,s_code_munis_rgint)
tmp2_qd24 <- filter_muni(dt_rem,s_code_munis_rgint,c("code_muni","remun_total"))
tmp3_qd24 <- filter_muni(dt_PSICE,s_code_munis_rgint)
tmp4_qd24 <- filter_muni(dt_ActivK,s_code_munis_rgint,c("code_muni","Kr0"))
tmp5_qd24 <- filter_muni(dt_PSExpY,s_code_munis_rgint,c("code_muni","ExpY")) 
intr_qd24 <- filter_muni(df_geral,s_code_munis_rgint,c("code_muni","name_muni"))
qd_24_muni <- merge.data.table(intr_qd24,tmp1_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp2_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp3_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp4_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp5_qd24,by = "code_muni",all = TRUE) 

qd_24_muni[]
# rgint
tmp1_qd24 <- filter_muni(dt_comex,s_code_munis_rgint) %>% 
  .[,list(code_muni = s_input
          ,exportacao = sum(exportacao,na.rm = TRUE),
          importacao = sum(importacao,na.rm = TRUE),
          saldo = sum(saldo,na.rm = TRUE))]
tmp2_qd24 <- filter_muni(dt_rem,s_code_munis_rgint,c("code_muni","remun_total")) %>% 
  .[,list(code_muni = s_input
          ,remun_total = sum(remun_total,na.rm = TRUE))]
tmp3_qd24 <- filter_muni(dt_PSICE,s_input)
tmp4_qd24 <- filter_muni(dt_ActivK,s_input,c("code_muni","Kr0")) 
tmp5_qd24 <- filter_muni(dt_PSExpY,s_input,c("code_muni","ExpY"))
intr_qd24 <- df_geral[code_intermediate == s_input,][1] %>% 
  .[,.SD,.SDcols = c("code_intermediate","name_intermediate")] %>% 
  setnames(.,c("code_intermediate","name_intermediate")
           ,c("code_muni","name_muni"))

qd_24_rgint <- merge.data.table(intr_qd24,tmp1_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp2_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp3_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp4_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp5_qd24,by = "code_muni",all = TRUE) 
# state
tmp1_qd24 <- filter_muni(dt_comex,s_code_munis_state) %>% 
  .[,list(code_muni = s_abbrev_state
          ,exportacao = sum(exportacao,na.rm = TRUE),
          importacao = sum(importacao,na.rm = TRUE),
          saldo = sum(saldo,na.rm = TRUE))]
tmp2_qd24 <- filter_muni(dt_rem,s_code_munis_state,c("code_muni","remun_total")) %>% 
  .[,list(code_muni = s_abbrev_state
          ,remun_total = sum(remun_total,na.rm = TRUE))]
tmp3_qd24 <- filter_muni(dt_PSICE,s_abbrev_state) 
tmp4_qd24 <- filter_muni(dt_ActivK,s_abbrev_state,c("code_muni","Kr0")) 
tmp5_qd24 <- filter_muni(dt_PSExpY,s_code_state,c("code_muni","ExpY")) %>% 
  .[,code_muni := s_abbrev_state]
intr_qd24 <- df_geral[code_intermediate == s_input,][1] %>% 
  .[,.SD,.SDcols = c("abbrev_state","name_state")] %>% 
  setnames(.,c("abbrev_state","name_state")
           ,c("code_muni","name_muni"))

qd_24_state <- merge.data.table(intr_qd24,tmp1_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp2_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp3_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp4_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp5_qd24,by = "code_muni",all = TRUE) 
# rtp
tmp1_qd24 <- filter_muni(dt_comex,s_code_munis_rtp) %>% 
  .[,list(code_muni = "Projeto"
          ,exportacao = sum(exportacao,na.rm = TRUE),
          importacao = sum(importacao,na.rm = TRUE),
          saldo = sum(saldo,na.rm = TRUE))]
tmp2_qd24 <- filter_muni(dt_rem,s_code_munis_rtp,c("code_muni","remun_total")) %>% 
  .[,list(code_muni = "Projeto"
          ,remun_total = sum(remun_total,na.rm = TRUE))]
tmp3_qd24 <- filter_muni(dt_PSICE,"Projeto") 
tmp4_qd24 <- filter_muni(dt_ActivK,"TOTAL",c("code_muni","Kr0")) %>% .[,code_muni := "Projeto"]
tmp5_qd24 <- filter_muni(dt_PSExpY,"Projeto",c("code_muni","ExpY")) %>% 
  .[,code_muni := "Projeto"]


qd_24_rtp <- 
  merge.data.table(tmp1_qd24,tmp2_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp3_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp4_qd24,by = "code_muni",all = TRUE) %>% 
  merge.data.table(.,tmp5_qd24,by = "code_muni",all = TRUE) 

# 
q24 <- data.table::rbindlist(l = list(qd_24_muni,qd_24_rgint,qd_24_state,qd_24_rtp)
                             ,use.names = TRUE,fill = TRUE)
getCols <- c('exportacao', 'importacao', 'saldo', 'remun_total', 'ICE', 'Kr0', 'ExpY')
q24[,(getCols) := lapply(.SD,round,3),.SDcols = getCols]
q24[is.na(q24)] <- "-"
names(q24) <- c("Codigo","Município","Exportação (R$)"
                ,"Importação (R$)","Saldo (R$)"," Remuneração (2020)"
                ,"Índice de Complexidade Econômica"
                ,"Diversificação (Kr0)"
                ,"Nível de Produtividade-renda das Exportações (ExpY)")


# 5.1.3 div prod -----
cria_rank_prod <- function(dt, region, order_rank_base){
  tmp_dt <- dt[code_muni %in% region] %>% 
    setorder(.,-ICPBase,-ICPDiv) %>%
    .[,rank_base := .GRP,by = .(ProdBase)] %>%
    .[,rank_div := 1:.N,by = .(ProdBase)] %>%
    .[,Base_full_name := sprintf("%s - Produto Base: (Cód. %s, ICP3: %s) %s.",
                                 rank_base,ProdBase,round(ICPBase,4),NomeBase)] %>% 
    .[,Div_full_name := sprintf("%s - Cód. %s, ICP: %s %s:",
                                rank_div,ProdDiv,round(ICPDiv,4),NomeDiv)] %>% 
    .[rank_base == order_rank_base,] %>% 
    .[,.SD,.SDcols = c("Base_full_name", "Div_full_name","rank_div")] %>% 
    dcast(., Div_full_name + rank_div ~ Base_full_name,value.var = "Div_full_name") %>% 
    setorder(.,rank_div) %>% .[,3]
  return(tmp_dt)
}


n_prodbase <- uniqueN(dt_ListaProdutos[code_muni %in% s_input,]$ProdBase)
prodBaseList <- lapply(1:5,function(i){
  if(i <= n_prodbase){
  tmp <- cria_rank_prod(dt_ListaProdutos,s_input,i) 
  }else{
    tmp <- data.frame("-")
  }
})


# atividade
cria_rank_ativ <- function(dt, region, order_rank_base){
  tmp_dt <- dt[code_muni %in% region] %>% 
    setorder(.,-ICABase,-ICADiv) %>%
    .[,rank_base := .GRP,by = .(ActivBase)] %>%
    .[,rank_div := 1:.N,by = .(ActivBase)] %>%
    .[,Base_full_name := sprintf("%s - Atividade Base: (Cód. %s, ICP3: %s) %s.",
                                 rank_base,ActivBase,round(ICABase,4),NomeBase)] %>% 
    .[,Div_full_name := sprintf("%s - Cód. %s, ICP: %s %s.",
                                rank_div,ActivDiv,round(ICADiv,4),NomeDiv)] %>% 
    .[rank_base == order_rank_base,] %>% 
    .[,.SD,.SDcols = c("Base_full_name", "Div_full_name","rank_div")] %>% 
    dcast(., Div_full_name + rank_div ~ Base_full_name,value.var = "Div_full_name") %>% 
    setorder(.,rank_div) %>% .[,3]
  return(tmp_dt)
}

ativbase1 <- cria_rank_ativ(dt_ListaAtividadesDiv,s_input,1)

# 6 TENDÊNCIAS DE INVESTIMENTOS ------------
# 6.1 INSTRUMENTOS DE INVESTIMENTOS DA SUDENE
# 6.1.1 FNE

prop_cnpj_inv <- function(dt,f_muni){
  tmp_dt <- dt %>% 
    .[code_muni %in% f_muni,] %>% 
    .[,list(
      inv_pf = my_division(
        sum(inv_pf,na.rm = TRUE)
        ,sum(inv_pf,na.rm = TRUE) + sum(inv_pj,na.rm = TRUE)
        ,1)
      , inv_pj =  my_division(
        sum(inv_pj,na.rm = TRUE)
        ,sum(inv_pf,na.rm = TRUE) + sum(inv_pj,na.rm = TRUE)
        ,1)
    )]
  return(tmp_dt)
}

qd37 <- prop_cnpj_inv(dt_fne,s_code_munis_rgint)
names(qd37) <- c("PF (%)","PJ (%)")

func_qd38 <- function(dt,f_muni,myMeasure,isRural = TRUE){
  tmp_dt <- dt %>% 
    .[code_muni %in% f_muni,] %>% 
    .[,list(
      inv_rural     = sum(inv_rural     ,na.rm = TRUE)
      , inv_agr       = sum(inv_agr       ,na.rm = TRUE)
      , inv_agroind   = sum(inv_agroind   ,na.rm = TRUE)
      , inv_pec       = sum(inv_pec       ,na.rm = TRUE)
      , inv_agr       = sum(inv_agr       ,na.rm = TRUE)
      , inv_agroind   = sum(inv_agroind   ,na.rm = TRUE)
      , inv_comercio  = sum(inv_comercio  ,na.rm = TRUE)
      , inv_ind       = sum(inv_ind       ,na.rm = TRUE)
      , inv_infra     = sum(inv_infra     ,na.rm = TRUE)
      , inv_pec       = sum(inv_pec       ,na.rm = TRUE)
      , inv_pf        = sum(inv_pf        ,na.rm = TRUE)
      , inv_pj        = sum(inv_pj        ,na.rm = TRUE)
      , inv_rural     = sum(inv_rural     ,na.rm = TRUE)
      , inv_turismo   = sum(inv_turismo   ,na.rm = TRUE)
      , inv_urbano    = sum(inv_urbano    ,na.rm = TRUE)    
    )] %>% 
    .[,value_total := fcase(isRural,inv_rural
                            ,!isRural,inv_urbano)] %>% 
    melt(., 
         measure.vars = myMeasure) %>% 
    
    .[,perc := my_division(value
                           ,value_total,1)] %>% 
    .[,.SD,.SDcols = c('variable','value','perc')]
}

qd38 <- func_qd38(dt = dt_fne
                  ,f_muni = s_code_munis_rgint
                  ,myMeasure = c('inv_rural' , 
                                 'inv_agr' ,   
                                 'inv_agroind',
                                 'inv_pec')
                  ,isRural = TRUE)
qd38[]
names(qd38) <- c("FNE","R$","%")
qd38[,FNE := fcase(
  FNE == "inv_rural","Rural",
  FNE == "inv_agr","Agricultura",
  FNE == "inv_agroind","Agroindústria",
  FNE == "inv_pec","Pecuária"
)]

qd39 <- func_qd38(dt = dt_fne
                  ,f_muni = s_code_munis_rgint
                  ,myMeasure = c('inv_urbano'
                                 ,'inv_comercio'  
                                 , 'inv_ind'   
                                 , 'inv_infra'
                                 , 'inv_turismo')
                  ,isRural = FALSE)
qd39[,variable := fcase(
  variable == 'inv_urbano'  ,"Urbano",
  variable == 'inv_comercio',"Comércio",
  variable == 'inv_ind'     ,"Indústria",
  variable == 'inv_infra'   ,"Infraestrutura",
  variable == 'inv_turismo' ,"Turismo"
)]
names(qd39) <- c("FNE","R$","%")

get_f_q25 <- function(dt,code_inter,situacao,sum = FALSE){
  tmp <- dt %>% 
    .[code_intermediate == code_inter,] %>% 
    .[urbano_rural == situacao,] %>% 
    .[,":="(code_intermediate = NULL,
            urbano_rural = NULL)] %>% 
    .[,":="(`%` = my_division(Total,sum(Total,na.rm = TRUE)))] %>% 
    setnames(.,'programa','FNE')
  if(sum){
    getCols <- setdiff(names(tmp),"FNE")
    tmp <- tmp[,lapply(.SD,sum,na.rm = TRUE),.SDcols = getCols]
    tmp$FNE <- paste("TOTAL",situacao)
  }
  return(tmp)
}

q25_tp1 <- get_f_q25(dt = dt_fne_v1,code_inter = s_input,situacao = "RURAL",sum = TRUE)
q25_tp2 <- get_f_q25(dt_fne_v1,s_input,"RURAL")
q25_tp3 <- get_f_q25(dt_fne_v1,s_input,"URBANO",sum = TRUE)
q25_tp4 <- get_f_q25(dt_fne_v1,s_input,"URBANO")
q25 <- data.table::rbindlist(
  l = list(q25_tp1,q25_tp2,q25_tp3,q25_tp4)
  ,use.names = TRUE,fill = TRUE)
q25 <- q25[,.SD,.SDcols = c("FNE","PF","PJ","Total","%")]
q25[,Total := formato_bonitinho(Total)]

get_f_q26 <- function(dt,code_inter,situacao,sum = FALSE){
tmp <- dt %>% 
    .[code_intermediate == code_inter,] %>% 
    .[urbano_rural == situacao,] %>% 
    .[,":="(code_intermediate = NULL,
            urbano_rural = NULL)] %>% 
    .[,":="(`%` = my_division(Total,sum(Total,na.rm = TRUE)))]

  if(sum){
    getCols <- setdiff(names(tmp),"setor")
    tmp <- tmp[,lapply(.SD,sum,na.rm = TRUE),.SDcols = getCols]
    tmp$setor <- paste("TOTAL",situacao)
  }
  return(tmp)
}

q26_tp1 <- get_f_q26(dt_fne_v2,s_input,"RURAL",sum = TRUE)
q26_tp2 <- get_f_q26(dt_fne_v2,s_input,"RURAL")
q26_tp3 <- get_f_q26(dt_fne_v2,s_input,"URBANO",sum = TRUE)
q26_tp4 <- get_f_q26(dt_fne_v2,s_input,"URBANO")
q26 <- data.table::rbindlist(
  l = list(q26_tp1,q26_tp2,q26_tp3,q26_tp4)
  ,use.names = TRUE,fill = TRUE)
q26 <- q26[,.SD,.SDcols = c('setor','Capital de giro isolado'
                            , 'Comercialização'
           , 'Custeio isolado', 'Investimento', 'Total', '%')]
q26[,Total := formato_bonitinho(Total)]
q26[1]

# INPUT Doc-----
rmarkdown::render(
  input = "inst/rmarkdown/relatorio_econ.Rmd"
  , output_file = arquivo_resultado
  , params = list(
    # INTRO
     f_code_intermediate =   s_input
    , f_qdxx             =   as.data.frame(qdxx)
    , f_q24              =   as.data.frame(q24)
    , f_q25              =   as.data.frame(q25)
    , f_q26              =   as.data.frame(q26)
    , f_qd37             =   as.data.frame(qd37)
    , f_qd38             =   as.data.frame(qd38)
    , f_qd39             =   as.data.frame(qd39)
    , f_prodbase1        =   prodBaseList[[1]]
    , f_prodbase2        =   prodBaseList[[2]]
    , f_prodbase3        =   prodBaseList[[3]]
    , f_prodbase4        =   prodBaseList[[4]]
    , f_prodbase5        =   prodBaseList[[5]]
    , f_ativbase1        =   as.data.frame(ativbase1)
  )
  ,quiet = TRUE
)



  return(NULL)
#
}
# ** end -----