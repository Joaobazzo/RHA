# 2) function input data ----

function_rel_pib <- function(arquivo_resultado,s_input){ # s_input = "2101"
  
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
  my_division <- function(a,b){round(100 * a / b , 2)}
  formato_bonitinho <- function(x) {
    diff_nchar <- nchar(x) - nchar(round(x))
    formatC(
      x,
      , format = "f"
      , big.mark =  "."
      , big.interval = 3
      , decimal.mark = ","
      , digits = ifelse(diff_nchar>2,2,diff_nchar)
      , width = nchar(round(x))+3
    )
  }
  
  #s_input = "2101"
  # INTRO -----
  ## 
  s_name_rgint  <- df_geral[code_intermediate == s_input,unique(name_intermediate)]
  s_name_state  <- df_geral[code_intermediate == s_input,unique(name_state)]
  s_name_region <- df_geral[code_intermediate == s_input,unique(name_region)]
  s_name_bacia <- df_geral[code_intermediate == s_input,unique(bacia)][1]
  if(is.na(s_name_bacia)){has_bacia <- FALSE;s_name_bacia == "PISF"}else{has_bacia <- TRUE}
  
  
  s_code_munis_rgint <- df_geral[code_intermediate == s_input,code_muni]
  s_code_munis_state <- df_geral[name_state == s_name_state,code_muni]
  s_code_munis_bacia <- df_geral[bacia == s_name_bacia,code_muni]
  s_code_munis_rtp <- df_geral[regiao_total == 1,code_muni]
  s_code_munis_region <- df_geral[name_region == s_name_region,code_muni]
  
  
  
  # AGRO ----
  
  dt_agro$municipio_codigo %>% uniqueN()
  dt_agro$variavel %>% unique()
  dt_agro$produto_das_lavouras_temporarias_e_permanentes %>% unique()
  
  ## agro ----
  top_3_agro <- function(f_dt,f_code_muni,collapse = TRUE){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"produto_das_lavouras_temporarias_e_permanentes","produto") %>% 
      .[variavel %in% "Valor da produção",] %>% 
      .[produto != "Total",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:3,produto]
    if(collapse){
      vec <- paste0("(",1:3,") ",vec,collapse = ", ")
    }
    return(vec)
  }
  
  
  qd_27 <- data.table("Local" = c( paste("RGINT",s_name_rgint  )
                                   ,paste("Estado",s_name_state)
                                   ,paste("Bacia",s_name_bacia)
                                   ,paste("Região",s_name_region)
                                   ,"Região Total"),
                      "Produto" = c(
                        top_3_agro(f_dt = dt_agro,f_code_muni = s_code_munis_rgint),
                        top_3_agro(f_dt = dt_agro,f_code_muni = s_code_munis_state),
                        top_3_agro(f_dt = dt_agro,f_code_muni = s_code_munis_bacia),
                        top_3_agro(f_dt = dt_agro,f_code_muni = s_code_munis_region),
                        top_3_agro(f_dt = dt_agro,f_code_muni = s_code_munis_rtp)
                      ))
  if(has_bacia == FALSE){qd_27 <- qd_27[!(Local %like% "Bacia")]}
  
  top_agro <- function(f_dt,f_code_muni,name_local){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"produto_das_lavouras_temporarias_e_permanentes","produto") %>% 
      .[variavel %in% "Valor da produção",] %>% 
      .[produto != "Total",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      setnames(.,"valor_total",name_local)
    return(vec)
  }
  dt_rgint <- top_agro(f_dt = dt_agro,f_code_muni = s_code_munis_rgint, name_local = "RGINT")
  dt_state <- top_agro(f_dt = dt_agro,f_code_muni = s_code_munis_state, name_local = "Estado")
  dt_bacia <- top_agro(f_dt = dt_agro,f_code_muni = s_code_munis_bacia, name_local = "Bacia")
  dt_rtp <- top_agro(f_dt = dt_agro,f_code_muni = s_code_munis_rtp,     name_local = "RTP")
  
  q_15 <- dt_rgint[dt_state, on = "produto"] %>% 
    .[dt_bacia, on = "produto"] %>% 
    .[dt_rtp, on = "produto"] %>% 
    .[order(RGINT,decreasing = TRUE),] %>% 
    .[1:20,] %>% 
    setnames(.,"produto","Produto") %>% 
    .[,"RGINT/Estado (%)" := my_division(RGINT,Estado)] %>% 
    .[,"RGINT/Bacia (%)" := my_division(RGINT,Bacia)] %>% 
    .[,"RGINT/RTP (%)" := my_division(RGINT,RTP)] 
  
  
  if(has_bacia == FALSE){q_15 <- q_15[,":="(Bacia = NULL,`RGINT/Bacia (%)` = NULL)]}
  
  
  top_n_area_agro <- function(n,f_dt,f_code_muni,name_local){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"produto_das_lavouras_temporarias_e_permanentes","produto") %>% 
      .[variavel %in% "Área plantada ou destinada à colheita",] %>% 
      .[produto != "Total",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:n,] %>% 
      setnames(.,"valor_total",name_local)
    return(vec)
  }
  
  dt_rgint <- top_n_area_agro(n = 5,f_dt = dt_agro,f_code_muni = s_code_munis_rgint,name_local = "RGINT")
  dt_state <- top_n_area_agro(n = 5,f_dt = dt_agro,f_code_muni = s_code_munis_state,name_local = "Estado")
  dt_bacia <- top_n_area_agro(n = 5,f_dt = dt_agro,f_code_muni = s_code_munis_bacia,name_local = "Bacia")
  dt_regiao <- top_n_area_agro(n = 5,f_dt = dt_agro,f_code_muni = s_code_munis_region,name_local = s_name_region)
  dt_rtp <- top_n_area_agro(n = 5,f_dt = dt_agro,f_code_muni = s_code_munis_rtp,name_local = "RTP")
  
  qd_29 <- dt_rgint[dt_state, on = "produto"] %>% 
    .[dt_bacia, on = "produto"] %>% 
    .[dt_rtp, on = "produto"] %>% 
    .[order(RGINT,decreasing = TRUE),] %>% 
    setnames(.,"produto","Produto") %>% 
    .[,"RGINT/Estado (%)" := my_division(RGINT,Estado)] %>% 
    .[,"RGINT/Bacia (%)" := my_division(RGINT,Bacia)] %>% 
    .[,"RGINT/RTP (%)" := my_division(RGINT,RTP)] %>% 
    .[!is.na(RGINT),]
  
  if(has_bacia == FALSE){qd_29 <- qd_29[,":="(Bacia = NULL,`RGINT/Bacia (%)` = NULL)]}
  ## silv ----
  top_silv <- function(n,f_dt,f_code_muni,name_local){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"especie_florestal","produto") %>% 
      .[produto != "Total",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:n,] %>% 
      .[!is.na(valor_total),] %>% 
      setnames(.,"valor_total",name_local)
  }
  
  dt_rgint  <- top_silv(n = 5, f_dt = dt_silv, f_code_muni = s_code_munis_rgint, name_local = "RGINT")
  dt_state  <- top_silv(n = 5, f_dt = dt_silv, f_code_muni = s_code_munis_state, name_local = "Estado")
  dt_bacia  <- top_silv(n = 5, f_dt = dt_silv, f_code_muni = s_code_munis_bacia, name_local = "Bacia")
  dt_regiao <- top_silv(n = 5, f_dt = dt_silv, f_code_muni = s_code_munis_region, name_local = "Região")
  dt_rtp    <- top_silv(n = 5,f_dt = dt_silv,f_code_muni = s_code_munis_rtp,name_local = "RTP")
  
  qd_16 <- data.table::merge.data.table(x = dt_rtp,y = dt_regiao,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_bacia,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_state,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_rgint,by = "produto",all = TRUE) %>% 
    .[order(RGINT,decreasing = TRUE),] %>% 
    setnames(.,"produto","Produto") %>% 
    .[,.SD,.SDcols = c("Produto","RGINT","Estado","Bacia","Região","RTP")] %>% 
    .[,"RGINT/Estado (%)" := my_division(RGINT,Estado)] %>% 
    .[,"RGINT/Bacia (%)" := my_division(RGINT,Bacia)] %>% 
    .[,"RGINT/Região (%)" := my_division(RGINT,`Região`)] %>% 
    .[,"RGINT/RTP (%)" := my_division(RGINT,RTP)] 
  
  qd_16[is.na(qd_16)] <- "-"
  if(has_bacia == FALSE){qd_16 <- qd_16[,":="(Bacia = NULL,`RGINT/Bacia (%)` = NULL)]}
  
  # figura 7 
  #"data_graph_p4-p5/_RGINT/tabela 5457 - Área Plantada/Colunas Empilhadas/2101_Valor Bacia Parna�ba_2020.png"
  
  ## aqui ----
  
  top_3_aqui <- function(f_dt,f_code_muni,collapse = TRUE){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"tipo_de_produto_da_aquicultura","produto") %>% 
      .[variavel %in% "Valor da produção",] %>% 
      .[produto != "Total",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:3,produto]
    if(collapse){
      vec <- paste0("(",1:3,") ",vec,collapse = ", ")
    }
    return(vec)
  }
  qd_32 <- data.table("Local" = c( paste("RGINT",s_name_rgint  )
                                   ,paste("Estado",s_name_state)
                                   ,paste("Bacia",s_name_bacia)
                                   ,paste("Região",s_name_region)
                                   ,"Região Total"),
                      "Produto" = c(
                        top_3_aqui(f_dt = dt_aqui,f_code_muni = s_code_munis_rgint),
                        top_3_aqui(f_dt = dt_aqui,f_code_muni = s_code_munis_state),
                        top_3_aqui(f_dt = dt_aqui,f_code_muni = s_code_munis_bacia),
                        top_3_aqui(f_dt = dt_aqui,f_code_muni = s_code_munis_region),
                        top_3_aqui(f_dt = dt_aqui,f_code_muni = s_code_munis_rtp)
                      ))
  if(has_bacia == FALSE){qd_32 <- qd_32[!(Local %like% "Bacia")]}
  
  top_aqui <- function(n,f_dt,f_code_muni,name_local){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"tipo_de_produto_da_aquicultura","produto") %>% 
      .[produto != "Total",] %>% 
      .[variavel %in% "Valor da produção",] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:n,] %>% 
      .[!is.na(valor_total),] %>% 
      setnames(.,"valor_total",name_local)
  }
  
  dt_rgint <- top_aqui(n = 10,f_dt = dt_aqui,f_code_muni = s_code_munis_rgint,name_local = "RGINT")
  dt_state <- top_aqui(n = 10,f_dt = dt_aqui,f_code_muni = s_code_munis_state,name_local = "Estado")
  dt_bacia <- top_aqui(n = 10,f_dt = dt_aqui,f_code_muni = s_code_munis_bacia,name_local = "Bacia")
  dt_regiao <- top_aqui(n = 10,f_dt = dt_aqui,f_code_muni = s_code_munis_region,name_local = "Região")
  dt_rtp <- top_aqui(n = 10,f_dt = dt_aqui,f_code_muni = s_code_munis_rtp,name_local = "RTP")
  
  q_17 <- data.table::merge.data.table(x = dt_rtp,y = dt_regiao,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_bacia,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_state,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_rgint,by = "produto",all = TRUE) %>% 
    .[order(RGINT,decreasing = TRUE),] %>% 
    setnames(.,"produto","Produto") %>% 
    .[,.SD,.SDcols = c("Produto","RGINT","Estado","Bacia","Região","RTP")] %>% 
    .[,"RGINT/Estado (%)" := my_division(RGINT,Estado)] %>% 
    .[,"RGINT/Bacia (%)" := my_division(RGINT,Bacia)] %>% 
    .[,"RGINT/Região (%)" := my_division(RGINT,`Região`)] %>% 
    .[,"RGINT/RTP (%)" := my_division(RGINT,RTP)] 
  
  q_17
  q_17[is.na(q_17)] <- "-"
  if(has_bacia == FALSE){q_17 <- q_17[,":="(Bacia = NULL,`RGINT/Bacia (%)` = NULL)]}
  ## reb ----
  
  top_3_reb <- function(f_dt,f_code_muni,collapse = TRUE){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"tipo_de_rebanho","produto") %>% 
      .[variavel %in% "Efetivo dos rebanhos",] %>% 
      .[!(produto %like% "total"),] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:5,produto]
    if(collapse){
      vec <- paste0("(",1:5,") ",vec,collapse = ", ")
    }
    return(vec)
  }
  qd_33 <- data.table("Local" = c( paste("RGINT",s_name_rgint  )
                                   ,paste("Estado",s_name_state)
                                   ,paste("Bacia",s_name_bacia)
                                   ,paste("Região",s_name_region)
                                   ,"Região Total"),
                      "Produto" = c(
                        top_3_reb(f_dt = dt_reb,f_code_muni = s_code_munis_rgint),
                        top_3_reb(f_dt = dt_reb,f_code_muni = s_code_munis_state),
                        top_3_reb(f_dt = dt_reb,f_code_muni = s_code_munis_bacia),
                        top_3_reb(f_dt = dt_reb,f_code_muni = s_code_munis_region),
                        top_3_reb(f_dt = dt_reb,f_code_muni = s_code_munis_rtp)
                      ))
  qd_33
  if(has_bacia == FALSE){qd_33 <- qd_33[!(Local %like% "Bacia")]}
  
  top_reb <- function(n,f_dt,f_code_muni,name_local){
    vec <- data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"tipo_de_rebanho","produto") %>% 
      .[variavel %in% "Efetivo dos rebanhos",] %>% 
      .[!(produto %like% "total"),] %>% 
      .[code_muni %in% f_code_muni,] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(produto)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:n,] %>% 
      .[!is.na(valor_total),] %>% 
      setnames(.,"valor_total",name_local)
  }
  
  dt_rgint  <- top_reb(n = 10,f_dt = dt_reb, f_code_muni = s_code_munis_rgint , name_local = "RGINT")
  dt_state  <- top_reb(n = 10,f_dt = dt_reb, f_code_muni = s_code_munis_state , name_local = "Estado")
  dt_bacia  <- top_reb(n = 10,f_dt = dt_reb, f_code_muni = s_code_munis_bacia , name_local = "Bacia")
  dt_regiao <- top_reb(n = 10,f_dt = dt_reb, f_code_muni = s_code_munis_region, name_local = "Região")
  dt_rtp    <- top_reb(n = 10,f_dt = dt_reb, f_code_muni = s_code_munis_rtp,    name_local = "RTP")
  
  q_18 <- data.table::merge.data.table(x = dt_rtp,y = dt_regiao,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_bacia,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_state,by = "produto",all = TRUE) %>% 
    data.table::merge.data.table(x = .,y = dt_rgint,by = "produto",all = TRUE) %>% 
    .[order(RGINT,decreasing = TRUE),] %>% 
    setnames(.,"produto","Produto") %>% 
    .[,.SD,.SDcols = c("Produto","RGINT","Estado","Bacia","Região","RTP")] %>% 
    .[,"RGINT/Estado (%)" := my_division(RGINT,Estado)] %>% 
    .[,"RGINT/Bacia (%)" := my_division(RGINT,Bacia)] %>% 
    .[,"RGINT/Região (%)" := my_division(RGINT,`Região`)] %>% 
    .[,"RGINT/RTP (%)" := my_division(RGINT,RTP)] 
  
  q_18
  q_18[is.na(q_18)] <- "-"
  if(has_bacia == FALSE){q_18 <- q_18[,":="(Bacia = NULL,`RGINT/Bacia (%)` = NULL)]}
  
  ## irr  ----
  
  top_irr <- function(n,f_dt,f_code_muni,name_local){
    f_dt <- copy(dt_irr)
    data.table::copy(f_dt) %>% 
      setnames(.,"municipio_codigo","code_muni") %>% 
      setnames(.,"metodo_utilizado_para_irrigacao","metodo") %>% 
      setnames(.,"grupos_de_atividade_economica","grupos") %>% 
      setnames(.,"origem_da_orientacao_tecnica_recebida","origem") %>% 
      setnames(.,"condicao_do_produtor_em_relacao_as_terras","condicao_do_produtor") %>% 
      .[variavel %in% "Número de estabelecimentos agropecuários com uso de irrigação",] %>%
      .[code_muni %in% f_code_muni,] %>%
      .[grupos != "Total",] %>%
      .[origem != "Total",] %>% 
      .[condicao_do_produtor == "Total",] %>% 
      .[,sum(valor,na.rm = TRUE),by = .(grupos,metodo)] %>% 
      .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(grupos)] %>% 
      .[valor_total > 0,] %>% 
      .[order(valor_total,decreasing = TRUE),] %>% 
      .[1:n,] %>% 
      .[!is.na(valor_total),] %>% 
      setnames(.,"valor_total",name_local)
  }
  
  ## politica rural -----
  
  f_dt <- data.table::copy(dt_irr) %>% 
    setnames(.,"municipio_codigo","code_muni") %>% 
    setnames(.,"metodo_utilizado_para_irrigacao","metodo") %>% 
    setnames(.,"grupos_de_atividade_economica","grupos") %>% 
    setnames(.,"origem_da_orientacao_tecnica_recebida","origem") %>% 
    setnames(.,"condicao_do_produtor_em_relacao_as_terras","condicao_do_produtor") %>% 
    .[variavel %in% "Número de estabelecimentos agropecuários com uso de irrigação",] %>%
    .[condicao_do_produtor == "Total",] %>% 
    .[code_muni %in% s_code_munis_rgint,] 
  
  l1 <- f_dt %>% 
    .[grupos == "Total",] %>%
    .[metodo == "Total",] %>% 
    .[tipologia == "Total",] %>% 
    .[origem != "Total",] %>% 
    .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(origem)] %>% 
    .[,prop := my_division(valor_total,sum(valor_total,na.rm = TRUE))] %>% 
    .[,label := paste0(valor_total," (",prop,"%)")] %>% 
    .[origem %like% "ecebe",label]
  l1
  l2 <-  f_dt %>% 
    .[grupos == "Total",] %>%
    .[metodo == "Total",] %>% 
    .[tipologia == "Total",] %>% 
    .[!(origem %in% c("Total","Não recebe","Recebe")),] %>% 
    .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(origem)] %>% 
    .[order(valor_total,decreasing = TRUE),origem] %>% head(1)
  l2
  l3 <-  f_dt %>% 
    .[grupos == "Total",] %>%  .[metodo != "Total",] %>% 
    .[tipologia == "Total",] %>% .[origem == "Total",] %>% 
    .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(metodo)] %>% 
    .[order(valor_total,decreasing = TRUE),metodo] %>% 
    head(5) %>% 
    paste0("(",1:5,") ",.,collapse = "; \n")
  l4 <-  f_dt %>% 
    .[grupos == "Total",] %>%  .[metodo == "Total",] %>%
    .[origem == "Total",] %>% 
    .[tipologia != "Total",] %>% 
    .[tipologia %like% "Agricultura familiar - sim",tipologia := "Agricultura familiar"] %>% 
    .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(tipologia)] %>% 
    .[,prop := my_division(valor_total,sum(valor_total,na.rm = TRUE))] %>% 
    .[,label := paste0(valor_total," (",prop,"%)")] %>% 
    .[tipologia == "Agricultura familiar",label]
  l4
  l5 <- f_dt %>% 
    .[grupos == "Total",] %>%  .[metodo == "Total",] %>%
    .[origem == "Total",] %>% 
    .[tipologia %like% "Agricultura familiar",] %>% 
    .[!(tipologia %in% c("Agricultura familiar - sim",
                         "Agricultura familiar - não",
                         "Agricultura familiar - não pronafiano",
                         "Pronamp - não")),] %>% 
    .[,list(valor_total = sum(valor,na.rm = TRUE)),by = .(tipologia)] %>% 
    .[order(valor_total,decreasing = TRUE),tipologia] %>% 
    gsub(" - não","",.) %>% gsub(" - sim","",.) %>% 
    gsub("Agricultura familiar - ","",.) %>% 
    head(1)
  
  qd34 <- data.table("Descrição" = c(
    "Número de estabelecimentos que recebem orientação:",
    "Número de estabelecimentos que não recebem orientação:",
    "Principal origem da orientação técnica:",
    "Principais tipos de irrigação:",
    "Número de estabelecimentos da agricultura familiar:",
    "Principal programa:"),
    "Destaques" = c(l1[1],l1[2],l2,l3,l4,l5))
  
  #qd34 %>% View()
  
  ## origem orient tecnica ----
  
  # INPUT Doc-----
  rmarkdown::render(
    input = "inst/rmarkdown/relatorio_agro.Rmd"
    , output_file = arquivo_resultado
    , params = list(
      # INTRO
        f_code_intermediate =   s_input
      , f_q_15              =   as.data.frame(q_15 )
      , f_qd_16             =   as.data.frame(qd_16)
      , f_q_17              =   as.data.frame(q_17 )
      , f_q_18              =   as.data.frame(q_18 )
      , f_qd_27             =   as.data.frame(qd_27)
      , f_qd_29             =   as.data.frame(qd_29)
      , f_qd_32             =   as.data.frame(qd_32)
      , f_qd_33             =   as.data.frame(qd_33)
      , f_qd_34             =   as.data.frame(qd34 ) 
    )
    ,quiet = TRUE
  )
  

  

  return(NULL)
  #
}
# ** end -----