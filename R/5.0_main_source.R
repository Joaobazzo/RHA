# 2) function input data ----

function_rel_pib <- function(arquivo_resultado,s_input){ # s_input = "5301"
  
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
  # INTRO -----
  # ..................................... ------
  ## f1 | f2 ----
  s_name_intermediate <- input_file$intermediate_region[
    code_intermediate == s_input,name_intermediate
  ][1]
  

  # POP -----
  # ..................................... ------
  
  formato_bonitinho_pop <- function(vetor_numerico) {
    formatC(
      vetor_numerico,
      format = "d",
      big.mark =  ".",
      decimal.mark = ","
    )
  }
  
  raw_rgint <- data.table::copy(input_file$intermediate_region)[
    code_intermediate %in% as.character(s_input)
    ,
  ] 
  
  raw_pop_censo_pj <- data.table::copy(input_file$pop_censo_br)[
    municipio_codigo %in% as.character(unique(raw_rgint$code_muni))
    ,
  ]
  
  raw_pop_2020_pj <- data.table::copy(input_file$pop_2020_br)[
    municipio_codigo %in% as.character(unique(raw_rgint$code_muni))
    ,
  ]
  
  ## fp1 -----
  s_pop_2020_rgint = formato_bonitinho_pop(
    sum(
      raw_pop_2020_pj[ano == "2019" ,"valor"]
      ,na.rm = TRUE)
  )
  
  ## fp2 ----
  my_prop_urb_2010_rgint <- data.table::copy(raw_pop_censo_pj) %>% 
    .[ano == "2010",] %>% 
    .[situacao_do_domicilio != "Total",] %>% 
    .[,sum(valor,na.rm = TRUE),by = .(situacao_do_domicilio)] %>% 
    .[,proportion := round(100 * V1/sum(V1),1)] %>% 
    .[,proportion := paste(proportion,"%")]
  
  s_prop_urb_2010_rgint <- data.table::copy(my_prop_urb_2010_rgint) %>% 
    .[situacao_do_domicilio == "Urbana",proportion] 
  
  ## fp3 ----
  s_number_muni <- data.table::copy(raw_pop_censo_pj) %>% 
    .[ano == 2010,] %>% 
    .[situacao_do_domicilio == "Total",.N]
  
  ## fp4 ----
  s_number_rgi <- data.table::uniqueN(
    raw_rgint$regiao_geografica_imediata
  )
  
  ### prep ----
  
  my_prop_urb_2010_muni <- data.table::copy(raw_pop_censo_pj) %>% 
    .[ano == "2010",] %>% 
    .[situacao_do_domicilio != "Total",] %>% 
    .[,sum(valor,na.rm = TRUE),by = .(situacao_do_domicilio,municipio_codigo)] %>% 
    .[,proportion := round(100 * V1/sum(V1),1),by = .(municipio_codigo)] %>% 
    .[situacao_do_domicilio == "Urbana",] %>% 
    .[,classificacao := data.table::fcase(
      proportion >= 00 & proportion <= 25,"Pop. < 25%"
      ,proportion > 25 & proportion <= 50,"25% < Pop. <= 50%"
      ,proportion > 50 & proportion <= 75,"50% < Pop. <= 75%"
      ,proportion > 75 & proportion <= 100,"Pop. > 75%"
    )] %>% 
    .[,.N,by = classificacao]
  
  my_prop_urb_2010_rgi <- data.table::copy(raw_pop_censo_pj) %>% 
    .[ano == "2010",] %>% 
    .[situacao_do_domicilio != "Total",] %>% 
    .[,sum(valor,na.rm = TRUE),by = .(situacao_do_domicilio,code_imediate)] %>% 
    .[,proportion := round(100 * V1/sum(V1),1),by = .(code_imediate)] %>% 
    .[situacao_do_domicilio == "Urbana",] %>% 
    .[,classificacao := data.table::fcase(
      proportion >= 00 & proportion <= 25,"Pop. < 25%"
      ,proportion > 25 & proportion <= 50,"25% < Pop. <= 50%"
      ,proportion > 50 & proportion <= 75,"50% < Pop. <= 75%"
      ,proportion > 75 & proportion <= 100,"Pop. > 75%"
    )] %>% 
    .[,.N,by = classificacao]
  
  ## fp5 ----
  
  dt_prop_muni <- data.table::copy(my_prop_urb_2010_muni) %>% 
    .[my_prop_urb_2010_rgi,on = "classificacao","N_rgi" := i.N]
  
  dt_prop_muni[is.na(N),N := 0]
  dt_prop_muni[is.na(N_rgi),N_rgi := 0]
  dt_prop_muni[,classificacao := factor(
    x = classificacao
    ,levels = c("Pop. < 25%","25% < Pop. <= 50%","50% < Pop. <= 75%","Pop. > 75%")
    ,labels = c("Pop. < 25%","25% < Pop. <= 50%","50% < Pop. <= 75%","Pop. > 75%")
  )]
  data.table::setkeyv(dt_prop_muni,cols = "classificacao")
  data.table::setnames(x = dt_prop_muni
                       ,old = c("classificacao","N","N_rgi")
                       ,new = c("População Urbana (%)"
                                ,"Número de Municípios"
                                ,"Número de Reg. Imediatas"))
  ## fp6  -----
  
  
  my_pop_rgi_2020 <- data.table::copy(input_file$pop_2020_br)  %>% 
    .[code_intermediate == s_input,] %>% 
    .[,{
      list(
        name_imediate = name_imediate[1]
        ,total_municipios = data.table::uniqueN(municipio_codigo)
        ,pop_2020 = sum(valor,na.rm = TRUE)
      )
    },by = .(code_imediate)] %>% 
    .[order(total_municipios,decreasing = TRUE)]
  
  # create sum at the bottom for RGIT
  my_pop_rgint_2020 <- data.table::copy(my_pop_rgi_2020) %>% 
    .[,lapply(.SD,sum,na.rm=TRUE)
      ,.SDcols = c("total_municipios","pop_2020")] %>% 
    .[,code_imediate := s_input] %>% 
    .[,name_imediate := paste("Total RGIT",s_name_intermediate)]
  
  # rbind
  my_bind_pop_2020 <- list(my_pop_rgi_2020,my_pop_rgint_2020) %>% 
    data.table::rbindlist(use.names = TRUE)
  
  my_pop_rgi_2010 <- data.table::copy(input_file$pop_censo_br)  %>% 
    .[code_intermediate == s_input,] %>% 
    .[ano == "2010",] %>% 
    .[situacao_do_domicilio != "Total",] %>% 
    .[,sum(valor,na.rm = TRUE)
      ,by = .(code_imediate,situacao_do_domicilio)] %>% 
    data.table::dcast(.
                      ,code_imediate ~ situacao_do_domicilio
                      ,value.var = "V1") 
  
  # create sum at the bottom
  my_pop_rgint_2010 <- data.table::copy(my_pop_rgi_2010) %>% 
    .[,lapply(.SD,sum,na.rm=TRUE),.SDcols = c("Rural","Urbana")] %>% 
    .[,code_imediate := s_input]
  
  # rbind
  my_bind_pop_2010 <- list(my_pop_rgi_2010,my_pop_rgint_2010) %>% 
    data.table::rbindlist(use.names = TRUE)
  
  my_bind_pop_2010 <- my_bind_pop_2010 %>%  
    .[,{
      prop_rural <- 100 * Rural / (Rural + Urbana)
      prop_rural <- round(prop_rural,1)
      prop_urbana <- 100 * Urbana / (Rural + Urbana)
      prop_urbana <- round(prop_urbana,1)
      list(rural = prop_rural
           ,urbana = prop_urbana)
    },by = code_imediate]
  
  # c) join pop 2010 & 2020 
  dt_pop_rgi_join <- my_bind_pop_2020[
    my_bind_pop_2010,on = "code_imediate"]
  
  dt_pop_rgi_join[,pop_2020 := formato_bonitinho_pop(pop_2020)]
  dt_pop_rgi_join[,rural := format(rural,dec = ",")]
  dt_pop_rgi_join[,urbana := format(urbana,dec = ",")]
  data.table::setnames(x = dt_pop_rgi_join
                       ,old = c("code_imediate","name_imediate"
                                ,"total_municipios","pop_2020"
                                ,"rural","urbana")
                       ,new = c("Cód.","Nome RGI"
                                ,"Nº mun.","Pop. (2020)"
                                ,"Rural (%)","Urbana (%)"))
  dt_pop_rgi_join
  
  # fp7  ------
  l1 <- c("Classificação"
          ,"Pop. Estimada (2020)"
          ,"Pop. Rural (2010)"
          ,"% Pop. Rural (2010)"
          ,"Pop. Urbana (2010)"
          ,"% Pop. Urbana (2010)")
  lbaixo <- c("Baixo"
              ,"< 8573"
              ,"< 2.482"
              ,"< 27,7"
              ,"< 4.624"
              ,"< 57,9")
  lmedio <- c("Médio"
              ,"8.573 - 15.622"
              ,"2.482 - 4.336 "
              ,"27,7 - 42,1  "
              ,"4.624 - 8.732 "
              ,"57,9 - 72,3  ")
  lalto <- c("Alto",
             " > 15.622"   
             ," > 4.336 "   
             ," > 42,1  "
             ," > 8.732 "  
             ," > 72,3  ")
  sq_ref <- do.call(rbind,list(lbaixo,lmedio,lalto))
  sq_ref <- as.data.frame(sq_ref)
  names(sq_ref) <- l1
  sq_ref
  

  # PIB -----
  # ..................................... ------
  ### prep ----
  tmp_pop_rgi <- data.table::copy(pop_raw) %>% 
    .[ano == 2019,]
  
  tmp_pib_rgi <- data.table::copy(pib_raw)
  tmp_pib_rgi[,mun := as.character(mun)]
  setnames(tmp_pib_rgi,"mun","code_muni")
  tmp_pib_rgi <- tmp_pib_rgi[,.SD,.SDcols = 
                               c("code_muni","pop_est2020","pib"
                                 ,"vab_ind","vab_agro"
                                 ,"vab_servicos","vab_publico"
                                 ,"pib_per_capita","uf"
                                 ,"nome_uf","regiao_geografica_intermediaria"
                                 ,"nome_regiao_geografica_intermediaria"
                                 , "regiao_geografica_imediata"
                                 ,"nome_regiao_geografica_imediata"
                                 ,"mesorregiao_geografica"
                                 ,"nome_mesorregiao"
                                 ,"microrregiao_geografica"
                                 ,"nome_microrregiao"
                                 )] 
  tmp_pib_rgi <- tmp_pib_rgi[df_geral, on = c("code_muni")]
  
  s_state <- unique(tmp_pib_rgi[code_intermediate == s_input,]$name_state)
  s_state_code <- unique(tmp_pib_rgi[code_intermediate == s_input,]$code_state)
  s_code_capital <- capitais_raw[code_state == s_state_code,code_muni]
  s_code_capital <- unique(s_code_capital)
  
  # remove NA, such as ano == 2002
  #tmp_pib_rgi <- tmp_pib_rgi[!is.na(population)]
  
  tmp_pib_rgi_bind <- 
    list(
      tmp_pib_rgi[regiao_geografica_intermediaria %in% s_input,       ] %>% 
        .[,local_id := code_muni] %>% 
        .[,name_local := name_muni] %>% 
        .[,local := "Município"]
      , tmp_pib_rgi[regiao_geografica_intermediaria %in% s_input,       ] %>% 
        .[,local_id := code_rgi] %>% 
        .[,name_local := name_rgi] %>% 
        .[,local := "Região Imediata"]
      ,tmp_pib_rgi[regiao_geografica_intermediaria %in% s_input,      ] %>% 
        .[,local_id := regiao_geografica_intermediaria] %>% 
        .[,name_local := nome_regiao_geografica_intermediaria] %>% 
        .[,local := "Região Intermediária"]
      ,tmp_pib_rgi[code_muni %in% s_code_capital,] %>% 
        .[,local_id := code_muni] %>% 
        .[,name_local := name_muni] %>% 
        .[,local := "Capital"]
      ,tmp_pib_rgi[name_state %in% s_state,] %>% 
        .[,local_id := code_state] %>% 
        .[,name_local := nome_uf] %>% 
        .[,local := "Estado"]
    ) %>% data.table::rbindlist()
  

  # arruma colunas e calcula percentuais
  tmp_pib_cap_rgint <- data.table::copy(tmp_pib_rgi_bind) %>% 
    .[,{
      
      sum_pop  <- sum(pop_est2020,na.rm = TRUE)
      sum_pib  <- sum(pib, na.rm = TRUE)
      sum_adm  <- sum(vab_publico,na.rm = TRUE)  
      sum_agr  <- sum(vab_agro,na.rm = TRUE) 
      sum_ind  <- sum(vab_ind,na.rm = TRUE)  
      sum_serv <- sum(vab_servicos,na.rm = TRUE)
      
      prop_adm  <- sum_adm  / sum_pib
      prop_agr  <- sum_agr  / sum_pib
      prop_ind  <- sum_ind  / sum_pib
      prop_serv <- sum_serv / sum_pib
      
      perc_adm  <- (100 * prop_adm  ) %>% round(1) %>% format(.,dec = ",")
      perc_agr  <- (100 * prop_agr  ) %>% round(1) %>% format(.,dec = ",")
      perc_ind  <- (100 * prop_ind  ) %>% round(1) %>% format(.,dec = ",")
      perc_serv <- (100 * prop_serv ) %>% round(1) %>% format(.,dec = ",")
      
      #perc_pib <- (100 * (PIB / sum_pib) ) %>% round(1) %>% format(.,dec = ",")
      
      pib_capita_num <- sum_pib / sum_pop
      pib_capita_string <- pib_capita_num %>% round(1) %>% format(.,dec = ",")
      
      list(
        #PIB = format(sum_pib,dec = ",")
        PIB = sum_pib
        #,perc_pib = perc_pib
        ,ind = format(sum_ind,dec = ",")
        ,pp_ind = perc_ind
        ,serv = format(sum_serv,dec = ",")
        ,pp_serv = perc_serv
        ,agr = format(sum_agr,dec = ",")
        ,pp_agr = perc_agr
        ,adm = format(sum_adm,dec = ",")
        ,pp_adm =  perc_adm
        
        ,pib_capita_num = pib_capita_num
        ,pib_capita = pib_capita_string
      )
    }
    ,by = .(local,local_id,name_local)]
  
  # order
  tmp_pib_cap_rgint[,local := factor(local
                                     ,levels = c("Município"
                                                 ,"Região Imediata"
                                                 ,"Região Intermediária"
                                                 ,"Capital"
                                                 ,"Estado")
                                     ,labels = c("Município"
                                                 ,"RGI"
                                                 ,"RGINT"
                                                 ,"Capital"
                                                 ,"Estado"))]
  
  tmp_pib_cap_rgint_muni <- data.table::copy(tmp_pib_cap_rgint)[local == "Município",]
  tmp_pib_cap_rgint <- tmp_pib_cap_rgint[local != "Município",]
  ## prep  ----

  s_pib_rgint_num <- tmp_pib_cap_rgint[local_id %in% s_input
                                       ,as.numeric(PIB) * 1000]

  s_pib_estado_num <- tmp_pib_cap_rgint[local == "Estado"
                                        ,as.numeric(PIB) * 1000]

  ## f4 -----
  s_pib_rgint_str <- data.table::fcase(
    # bilhoes
    nchar(s_pib_rgint_num) > 9
    , sprintf("%s bilhões"
              ,formato_bonitinho(s_pib_rgint_num/10^9))
    # milhoes
    , nchar(s_pib_rgint_num) <= 9 &  nchar(s_pib_rgint_num) > 6
    , sprintf("%s milhões"
              ,formato_bonitinho(s_pib_rgint_num/10^6))
    # mil
    , nchar(s_pib_rgint_num) <= 6 & nchar(s_pib_rgint_num) > 3
    , sprintf("%s mil"
              ,formato_bonitinho(s_pib_rgint_num/10^3))
  )
  
  
  ## f5 -----
  s_pib_razao_rgint <- s_pib_rgint_num / s_pib_estado_num
  s_pib_razao_rgint <- round(100 * s_pib_razao_rgint, 1)
  s_pib_razao_rgint <- paste0(s_pib_razao_rgint,"%")
  
  s_pib_capita_rgint <- tmp_pib_cap_rgint[local_id %in% s_input
                                          ,pib_capita_num]
  s_pib_capita_rgint <- format(s_pib_capita_rgint * 1000
                               , dec = ","
                               , format = "d"
                               , big.mark =  "."
                               , decimal.mark = ",")
  
  
  ## f3-----
  
  s_pib_rgint_num <- formato_bonitinho(s_pib_rgint_num)
  s_pib_estado_num <- format(s_pib_estado_num,decimal.mark = ",")
  ## f6 ----
  
  s_pib_capita_rgint <- tmp_pib_cap_rgint[
    local_id == s_input,pib_capita]
  
  ## prep -----
  
  # f8 -----
  pib_cap_rgint_num <- tmp_pib_cap_rgint[
    local_id == s_input,pib_capita_num]
  s_class_pib_cap_rgint = fcase(pib_cap_rgint_num < 13.849,"Baixa",
                                pib_cap_rgint_num >= 13.849 & 
                                  pib_cap_rgint_num < 21.288, "Média",
                                pib_cap_rgint_num > 21.288,"Alta")
  

  ## f9 -----
  l1 <- c("Classificação"
          ,"PIB"
          ,"PIB capita")
  lBaixa <- c("Baixa","< 133.094.200","< 13.849")
  lmedio <- c("Média", "133.094.200 - 254.476.800","13.849 - 21.288")
  lAlta <- c("Alta","> 254.4768.00","> 21.288")
  
  sq_ref_pib_cap <- do.call(rbind,list(lBaixa,lmedio,lAlta))
  sq_ref_pib_cap <- as.data.frame(sq_ref_pib_cap)
  names(sq_ref_pib_cap) <- l1
  
  # f10
  l1 <- c("Classificação"
          ,"VAB Indústria"
          ,"VAB Serviços"
          ,"VAB Agro."
          ,"VAB Adm. Púb.")
  lBaixa <- c("Baixa"
              ,"< 7.768.600"
              ,"< 36.184.600"
              ,"< 17.935.800"
              ,"< 39.721.000")
  lmedio <- c("Média"
              , "7.768.600 - 22.320.400"
              ,"36.184.600 - 78.786.400"
              ,"17.935.800 - 35.276.400"
              ,"39.721.000 - 68.173.600")
  lAlta <-  c("Alta"
              ,"> 22.320.400"
              ,"> 78.786.400"
              ,"> 35.276.400"
              ,"> 68.173.600")
  
  sq_ref_pib <- do.call(rbind,list(lBaixa,lmedio,lAlta))
  sq_ref_pib <- as.data.frame(sq_ref_pib)
  names(sq_ref_pib) <- l1
  
  ## f7.1 f7.4 ----
  pib_rgint_num_ind <- tmp_pib_cap_rgint[ local_id == s_input,ind] %>% as.numeric()
  pib_rgint_num_ser <- tmp_pib_cap_rgint[ local_id == s_input,serv] %>% as.numeric()
  pib_rgint_num_agr <- tmp_pib_cap_rgint[ local_id == s_input,agr]  %>% as.numeric()
  pib_rgint_num_adm <- tmp_pib_cap_rgint[ local_id == s_input,adm]  %>% as.numeric()
  pib_rgint_num_ind <- pib_rgint_num_ind * 1000
  pib_rgint_num_ser <- pib_rgint_num_ser * 1000
  pib_rgint_num_agr <- pib_rgint_num_agr * 1000
  pib_rgint_num_adm <- pib_rgint_num_adm * 1000
  
  s_class_pib_rgint_ind = fcase(pib_rgint_num_ind < 7768600,"Baixa",
                                pib_rgint_num_ind >= 7768600 & 
                                  pib_rgint_num_ind < 22320400, "Média",
                                pib_rgint_num_ind > 22320400,"Alta")
  s_class_pib_rgint_serv = fcase(pib_rgint_num_ser < 36184600,"Baixa",
                                 pib_rgint_num_ser >= 36184600 & 
                                   pib_rgint_num_ser < 78786400, "Média",
                                 pib_rgint_num_ser > 78786400,"Alta")
  s_class_pib_rgint_agr = fcase(pib_rgint_num_agr < 17935800,"Baixa",
                                pib_rgint_num_agr >= 17935800 & 
                                  pib_rgint_num_agr < 35276400, "Média",
                                pib_rgint_num_agr > 35276400,"Alta")
  s_class_pib_rgint_adm = fcase(pib_rgint_num_adm < 39721000,"Baixa",
                                pib_rgint_num_adm >= 39721000 & 
                                  pib_rgint_num_adm < 68173600, "Média",
                                pib_rgint_num_adm > 68173600,"Alta")
  s_class_pib_rgint_ind
  s_class_pib_rgint_serv
  s_class_pib_rgint_agr
  s_class_pib_rgint_adm
  
  ## f12-----
  
  s_name_capital_estado <- tmp_pib_cap_rgint[ local == "Capital",name_local]
  s_name_estado <- tmp_pib_cap_rgint[ local == "Estado",name_local]
  s_abbrev_estado <- input_file$intermediate_region %>% 
    .[name_state == s_name_estado,unique(abbrev_state)]
  
  
  ### f13  ----
  
  sq_num_muni_destaque <- tmp_pib_cap_rgint[order(local)
                                            ,.SD
                                            ,.SDcols = c("local", "local_id", "name_local", "PIB", "pp_ind", "pp_serv"
                                                         , "pp_agr", "pp_adm", "pib_capita")
  ]
  names(sq_num_muni_destaque) <- c("Local","Cód. Local","Nome","PIB"
                                   ,"Ind.(%)","Serv.(%)","Agro.(%)"
                                   ,"Adm(%)","PIB/capita")
  
  ## f14 -----
  s_num_muni_rgint <- tmp_pib_rgi %>% 
    .[code_intermediate == s_input,code_muni] %>% 
    uniqueN()
  
  # IVS / IDHM  ------
  # ..................................... ------
  
  ## f15 -----
  sq_num_renda_din <- data.table::copy(ivs_raw) %>% 
    .[code_intermediate == s_input,] %>% 
    .[,.N,by = .(renda_status,dinamismo_status)]
  
  names(sq_num_renda_din) <- c("Renda","Dinamismo","Número")
  
  ## f16 ----
  c1 <- c("Classificação"
          ,"Muito Baixo"
          ,"Baixo"
          ,"Médio"
          ,"Alto"
          ,"Muito Alto")
  c2 <- c("IDH-M"
          ,"< 0.499"
          ,"0.500 - 0.599"
          ,"0.600 - 0.699"
          ,"0.700 - 0.799"
          ,"0.800 - 1.000")
  c3 <- c("IVS"
          , "< 0.200"
          ,"0.201 - 0.300"
          ,"0.301 - 0.400"
          ,"0.401 - 0.500"
          ,"0.501 - 1.00")
  
  sq_ivs_idh <- do.call(cbind,list(c1,c2,c3))
  sq_ivs_idh <- as.data.frame(sq_ivs_idh[-1,])
  names(sq_ivs_idh) <- do.call(cbind,list(c1,c2,c3))[1,]
  
  class_indice <- function(indice,tipo){
    if(tipo == "IDH"){
      out <- fcase(indice < 0.499,"Muito Baixo",
                   indice >= 0.500 & 
                     indice < 0.599, "Baixo",
                   indice >= 0.600 & 
                     indice < 0.699, "Médio",
                   indice >= 0.700 & 
                     indice < 0.799, "Alto",
                   indice > 0.800,"Muito Alto")
    }else{
      out <- fcase(indice < 0.200,"Muito Baixa",
                   indice >= 0.201 & 
                     indice < 0.300, "Baixa",
                   indice >= 0.301 & 
                     indice < 0.400, "Média",
                   indice >= 0.401 & 
                     indice < 0.500, "Alta",
                   indice > 0.501,"Muito Alta")
    }
    return(out)
  }
  ## f17-f28 -----

  tmp_idhm <- data.table::copy(ivs_raw) %>% 
    .[code_intermediate == s_input,] %>% 
    .[,idhm := as.numeric(idhm)] %>% 
    .[,ivs := as.numeric(ivs)] %>% 
    .[idhm == min(idhm) | idhm == max(idhm) | 
        ivs == min(ivs) | ivs == max(ivs),]
  
  
  s_min_muni_idhm_num   <- tmp_idhm[idhm == min(idhm),idhm]
  s_max_muni_idhm_num   <- tmp_idhm[idhm == max(idhm),idhm]
  s_min_muni_idhm_nme   <- tmp_idhm[idhm == min(idhm),paste0(name_muni," - ",uf_sigla)]
  s_max_muni_idhm_nme   <- tmp_idhm[idhm == max(idhm),paste0(name_muni," - ",uf_sigla)]
  
  s_min_muni_ivs_num    <- tmp_idhm[ivs == min(ivs),ivs]
  s_max_muni_ivs_num    <- tmp_idhm[ivs == max(ivs),ivs]
  s_min_muni_ivs_nme    <- tmp_idhm[ivs == min(ivs),paste0(name_muni," - ",uf_sigla)]
  s_max_muni_ivs_nme    <- tmp_idhm[ivs == max(ivs),paste0(name_muni," - ",uf_sigla)]
  
  s_class_max_muni_idhm <- class_indice(s_max_muni_idhm_num,"IDH")
  s_class_min_muni_idhm <- class_indice(s_min_muni_idhm_num,"IDH")
  s_class_max_muni_ivs  <- class_indice(s_max_muni_ivs_num,"IVS")
  s_class_min_muni_ivs  <- class_indice(s_min_muni_ivs_num,"IVS")
  
  
  ## f29-----
  
  tmp_idhm <- data.table::copy(ivs_raw) %>% 
    .[code_intermediate == s_input,]
  
  tmp_pop_2020 <- data.table::copy(pop_raw) %>% 
    .[code_intermediate == s_input,] %>% 
    .[ano == "2020"]
  
  tmp_pop_2010 <- data.table::copy(input_file$pop_censo_br) %>% 
    .[code_intermediate == s_input,] %>% 
    .[ano == "2010" & situacao_do_domicilio != "Total"] %>% 
    data.table::dcast(.,formula = municipio_codigo + municipio ~ situacao_do_domicilio
                      ,value.var = "valor") %>% 
    .[,prop_rural := Rural / (Rural + Urbana)] %>% 
    .[,prop_urbana := Urbana / (Rural + Urbana)] %>% 
    .[,prop_rural := round(100 * prop_rural, 1)] %>% 
    .[,prop_urbana := round(100 * prop_urbana, 1)]
  
  tmp_pop <- tmp_pop_2020[tmp_pop_2010,on = "municipio_codigo"
                          ,":="(
                            Rural = i.Rural
                            ,Urbana = i.Urbana
                            ,prop_rural = i.prop_rural
                            ,prop_urbana = i.prop_urbana
                          )]
  tmp_f27 <- data.table::merge.data.table(x = tmp_idhm
                                          ,y = tmp_pop
                                          ,by.x = "codigo_ibge"
                                          ,by.y = "municipio_codigo"
  )
  tmp_f27 <- tmp_f27[,.SD,.SDcols = c( "codigo_ibge"
                                       , "name_muni"
                                       , "Urbana"
                                       , "Rural"
                                       , "prop_urbana"
                                       , "prop_rural"
                                       , "valor"
                                       , "ivs"
                                       , "idhm"
                                       , "renda_status"
                                       , "dinamismo_status")]
  tmp_f27[,name_muni := sprintf("%s (%s)",name_muni,codigo_ibge)]
  tmp_f27[,codigo_ibge := NULL]
  names(tmp_f27) <- c("Município (Cód.)"
                      ,"Pop. Urbana","Pop. Rural"
                      ,"% Urbana","% Rural"
                      ,"Pop. (2020)","IVS","IDH"
                      ,"Renda","Dinamismo")
  

  # Destaques Economia----
  # ..................................... ------
  ## f30 -----
  
  tmp_pib <- data.table::copy(tmp_pib_cap_rgint_muni)
  tmp_pib[,PIB_num := as.numeric(PIB)]
  tmp_pib[,PIB := formato_bonitinho(PIB_num),by = .(local_id)]
  tmp_pib[,single_name := paste0(name_local," (",local_id,")")]
  tmp_pib[,destaque_vab := max(pp_ind,pp_serv,pp_adm),by = single_name]
  tmp_pib[,destaque_vab := fcase(destaque_vab == pp_ind,paste0(destaque_vab, "% Indústria")
                                 ,destaque_vab == pp_serv,paste0(destaque_vab, "% Serviços")
                                 ,destaque_vab == pp_agr,paste0(destaque_vab, "% Agropecuária")
                                 ,destaque_vab == pp_adm,paste0(destaque_vab, "% Administração"))]
  head(tmp_pib)
  
  tmp_pib[tmp_idhm,on = c("local_id" = "codigo_ibge"),tipologia_pndr := i.tipologia_sub_regional]
  
  sq_big_pib <- data.table::copy(tmp_pib)[order(PIB_num,decreasing = TRUE),][1:5]
  sq_big_pib <- sq_big_pib[,.SD,.SDcols = c("single_name","PIB","destaque_vab","tipologia_pndr")]
  names(sq_big_pib) <- c("Município","PIB (mil R$)","Destaque VAB (%)","Tipologia PNDR")
  sq_big_pib
  ## f31 ----
  sq_big_pib_cap <- data.table::copy(tmp_pib)[order(pib_capita_num,decreasing = TRUE),][1:5]
  sq_big_pib_cap <- sq_big_pib_cap[,.SD,.SDcols = c("single_name","pib_capita","destaque_vab","tipologia_pndr")]
  names(sq_big_pib_cap) <- c("Município","PIB per capita (mil R$/hab.)","Destaque VAB (%)","Tipologia PNDR")
  sq_big_pib_cap
  
  ## f32 -----
  tmp_muni_codes <- data.table::copy(tmp_pib)
  tmp_muni_codes <- tmp_muni_codes[muni_area,on = c("local_id" = "CD_MUN")
                                   ,area := i.AR_MUN_2021]
  tmp_muni_codes[,PIB_area_num := PIB_num /area]
  tmp_muni_codes[,PIB_area := formato_bonitinho(PIB_area_num), by = .(local_id)]
  sq_big_pib_area <- tmp_muni_codes[order(PIB_area_num,decreasing = TRUE),][1:5]
  sq_big_pib_area <- sq_big_pib_area[,.SD,.SDcols = c("single_name","PIB_area"
                                                      ,"destaque_vab","tipologia_pndr")]
  names(sq_big_pib_area) <- c("Município","PIB/área (mil R$/km²)","Destaque VAB (%)","Tipologia PNDR")
  sq_big_pib_area
  
  # 
  ## f33 -----
  sq_top_vab <- melt(tmp_pib
                     ,id.vars = "single_name"
                     ,measure.vars = c("pp_ind","pp_serv","pp_agr","pp_adm")
                     ,value.name = "perc"
                     ,variable.name = "VAB"
  )
  
  data.table::setkeyv(x = sq_top_vab
                      ,cols = c("perc","VAB")
                      ,physical = T)
  Num_max <- fifelse(nrow(tmp_pib) < 5,nrow(tmp_pib),5)
  sq_top_vab <- sq_top_vab[,.SD[.N:(.N+1-Num_max)],by = VAB]
  sq_top_vab[,perc := paste0(perc,"%")]
  sq_top_vab <- do.call(cbind,args = list(
    sq_top_vab[VAB == "pp_adm" ,2:3],
    sq_top_vab[VAB == "pp_serv",2:3],
    sq_top_vab[VAB == "pp_agr" ,2:3],
    sq_top_vab[VAB == "pp_ind" ,2:3]
  ))
  
  names(sq_top_vab) <- c( "VAB Administração","%    "
                          ,"VAB Serviços"    ,"%   "
                          ,"VAB Agropecuária","%  "
                          ,"VAB Indústria"   ,"% ")

  # Emprego -----
  # ..................................... ------
  # get files
  my_division <- function(a,b){round(100 * a / b , 2)}
  tmp_cnae <- data.table::copy(cnae_raw$raw_CNAE) %>% 
    .[regiao_intermediaria %in% s_input,]
  tmp_cnae_capital <- data.table::copy(cnae_raw$raw_CNAE) %>% 
    .[codigo_municipio %in% s_code_capital,]
  tmp_all <- data.table::copy(cnae_raw$resumo)
  workCols = c("soma_remun", "soma_horas", "contagem")
  s_bacia = unique(tmp_cnae[!is.na(bacia),bacia])[1]
  bacia_exists <- !is.na(s_bacia)
  if(bacia_exists == FALSE){
    s_bacia <- "PISF"
  }
  
  # prep RGINT
  tmp_jobs <-  data.table::copy(tmp_cnae) %>% 
    .[,lapply(.SD,sum,na.rm = TRUE)
      , by = .(nome_grupo,cnae20,nome_classe)
      , .SDcols = workCols]
  for(i in workCols){
    tmp_jobs[,paste0("pp_int_",i) := my_division(get(i),sum(get(i)))] 
  }

  ## f34------
  sdt_jobs_total <- data.table::copy(tmp_jobs) %>% 
    .[order(contagem,decreasing = TRUE),] %>% 
    .[,.SD[1:10],.SDcols = c("nome_grupo","cnae20","nome_classe","contagem","pp_int_contagem")]
  
  # quadro 1

  sdt_jobs_total <- list(sdt_jobs_total
                         ,data.frame(
                           nome_grupo = "Total RGINT"
                           ,contagem = tmp_all[local_id == s_input,contagem]
                           ,pp_int_contagem = my_division(
                             tmp_all[local_id == s_input,contagem]
                             ,tmp_all[local_id == s_abbrev_estado,contagem])
                         )
                         ,data.frame(
                           nome_grupo = paste0(s_name_capital_estado," (Capital)")
                           ,contagem = tmp_cnae_capital[codigo_municipio == s_code_capital,sum(contagem)] 
                           ,pp_int_contagem = my_division(
                             tmp_cnae_capital[codigo_municipio==s_code_capital,sum(contagem)]
                             ,tmp_all[local_id == s_abbrev_estado,contagem])
                         )
                         ,data.frame(
                           nome_grupo = paste0(s_name_estado," (Estado)")
                           , contagem = tmp_all[local_id == s_abbrev_estado,contagem]
                           ,pp_int_contagem = my_division(
                             tmp_all[local_id == s_abbrev_estado,contagem]
                             ,tmp_all[local_id == "Brasil",contagem])
                         )
                         ,data.frame(
                           nome_grupo = paste0(s_bacia," (Bacia)")
                           , contagem = tmp_all[local_id == s_bacia,contagem]
                           ,pp_int_contagem = my_division(
                             tmp_all[local_id == s_bacia,contagem]
                             ,tmp_all[local_id == "Brasil",contagem])
                         )
                         ,data.frame(
                           nome_grupo = "Região Total do Projeto"
                           , contagem = tmp_all[local_id == "Região Total do Projeto",contagem]
                           ,pp_int_contagem = my_division(
                             tmp_all[local_id == "Região Total do Projeto",contagem]
                             ,tmp_all[local_id == "Brasil",contagem])
                         )
  ) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)

  if(bacia_exists == FALSE){
    sdt_jobs_total <- sdt_jobs_total[!(nome_grupo %like% "(Bacia)"),]
  }
  sdt_jobs_total[is.na(sdt_jobs_total)] <- "-"
  
  names(sdt_jobs_total) <- c("Grupo","CNAE","Classe","Vínculos","% RGINT")
  ## f35------
  sdt_rem_total <- data.table::copy(tmp_jobs) %>% 
    .[order(soma_remun,decreasing = TRUE)] %>% 
    .[,.SD[1:10],.SDcols = c("nome_grupo","cnae20","nome_classe","soma_remun","pp_int_soma_remun")]

  # quadro 2
  sdt_rem_total <- list(sdt_rem_total
                        ,data.frame(
                          nome_grupo = "Total RGINT"
                          ,soma_remun = tmp_all[local_id == s_input,soma_remun]
                          ,pp_int_soma_remun = my_division(
                            tmp_all[local_id == s_input,soma_remun]
                            ,tmp_all[local_id == s_abbrev_estado,soma_remun])
                        )
                        ,data.frame(
                          nome_grupo = paste0(s_name_capital_estado," (Capital)")
                          ,soma_remun = tmp_cnae[codigo_municipio==s_code_capital,sum(soma_remun)]
                          ,pp_int_soma_remun = my_division(
                            tmp_cnae[codigo_municipio==s_code_capital,sum(soma_remun)]
                            ,tmp_all[local_id == s_abbrev_estado,soma_remun])
                        )
                        ,data.frame(
                          nome_grupo = paste0(s_name_estado," (Estado)")
                          , soma_remun = tmp_all[local_id == s_abbrev_estado,soma_remun]
                          ,pp_int_soma_remun = my_division(
                            tmp_all[local_id == s_abbrev_estado,soma_remun]
                            ,tmp_all[local_id == "Brasil",soma_remun])
                        )
                        ,data.frame(
                          nome_grupo = paste0(s_bacia," (Bacia)")
                          , soma_remun = tmp_all[local_id == s_bacia,soma_remun]
                          ,pp_int_soma_remun = my_division(
                            tmp_all[local_id == s_bacia,soma_remun]
                            ,tmp_all[local_id == "Brasil",soma_remun])
                        )
                        ,data.frame(
                          nome_grupo = "Região Total do Projeto"
                          , soma_remun = tmp_all[local_id == "Região Total do Projeto",soma_remun]
                          ,pp_int_soma_remun = my_division(
                            tmp_all[local_id == "Região Total do Projeto",soma_remun]
                            ,tmp_all[local_id == "Brasil",soma_remun])
                        )
  ) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
  
  if(bacia_exists == FALSE){
    sdt_rem_total <- sdt_rem_total[!(nome_grupo %like% "(Bacia)"),]
  }
  sdt_rem_total[is.na(sdt_rem_total)] <- "-"
  
  names(sdt_rem_total) <- c("Grupo","CNAE","Classe","Remuneração","% RGINT")
  #fdt_rem_total %>% View()
  
  ### prep ------

  fdt_num_total <- data.table::copy(tmp_cnae) %>% 
    .[
      ,{
        list(
          "total_jobs" = sum(contagem)
          ,"N_empresas" = .N
        )
      }
      ,by = .(cnae20,nome_classe,nome_grupo,tamanho_vinculo)] %>% 
    data.table::setorder(.,-total_jobs,-tamanho_vinculo,N_empresas)
  
  fdt_num_total[
    ,":="(pp_total_jobs = my_division(total_jobs,sum(total_jobs))
          ,pp_N_empresas = my_division(N_empresas,sum(N_empresas)))
    , by = .(tamanho_vinculo)]
  
  ## f36 - f41 ----
  num_jobs_by_vinculo <- function(i){
    int_vin <- data.table::copy( fdt_num_total) %>% 
      .[tamanho_vinculo == i,] %>% 
      .[,tamanho_vinculo := NULL] 
    
    int_vin <- list(
      head(int_vin,10)
      ,data.frame("cnae20" = "RGINT"
                  ,"total_jobs" = int_vin$total_jobs %>% sum()
                  ,"pp_total_jobs" = int_vin$pp_total_jobs %>% sum()
                  ,"N_empresas" = int_vin$N_empresas %>% sum()
                  ,"pp_N_empresas" = int_vin$pp_N_empresas %>% sum())
    ) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
    
    int_vin[is.na(int_vin)] <- "-"
    names(int_vin) <- c("Grupo","CNAE","Classe","Empregos","Empresas","% Empregos RGINT","% Empresas RGINT")
    return(int_vin)
  }
  sdt_num_total_vinculo500     <- num_jobs_by_vinculo("+ de 500 vínculos")
  sdt_num_total_vinculo100_499 <- num_jobs_by_vinculo("de 100 a 499 vínculos")
  sdt_num_total_vinculo50_99   <- num_jobs_by_vinculo("de 50 a 99 vínculos")
  sdt_num_total_vinculo20_49   <- num_jobs_by_vinculo("de 20 a 49 vínculos")
  sdt_num_total_vinculo10_19   <- num_jobs_by_vinculo("de 10 a 19 vínculos")
  sdt_num_total_vinculo1_9     <- num_jobs_by_vinculo("de 1 a 9 vínculos")
  
  
  
  # CTI ----
  # ..................................... ------
  #quadro_31 <- openxlsx::read.xlsx(
  #    xlsxFile = sprintf("data/dados_maira/%s.xlsx",s_input)
  #    ,sheet = "Quadro 31"
  #    ,colNames = FALSE
  #    ,fillMergedCells = FALSE
  #) %>% setDT()
#
  #quadro_31

  # INPUT Doc-----
  # ..................................... ------
  rmarkdown::render(
    input = "inst/rmarkdown/relatorio_pib.Rmd"
    , output_file = arquivo_resultado
    , params = list(
      # INTRO
      f_name_intermediate           =   s_name_intermediate     # f1      
      ,f_code_intermediate          =   s_input                 # f2  
      # POP
      ,f_pop_2020_rgint             =   s_pop_2020_rgint        # fp1          
      ,f_prop_urb_2010_rgint        =   s_prop_urb_2010_rgint   # fp2            
      ,f_number_muni                =   s_number_muni           # fp3       
      ,f_number_rgi                 =   s_number_rgi            # fp4      
      ,fdt_prop_muni                =   dt_prop_muni            # fp5      
      ,fdt_pop_rgi_sit              =   dt_pop_rgi_join         # fp6         
      ,fdt_quadro_ref               =   sq_ref                  # fp7
      # PIB
      ,f_name_estado                =   s_name_estado           # f13      
      ,f_pib_2019_rgint_num         =   s_pib_rgint_num         # f3          
      ,f_pib_2019_rgint_str         =   s_pib_rgint_str         # f4          
      ,f_prop_pib_est_num           =   s_pib_razao_rgint       # f5       
      ,f_pib_cap_2019_rgint         =   s_pib_capita_rgint      # f6          
      ,f_class_pib_rgint_ind        =   s_class_pib_rgint_ind   # f7.1         
      ,f_class_pib_rgint_serv       =   s_class_pib_rgint_serv  # f7.2           
      ,f_class_pib_rgint_agr        =   s_class_pib_rgint_agr   # f7.3        
      ,f_class_pib_rgint_adm        =   s_class_pib_rgint_adm   # f7.4          
      ,f_class_pib_cap_rgint        =   s_class_pib_cap_rgint   # f8             
      ,fdt_referencia_pib_cap       =   sq_ref_pib_cap          # f9      
      ,fdt_referencia_pib           =   sq_ref_pib              # f10    
      ,f_name_capital_estado        =   s_code_capital          # f12    
      ,fdt_num_muni_destaque        =   sq_num_muni_destaque    # f13           
      # PNDR 
      ,f_num_muni_rgint             =   s_num_muni_rgint        # f14
      ,fdt_num_renda_din            =   sq_num_renda_din        # f15
      ,fdt_ivs_idh                  =   sq_ivs_idh              # f16
      ,f_min_muni_idhm_num          =   s_min_muni_idhm_num     # f17    
      ,f_max_muni_idhm_num          =   s_max_muni_idhm_num     # f18    
      ,f_min_muni_idhm_nme          =   s_min_muni_idhm_nme     # f19    
      ,f_max_muni_idhm_nme          =   s_max_muni_idhm_nme     # f20  
      ,f_min_muni_ivs_num           =   s_min_muni_ivs_num      # f21    
      ,f_max_muni_ivs_num           =   s_max_muni_ivs_num      # f22    
      ,f_min_muni_ivs_nme           =   s_min_muni_ivs_nme      # f23    
      ,f_max_muni_ivs_nme           =   s_max_muni_ivs_nme      # f24   
      ,f_class_max_muni_idhm        =   s_class_max_muni_idhm   # f25    
      ,f_class_min_muni_idhm        =   s_class_min_muni_idhm   # f26    
      ,f_class_max_muni_ivs         =   s_class_max_muni_ivs    # f27    
      ,f_class_min_muni_ivs         =   s_class_min_muni_ivs    # f28   
      ,fdt_muni_idh_ivs             =   tmp_f27                 # f29
      # destaques
      ,fdt_big_pib                  =   sq_big_pib                # f30       
      ,fdt_big_pib_cap              =   sq_big_pib_cap            # f31     
      ,fdt_big_pib_area             =   sq_big_pib_area           # f32        
      ,fdt_top_vab                  =   sq_top_vab                # f33   
      # empregos
      ,fdt_jobs_total               =   sdt_jobs_total               # f34  
      ,fdt_rem_total                =   sdt_rem_total                # f35   
      ,fdt_num_total_vinculo500     =   sdt_num_total_vinculo500     # f36      
      ,fdt_num_total_vinculo100_499 =   sdt_num_total_vinculo100_499 # f37    
      ,fdt_num_total_vinculo50_99   =   sdt_num_total_vinculo50_99   # f38        
      ,fdt_num_total_vinculo20_49   =   sdt_num_total_vinculo20_49   # f39       
      ,fdt_num_total_vinculo10_19   =   sdt_num_total_vinculo10_19   # f40     
      ,fdt_num_total_vinculo1_9     =   sdt_num_total_vinculo1_9     # f41      
    )
    ,quiet = TRUE
  )
  
  
  return(NULL)
}
# ** end -----