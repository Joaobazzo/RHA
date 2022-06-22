# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)



# 2) function input data ----

function_rel_pib <- function(arquivo_resultado,s_input){ # s_input = "2101"
  
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
    
  # f1 | f2 ----
  s_name_intermediate = input_file$intermediate_region[
    code_intermediate == s_input,name_intermediate
    ][1]
  
  ### prep ----
  tmp_pop_rgi <- data.table::copy(pop_raw) %>% 
    .[ano == 2019,]
  
  tmp_pib_rgi <- data.table::copy(pib_raw) %>% 
    .[variavel != "Produto Interno Bruto a preços correntes",] %>% 
    .[variavel != "Impostos, líquidos de subsídios, sobre produtos a preços correntes",] %>% 
    #.[variavel != "Valor adicionado bruto a preços correntes total",] %>% 
    .[variavel %like% "preços correntes total", variavel := "PIB"] %>% 
    .[variavel %like% "agropecuária", variavel := "Agro."] %>% 
    .[variavel %like% "dos serviços,", variavel := "Serv."] %>% 
    .[variavel %like% "da administração,", variavel := "Adm."] %>% 
    .[variavel %like% "da indústria", variavel := "Ind."] %>% 
    .[ano == 2019,]
  
  # merge
  tmp_pib_rgi[tmp_pop_rgi
              , on = c("ano","municipio_codigo","code_intermediate")
              , ":="( population = i.valor)]
  # add state
  tmp_pib_rgi[,abbrev_state := stringr::str_sub(string = municipio
                                                ,start = nchar(municipio)-1
                                                ,end = nchar(municipio))]
  
  tmp_pib_rgi[capitais_raw,on = "abbrev_state"
              ,":=" (name_state = i.name_state,
                     code_state = i.code_state)]
  
  s_state <- unique(tmp_pib_rgi[code_intermediate == s_input,]$name_state)
  s_code_capital <- capitais_raw[name_state == s_state,code_muni]
  
  # remove NA, such as ano == 2002
  tmp_pib_rgi <- tmp_pib_rgi[!is.na(population)]
  
  tmp_pib_rgi_bind <- 
    list(
      tmp_pib_rgi[code_intermediate %in% s_input,       ] %>% 
        .[,local_id := code_imediate] %>% 
        .[,name_local := name_imediate] %>% 
        .[,local := "Região Imediata"]
      ,tmp_pib_rgi[code_intermediate %in% s_input,      ] %>% 
        .[,local_id := code_intermediate] %>% 
        .[,name_local := name_intermediate] %>% 
        .[,local := "Região Intermediária"]
      ,tmp_pib_rgi[municipio_codigo %in% s_code_capital,] %>% 
        .[,local_id := municipio_codigo] %>% 
        .[,name_local := municipio] %>% 
        .[,local := "Capital"]
      ,tmp_pib_rgi[name_state %in% s_state,] %>% 
        .[,local_id := code_state] %>% 
        .[,name_local := name_state] %>% 
        .[,local := "Estado"]
    ) %>% data.table::rbindlist()
  
  tmp_pib_local <-  data.table::dcast.data.table(
    data = tmp_pib_rgi_bind
    ,formula =   ano + local + local_id + name_local + code_intermediate + code_imediate + municipio_codigo + population~ variavel
    ,value.var = "valor"
    #,fun.aggregate = sum
  ) 
  
  # arruma colunas e calcula percentuais
  tmp_pib_cap_rgint <- data.table::copy(tmp_pib_local) %>% 
    .[,{
      
      sum_pop  <- sum(population,na.rm = TRUE)
      sum_pib  <- sum(PIB, na.rm = TRUE)
      sum_adm  <- sum(`Adm.`,na.rm = TRUE)  
      sum_agr  <- sum(`Agro.`,na.rm = TRUE) 
      sum_ind  <- sum(`Ind.`,na.rm = TRUE)  
      sum_serv <- sum(`Serv.`,na.rm = TRUE)
      
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
        PIB = format(sum_pib,dec = ",")
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
                                     ,levels = c("Região Imediata"
                                                 ,"Região Intermediária"
                                                 ,"Capital"
                                                 ,"Estado")
                                     ,labels = c("RGI"
                                                 ,"RGINT"
                                                 ,"Capital"
                                                 ,"Estado"))]
  
  ## prep  ----
  
  s_pib_rgint_num <- tmp_pib_cap_rgint[local_id == s_input
                                       ,as.numeric(PIB) * 1000]
  
  s_pib_estado_num <- tmp_pib_cap_rgint[local == "Estado"
                                        ,as.numeric(PIB) * 1000]
  
  # f4 -----
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

  
  # f5 -----
  s_pib_razao_rgint <- s_pib_rgint_num / s_pib_estado_num
  s_pib_razao_rgint <- round(100 * s_pib_razao_rgint, 1)
  s_pib_razao_rgint <- paste0(s_pib_razao_rgint,"%")
  
  s_pib_capita_rgint <- tmp_pib_cap_rgint[local_id == s_input
                                          ,pib_capita_num]
  s_pib_capita_rgint <- format(s_pib_capita_rgint * 1000
                               , dec = ","
                               , format = "d"
                               , big.mark =  "."
                               , decimal.mark = ",")

  
  # f3-----
  
  s_pib_rgint_num <- formato_bonitinho(s_pib_rgint_num)
  
  # f6 ----
  
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
    
  
  # f9 -----
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

  # f7.1 f7.4 ----
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
  
  # f12-----
  
  s_name_capital_estado <- tmp_pib_cap_rgint[ local == "Capital",name_local]
  
  s_name_estado <- tmp_pib_cap_rgint[ local == "Estado",name_local]
  
  
  ### f13  ----
  
  sq_num_muni_destaque <- tmp_pib_cap_rgint[order(local)
    ,.SD
    ,.SDcols = c("local", "local_id", "name_local", "PIB", "pp_ind", "pp_serv"
    , "pp_agr", "pp_adm", "pib_capita")
  ]
  names(sq_num_muni_destaque) <- c("Local","Cód. Local","Nome","PIB"
                                    ,"Ind.(%)","Serv.(%)","Agro.(%)"
                                    ,"Adm(%)","PIB/capita")
  
  # CHECK inputs -----
  
  s_input  
  s_name_intermediate  
  s_name_estado   
  s_pib_rgint_num  
  s_pib_rgint_str  
  s_pib_razao_rgint
  s_pib_capita_rgint
  s_class_pib_rgint_ind 
  s_class_pib_rgint_serv 
  s_class_pib_rgint_agr 
  s_class_pib_rgint_adm 
  s_class_pib_cap_rgint 
  sq_ref_pib_cap 
  sq_ref_pib    
  s_code_capital
  sq_num_muni_destaque 
  
  # Rmd DOC-----
  rmarkdown::render(
    input = "inst/rmarkdown/relatorio_pib.Rmd"
    , output_file = arquivo_resultado
    , params = list(
      f_name_intermediate       =   s_name_intermediate  
      ,f_code_intermediate      =   s_input   
      ,f_name_estado            =   s_name_estado   
      ,f_pib_2019_rgint_num     =   s_pib_rgint_num  
      ,f_pib_2019_rgint_str     =   s_pib_rgint_str  
      ,f_prop_pib_est_num       =   s_pib_razao_rgint
      ,f_pib_cap_2019_rgint     =   s_pib_capita_rgint
      ,f_class_pib_rgint_ind    =   s_class_pib_rgint_ind 
      ,f_class_pib_rgint_serv   =   s_class_pib_rgint_serv 
      ,f_class_pib_rgint_agr    =   s_class_pib_rgint_agr 
      ,f_class_pib_rgint_adm    =   s_class_pib_rgint_adm 
      ,f_class_pib_cap_rgint    =   s_class_pib_cap_rgint 
      ,fdt_referencia_pib_cap   =   sq_ref_pib_cap 
      ,fdt_referencia_pib       =   sq_ref_pib    
      ,f_name_capital_estado    =   s_code_capital
      ,fdt_num_muni_destaque    =   sq_num_muni_destaque 
    )
    ,quiet = TRUE
  )
  
  
  return(NULL)
}
# end function-----


# 3) Loop through function ------

input_file <- readr::read_rds("data/munis_list.rds")
vector_unique_code <- unique(input_file$intermediate_region$code_intermediate)
pib_raw <- readr::read_rds("data/pib_total_prep.rds")
pop_raw <- readr::read_rds("data/pop_proj_total_prep.rds")
capitais_raw <- readr::read_rds("data/capitais.rds")
i = 1
lapply(seq_along(vector_unique_code)
       ,function(i){
         
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
