# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)



# 2) function input data ----

function_rel_pop <- function(arquivo_resultado,s_input){
  
  s_input = "2101"
  
  formato_bonitinho <- function(vetor_numerico) {
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
  
  raw_pop_censo_pj <- data.table::copy(input_file$pop_censo_pj)[
    municipio_codigo %in% as.character(unique(raw_rgint$code_muni))
    ,
  ]
  
  raw_pop_2020_pj <- data.table::copy(input_file$pop_2020_pj)[
    municipio_codigo %in% as.character(unique(raw_rgint$code_muni))
    ,
  ]
  
  raw_pib <- data.table::copy(pib_raw)[
    municipio_codigo %in% as.character(unique(raw_rgint$code_muni))
    ,
  ]
  
  # 3) Argument data -----
  ## p1) Name Reg. intermediate----
  s_name_intermediate = unique(raw_rgint$name_intermediate)
  
  
  ## p2) Pib Reg. intermediate 2019-----
  tmp_pib_2019_rgint <- sum(
    raw_pib[
      ano == "2019" & variavel == "Produto Interno Bruto a preços correntes"
      ,"valor"]
    , na.rm = TRUE
  )
  tmp_pib_2019_rgint <- tmp_pib_2019_rgint * 1000
  
  s_pib_2019_rgint <- data.table::fcase(
    nchar(tmp_pib_2019_rgint) > 9
    , sprintf("%s bilhões",format(round(tmp_pib_2019_rgint/10^9,1),dec = ","))
    , nchar(tmp_pib_2019_rgint) <= 9 & nchar(tmp_pib_2019_rgint) > 6
    , sprintf("%s milhões",format(round(tmp_pib_2019_rgint/10^6,1),dec = ","))
    ,  nchar(tmp_pib_2019_rgint) <= 6 & nchar(tmp_pib_2019_rgint) > 3
    , sprintf("%s mil"    ,format(round(tmp_pib_2019_rgint/10^3,1),dec = ","))
  )
  
  s_pib_2019_rgint
  ## p3) Pib estado ----
  s_estado_name_abbrev <- unique(raw_rgint$abbrev_state)
  s_estado_name <- unique(raw_rgint$name_state)
  
  
  tmp_pib_2019_estado <-  sum(
    data.table::copy(pib_raw) %>% 
      .[ano == "2019" & 
          variavel == "Produto Interno Bruto a preços correntes",] %>% 
      .[municipio %like% sprintf(" - %s",s_estado_name_abbrev),] %>% 
      .[,"valor"]
    , na.rm = TRUE
  )
  tmp_pib_2019_estado <- tmp_pib_2019_estado * 1000
  
  
  s_percent_pib_rgint <- 100 * tmp_pib_2019_rgint / tmp_pib_2019_estado
  s_percent_pib_rgint <- round(s_percent_pib_rgint,1) %>% 
    format(.,dec = ",") %>% paste0(.,"%")
  s_percent_pib_rgint
  
  
  ## p4) Pib per capita----
  s_pib_rgint <- data.table::copy(raw_pop_2020_pj) %>% 
    .[]
  
  ## p5) pib munis in rgint ----
  s_pib_muni <- data.table::copy(my_pib_rgint) %>% 
    .[ano == 2019,]
  
  ## p6) pib RGI in RGINT -----
  s_number_rgi_above_rgint <- data.table::copy(my_pib_rgint)
  
  ## p7) VAB ----
  ### a) VAB | 2019 ----
  
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
  
  ### c) Reg. Imediata | 2010----
  
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
  
  ### d) Join Muni | Cod. Imed. | 2010 ----
  
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
  ## P6) Pop. Total | code_imediate ----
  ### a) code_imediate 2020 -----
  
  
  my_pop_rgi_2020 <- data.table::copy(input_file$pop_2020_pj)  %>% 
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
  
  ### b) code_imediate 2010 -----
  
  my_pop_rgi_2010 <- data.table::copy(input_file$pop_censo_pj)  %>% 
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
  
  ### c) join pop 2010 & 2020 ----
  dt_pop_rgi_join <- my_bind_pop_2020[
    my_bind_pop_2010,on = "code_imediate"]
  
  dt_pop_rgi_join[,pop_2020 := formato_bonitinho(pop_2020)]
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
  
  # 4) Check -----
  s_name_intermediate
  s_input
  s_pop_2020_rgint
  s_prop_urb_2010_rgint
  s_number_muni
  s_number_rgi
  dt_prop_muni
  dt_pop_rgi_join
  
  # 5) Function -----
  rmarkdown::render(
    input = "inst/rmarkdown/relatorio_pop.Rmd"
    , output_file = arquivo_resultado
    , params = list(
      f_name_intermediate    = s_name_intermediate
      ,f_code_intermediate   = s_input
      ,f_pop_2020_rgint      = s_pop_2020_rgint
      ,f_prop_urb_2010_rgint = s_prop_urb_2010_rgint
      ,f_number_muni         = s_number_muni
      ,f_number_rgi          = s_number_rgi
      ,fdt_prop_muni         = dt_prop_muni
      ,fdt_pop_rgi_sit       = dt_pop_rgi_join
    )
    ,quiet = TRUE
  )
  
  
  return(NULL)
}



# 3) Loop through function ------

input_file <- readr::read_rds("data/munis_list.rds")
vector_unique_code <- unique(input_file$intermediate_region$code_intermediate)
pib_raw <- readr::read_rds("data/pib_total_prep.rds")


lapply(seq_along(vector_unique_code)
       ,function(i){
         
         # i = 1
         # a) prepare names -----
         code_input <- vector_unique_code[i]
         
         file.copy(from = "inst/rmarkdown/relatorio_pop/modelo_pop.docx"
                   ,to = sprintf("inst/rmarkdown/relatorio_pib/%s.docx",code_input)
         )
         
         arquivo_input <- normalizePath(
           sprintf("inst/rmarkdown/relatorio_pib/%s.docx",code_input)
         )
         
         # b) apply ----
         function_rel_pop(arquivo_resultado = arquivo_input
                          ,s_input = code_input)
         
         return(NULL)
         # 
       })