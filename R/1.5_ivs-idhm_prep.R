rm(list=ls())
library("data.table")
library(magrittr)
library(stringr)
library("ggplot2")

# dinamismo---------------
openxlsx::getSheetNames("data-raw/IVS.xlsx")
ivs_raw <- readxl::read_xlsx("data-raw/IVS.xlsx",sheet = "Dados")
data.table::setDT(ivs_raw)
names(ivs_raw) <- janitor::make_clean_names(names(ivs_raw))


din_raw <- data.table::fread("data-raw/04_Tipologia Municípios MDR_nt522017.xlsx - Table 1.csv")
names(din_raw) <- janitor::make_clean_names(names(din_raw))
din_raw

regiao <- readr::read_rds("data/munis_list.rds")
regiao$pop_censo_pj
# prep -----------------
din_raw[,renda_per_capita_corrigida_e_ajustada := 
          renda_per_capita_corrigida_e_ajustada %>% 
          gsub("\\.","",.) %>% gsub("\\,","\\.",.)  %>% as.numeric()
]


din_raw[,
        taxa_de_crescimento_geometrico_do_pib_per_capita_trienal := 
          taxa_de_crescimento_geometrico_do_pib_per_capita_trienal %>% 
          gsub("\\.","",.) %>% gsub("\\,","\\.",.) %>% as.numeric()
]

din_raw[,renda_status := 
          stringr::str_split(string = tipologia_sub_regional
                             ,pattern = " e "
                             ,n = 2
                             ,simplify = TRUE) %>% .[,1] %>% 
          gsub(" Renda","",.)]

din_raw[,dinamismo_status := 
          stringr::str_split(string = tipologia_sub_regional
                             ,pattern = " e "
                             ,n = 2
                             ,simplify = TRUE) %>% .[,2] %>% 
          gsub(" Dinamismo","",.)]

din_raw[,codigo_ibge := as.character(codigo_ibge)]

# merge

din_raw[1]
tmp_ivs <- data.table::copy(ivs_raw)[ano == "2010"
                                     ,.SD
                                     ,.SDcols = c("uf","nome_da_uf","municipio"
                                                  ,"nome_do_municipio","ano","ivs"
                                                  ,"ivs_infraestrutura_urbana"
                                                  ,"ivs_capital_humano", "ivs_renda_e_trabalho"
                                                  ,"idhm", "idhm_longevidade", "idhm_educacao"
                                                  , "idhm_renda")
]

merge_dt <- data.table::copy(din_raw)[tmp_ivs
                                      ,on = c("codigo_ibge" = "municipio")
                                      ,`:=`(
                                        nome_da_uf                 = i.nome_da_uf
                                        , ivs                        = i.ivs
                                        , ivs_infraestrutura_urbana  = i.ivs_infraestrutura_urbana     
                                        , ivs_capital_humano         = i.ivs_capital_humano     
                                        , ivs_renda_e_trabalho       = i.ivs_renda_e_trabalho      
                                        , idhm                       = i.idhm        
                                        , idhm_longevidade           = i.idhm_longevidade         
                                        , idhm_educacao              = i.idhm_educacao        
                                        , idhm_renda                 = i.idhm_renda         
                                      )]

data.table::setnames(merge_dt
                     , old = c("uf","municipio")
                     , new = c("uf_sigla","name_muni"))

merge_dt2 <- data.table::copy(merge_dt)[
  regiao$pop_censo_pj[ano == 2010,.SD[1],by = .(municipio_codigo)]
  ,on = c("codigo_ibge" = "municipio_codigo")
  ,`:=`(
     code_intermediate  = i.code_intermediate
    , name_intermediate = i.name_intermediate
    , code_imediate     = i.code_imediate
    , name_imediate     = i.name_imediate     
)]

merge_dt2 <- merge_dt2[!is.na(name_imediate)]

readr::write_rds(x = merge_dt2
                 ,file = "data/ivs_idhm_muni.rds"
                 ,compress = "gz")
