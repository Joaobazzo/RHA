# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(googlesheets4)

my_division <- function(a,b){round(100 * a / b , 2)}
link_gdocs <- "https://docs.google.com/spreadsheets/d/1WbmvP0qgg6iHnu2o0H-4k0dXQ9wUX10HeBKIht7Nl5M/edit?usp=sharing"

# 2) Read  ------
input_file <- readr::read_rds("data/munis_list.rds")
vector_unique_code <- unique(input_file$intermediate_region$code_intermediate)
pib_raw <- readr::read_rds("data/pib_total_prep.rds")
pop_raw <- readr::read_rds("data/pop_proj_total_prep.rds")
capitais_raw <- readr::read_rds("data/capitais.rds")
ivs_raw <- readr::read_rds("data/ivs_idhm_muni.rds")
muni_area <- readxl::read_xls("data-raw/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xls"
                              ,sheet = "AR_BR_MUN_2021") %>% setDT()
cnae_raw <- readr::read_rds("data/CNAE_empregos.rds")
cplx_raw <- data.table::fread("data-raw/planilhas_basilio/AS_Total_AtiviDivMun.csv")

# 3) BASICO_POPULACAO-----

df <- data.table::copy(input_file$municipality)

df[1]
input_file$intermediate_region[,code_muni := as.numeric(code_muni)]

df[input_file$intermediate_region,on = "code_muni"
   ,":="(
     code_intermediate = i.code_intermediate
     ,name_intermediate = i.name_intermediate
     ,name_state = i.name_state
     ,name_region = i.name_region
     ,code_rgi = i.regiao_geografica_imediata
     ,name_rgi = i.nome_regiao_geografica_imediata
   )]
# area
muni_area[1]
df[1] %>% str()
muni_area[,code_muni := as.numeric(CD_MUN)]

df[muni_area,on = "code_muni"
   ,":="(
     area_muni = i.AR_MUN_2021
   )]

# pop 2020
input_file$pop_2020_pj[1]
input_file$pop_2020_pj[1] %>% str()
input_file$pop_2020_pj[,code_muni := as.numeric(municipio_codigo)]


df[input_file$pop_2020_pj,on = "code_muni"
   ,":="(
     pop_total_2020 = i.valor
   )]

# pop 2010 situacao
input_file$pop_censo_br[municipio_codigo == "5221858"]
input_file$pop_censo_br[1] %>% str()
input_file$pop_censo_br[,code_muni := as.numeric(municipio_codigo)]

df[
  input_file$pop_censo_br[ano == "2010" & situacao_do_domicilio == "Urbana",]
  ,on = "code_muni"
  ,":="(
    pop_urbana_2010 = i.valor
  )]

df[
  input_file$pop_censo_br[ano == "2010" & situacao_do_domicilio == "Rural",]
  ,on = "code_muni"
  ,":="(
    pop_rural_2010 = i.valor
  )]

df[is.na(df)] <- 0

df[,perc_urbana_2010 := my_division(pop_urbana_2010,pop_urbana_2010 +pop_rural_2010 )]
df[,perc_rural_2010 := my_division(pop_rural_2010,pop_urbana_2010 +pop_rural_2010 )]

# organize names
df %>% names()
ordem_nomes <- c("code_muni","name_muni","code_state","abbrev_state"
                 ,"name_state","name_region","code_rgi","name_rgi"  
                 ,"code_intermediate", "name_intermediate","area_muni"  
                 ,"pop_total_2020","pop_urbana_2010","pop_rural_2010"   
                 ,"perc_urbana_2010","perc_rural_2010")

df <- df[,.SD,.SDcols = ordem_nomes]

## Save-----

googlesheets4::gs4_auth_configure()
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "BASICO_POPULACAO") 

# 4) PIB-----
rm(df)
rm(ordem_nomes)

df <- data.table::copy(pib_raw)
df <- df[ano == "2019"][,.SD,.SDcols = c("municipio_codigo"
                                         ,"municipio"
                                         ,"variavel"
                                         ,"valor")]
df$variavel %>% unique()
df[variavel %like% "preços correntes total", variavel := "VAB_Total"] %>% 
  .[variavel %like% "agropecuária", variavel := "VAB_Agro"] %>% 
  .[variavel %like% "dos serviços,", variavel := "VAB_Serv"] %>% 
  .[variavel %like% "da administração,", variavel := "VAB_Adm"] %>% 
  .[variavel %like% "da indústria", variavel := "VAB_Ind"] %>% 
  .[variavel %like% "Produto Interno Bruto a preços correntes", variavel := "PIB_Total"] %>% 
  .[variavel %like% "Impostos", variavel := "Impostos"] 

df <- dcast(df,municipio_codigo + municipio ~ variavel,value.var = "valor")

ordem_nomes <- c("municipio_codigo","municipio"
                 ,"VAB_Adm","VAB_Agro","VAB_Ind","VAB_Serv"
                 ,"VAB_Total","Impostos","PIB_Total")
df <- df[,.SD,.SDcols = ordem_nomes]
data.table::setnames(df,c("municipio_codigo","municipio")
                     ,c("code_muni","name_muni"))

# add pop
df[,code_muni := as.numeric(code_muni)]

df[input_file$pop_2020_pj,on = "code_muni"
   ,":="(
     pop_total_2020 = i.valor
   )]

# pib per capita
df[,PIB_capita := PIB_Total / pop_total_2020]
df[,pop_total_2020 := NULL]

# only muni of proj
df <- df[code_muni %in% input_file$municipality$code_muni,]


## Save-----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "PIB") 
# 5) COMPLEX ----
#' [Mensagem Basílio]
#' Estou enviando dos arquivos com  os atividades (@CNAEDiv) para diversicar.
#' Um arquivo agregado na região TOTAL do projeto, e outro desagregado por municipios. 
#' Os arquivos contem o @ICE da região/municipio, e o @ICA de cada atividade econômica e 
#' outras informações: @Rem (Remuneração), @Emp (nº de empregos) antes e depois dos cenários.
#' Qualquer dúvida entrem em contato.
#' 
#' falrou colocar uma coluna para o @ICE do municipio e o @ICA_New, após o cenário. 
#' estou melhorando isso agora de manhã e logo envio um novo arquivo. Preciosismo. 
#' Isto não impede que sigam os trabalhos ai. o resto está certo. Depois é só 
#' substituir o novo arquivo, que terá algumas colunas a mais.
#' 
dt <- data.table::copy(cplx_raw)
dt[1]
dt[,num_ativ_div := uniqueN(NomeDiv),by = .(CO_MUN)]
dt[1]
