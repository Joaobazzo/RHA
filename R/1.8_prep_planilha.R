# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(googlesheets4)

my_division <- function(a,b){round(100 * a / b , 2)}
input_file <- readr::read_rds("data/munis_list.rds")
df_geral <- readr::read_rds("data/df_geral_muni.rds")
df_geral <- df_geral[!is.na(code_muni),]
df_geral[mun_pisf == 1,bacia := "PISF"]
df_geral[mun_bsf == 1,bacia := "BSF"]
df_geral[mun_bpar == 1,bacia := "BPAR"]

link_gdocs <- "https://docs.google.com/spreadsheets/d/1WbmvP0qgg6iHnu2o0H-4k0dXQ9wUX10HeBKIht7Nl5M/edit?usp=sharing"

# 2) Read  ------
vector_unique_code <- unique(input_file$intermediate_region$code_intermediate)
capitais_raw <- readr::read_rds("data/capitais.rds")
ivs_raw <- readr::read_rds("data/ivs_idhm_muni.rds")
cnae_raw <- readr::read_rds("data/CNAE_empregos.rds")




# 3) BASICO_POPULACAO ok-----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

df <- data.table::copy(df_geral)
muni_area <- readxl::read_xls("data-raw/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xls"
                              ,sheet = "AR_BR_MUN_2021") %>% setDT()


df[1]
input_file$intermediate_region[,code_muni := as.numeric(code_muni)]

# area
muni_area[,code_muni := as.character(CD_MUN)]

df[muni_area,on = "code_muni"
   ,":="(
     area_muni = i.AR_MUN_2021
   )]

df[,code_muni := as.character(code_muni)]
df[input_file$pop_2020_br,on = c("code_muni" = "municipio_codigo")
   ,":="(
     pop_total_2020 = i.valor
   )]
df[,dens_demog_2020 := pop_total_2020 / area_muni]

# pop 2010 situacao
input_file$pop_censo_br[municipio_codigo == "5221858"]
input_file$pop_censo_br[1] %>% str()
input_file$pop_censo_br[,code_muni := as.numeric(municipio_codigo)]

df[,code_muni := as.numeric(code_muni)]
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

# classificacao
df[,classe_pop_total_2020 := fcase(pop_total_2020 <= 8573,"Baixa",
                                   pop_total_2020 < 15622,"Média",
                                   pop_total_2020 >= 15622,"Alta")  ]
df[,classe_pop_rural_2010 :=  fcase(pop_rural_2010 <= 2482,"Baixa",
                                    pop_rural_2010 < 4336,"Média",
                                    pop_rural_2010 >= 4336,"Alta")  ]
df[,classe_perc_rural_2010 :=  fcase(perc_rural_2010 <= 27.7,"Baixa",
                                     perc_rural_2010 < 42.1,"Média",
                                     perc_rural_2010 >= 42.1,"Alta") ]
df[,classe_pop_urbana_2010 :=  fcase(pop_urbana_2010 <= 4624,"Baixa",
                                     pop_urbana_2010 < 8732,"Média",
                                     pop_urbana_2010 >= 8732,"Alta") ]
df[,classe_perc_urbana_2010 := fcase(perc_urbana_2010 <= 57.9,"Baixa",
                                     perc_urbana_2010 < 72.3,"Média",
                                     perc_urbana_2010 >= 72.3,"Alta")] 

## Save-----
readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/POPULACAO.rds")


df <- format(df,decimal.mark = ",") %>% 
  as.data.frame() %>% setDT()


googlesheets4::gs4_auth_configure()
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "POPULACAO") 



# 4) PIB  -----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)
# pib
pib_raw <- readr::read_rds("data/pib_total_prep.rds")
setnames(pib_raw,"mun","code_muni")
# produto
icep <- fread("data-raw/planilhas_basilio/PSICE_Municipio.csv")

#icep$Ano %>% unique()
icep <- icep[Ano == 2019,]
setnames(icep,"Mun","code_muni")
setnames(icep,"ICE","ICEM_Prod")
# atividade
#icea <- fread("data-raw/planilhas_basilio/PSICP_Municipio.csv")
icea <- fread("data-raw/planilhas_basilio/ASICE_Municipio.csv")
icea <- icea[Ano == 2019,]
setnames(icea,"Mun","code_muni")
setnames(icea,"ICE","ICEM_Ativ")

icea$ICEM_Ativ %>% summary()
# prep
df <- data.table::copy(df_geral)
df[,code_muni := as.numeric(code_muni)]
df <- df[pib_raw, on = "code_muni"]
df <- data.table::copy(df)[,.SD,
                           .SDcols = c(names(df_geral),"vab_publico"
                                       ,"vab_agro","vab_servicos"
                                       ,"vab_ind"
                                       ,"pib","pib_per_capita")]

names(df) <- c(names(df_geral),"VAB_Adm","VAB_Agro"
               ,"VAB_Serv","VAB_Ind"
               ,"PIB_Total","PIB_capita")
df <- df[icep, on = "code_muni",ICEM_Prod := i.ICEM_Prod]
df <- df[icea, on = "code_muni",ICEM_Ativ := i.ICEM_Ativ]

# pib percentual
name_vab <- c("VAB_Adm","VAB_Agro","VAB_Ind","VAB_Serv")
df[,paste0(name_vab,"_pp") := 
     lapply(.SD,my_division,b = PIB_Total)
   ,.SDcols = name_vab]

df[,classe_PIB_capita :=  fcase(PIB_capita <= 13849 ,"Baixa",
                                PIB_capita <= 21288 , "Média",
                                PIB_capita > 21288 ,"Alta")]
df[,classe_PIB_Total :=  fcase(PIB_Total <= 133094200 ,"Baixa",
                               PIB_Total <= 254476800 , "Média",
                               PIB_Total > 254476800 ,"Alta")]

df[,classe_VAB_Ind_pp :=  fcase(VAB_Ind_pp < 5.5,"Baixa",
                                VAB_Ind_pp >= 5.5 & 
                                  VAB_Ind_pp <= 9.5, "Média",
                                VAB_Ind_pp > 9.5,"Alta")]
df[,classe_VAB_Serv_pp :=  fcase(VAB_Serv_pp < 28.6,"Baixa",
                                 VAB_Serv_pp >= 28.6 & 
                                   VAB_Serv_pp < 35.7, "Média",
                                 VAB_Serv_pp > 35.7,"Alta")]
df[,classe_VAB_Agro_pp :=  fcase(VAB_Agro_pp < 10.8,"Baixa",
                                 VAB_Agro_pp >= 10.8 & 
                                   VAB_Agro_pp < 18.7, "Média",
                                 VAB_Agro_pp > 18.7,"Alta")]
df[,classe_VAB_Adm_pp :=  fcase(VAB_Adm_pp < 25.2,"Alta",
                                VAB_Adm_pp < 36.8, "Média",
                                VAB_Adm_pp >= 36.8,"Baixa")]

df[,peso_VAB_Adm_pp := fcase(classe_VAB_Adm_pp == "Baixa",0.75,
                             classe_VAB_Adm_pp == "Média",0.5,
                             classe_VAB_Adm_pp == "Alta",0.25)]

df[,classe_area_destaque := fifelse(
  (classe_VAB_Ind_pp == "Alta" | 
     classe_VAB_Serv_pp == "Alta" | 
     classe_VAB_Agro_pp == "Alta"),1,0)
  , by = .(code_muni)]

df[,classe_area_vulneraveis := fifelse(
  classe_VAB_Ind_pp == "Baixa" & 
    classe_VAB_Serv_pp == "Baixa" & 
    classe_VAB_Agro_pp == "Baixa"
  ,0,1), by = .(code_muni)]


df[,num_vab_alto := sum(c(
  classe_VAB_Ind_pp == "Alta", 
  classe_VAB_Serv_pp == "Alta", 
  classe_VAB_Agro_pp == "Alta"))
  , by = .(code_muni)]

df[,peso_VAB_COMBINACAO := 
     fcase(num_vab_alto == 1,0,
           num_vab_alto == 2,0.25,
           num_vab_alto == 3,0.75)]
df[,num_vab_alto := NULL]

df[,classe_ICEM_Ativ := fifelse(ICEM_Ativ > 0,2,0)]

df[,peso_ICEM_Prod := fcase(
  is.na(ICEM_Prod) ,0,
  !is.na(ICEM_Prod) & ICEM_Prod <= 0,0.5,
  !is.na(ICEM_Prod) & ICEM_Prod > 0,0.75)]

# peso_VAB_COMB_Agro_Serv_altos
df[,peso_VAB_COMB_Agro_Serv_altos := sum(c(
  classe_VAB_Serv_pp == "Alta", 
  classe_VAB_Agro_pp == "Alta"))
  , by = .(code_muni)]
df[,peso_VAB_COMB_Agro_Serv_altos := 
     fcase(peso_VAB_COMB_Agro_Serv_altos <= 1,0,
           peso_VAB_COMB_Agro_Serv_altos == 2,0.25)]


# peso_VAB_COMB_Ind_Serv_altos
df[,peso_VAB_COMB_Ind_Serv_altos := sum(c(
  classe_VAB_Ind_pp == "Alta", 
  classe_VAB_Serv_pp == "Alta"))
  , by = .(code_muni)]
df[,peso_VAB_COMB_Ind_Serv_altos := 
     fcase(peso_VAB_COMB_Ind_Serv_altos <= 1,0,
           peso_VAB_COMB_Ind_Serv_altos == 2,0.25)]


# peso_VAB_COMB_Agro_Ind_altos
df[,peso_VAB_COMB_Agro_Ind_altos := sum(c(
  classe_VAB_Ind_pp == "Alta", 
  classe_VAB_Agro_pp == "Alta"))
  , by = .(code_muni)]
df[,peso_VAB_COMB_Agro_Ind_altos := 
     fcase(peso_VAB_COMB_Agro_Ind_altos <= 1,0,
           peso_VAB_COMB_Agro_Ind_altos == 2,0.25)]

# pontuacao final
df[,pont_total_pib := classe_area_vulneraveis + 
     classe_area_destaque + 
     classe_ICEM_Ativ + 
     peso_ICEM_Prod + 
     peso_VAB_Adm_pp + 
     peso_VAB_COMBINACAO + 
     peso_VAB_COMB_Agro_Serv_altos +
     peso_VAB_COMB_Ind_Serv_altos + 
     peso_VAB_COMB_Agro_Ind_altos
]

df[1]
names(df)
## Save-----
googlesheets4::gs4_auth()
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "PIB") 


readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/pib.rds")

# 5) COMPLEX ATIV ----
# [Mensagem Basílio]
# Estou enviando dos arquivos com  os atividades (@CNAEDiv) para diversicar.
# Um arquivo agregado na região TOTAL do projeto, e outro desagregado por municipios. 
# Os arquivos contem o @ICE da região/municipio, e o @ICA de cada atividade econômica e 
# outras informações: @Rem (Remuneração), @Emp (nº de empregos) antes e depois dos cenários.
# Qualquer dúvida entrem em contato.
# 
# falrou colocar uma coluna para o @ICE do municipio e o @ICA_New, após o cenário. 
# estou melhorando isso agora de manhã e logo envio um novo arquivo. Preciosismo. 
# Isto não impede que sigam os trabalhos ai. o resto está certo. Depois é só 
# substituir o novo arquivo, que terá algumas colunas a mais.
# 
# Starting download of 
# /db/Dados/Resultados/EspacoProduto/Municipio/ListaProdutos_Municipio.csv

#' Helo: uma planilha é 
#' 1) ref. a complexidade das atividades do municipio
#' 2) outra é a complexidades dos produtos do municipio;
#' 3) outra é complexidade economica do municipio geral
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)


kativ <- fread(input = "data-raw/planilhas_basilio/ActivKDivers_Municipio.csv",header = TRUE)
kativ <- kativ[Ano == 2019]
kativ[,Region := as.character(Region)]


kprod <- fread(input = "data-raw/planilhas_basilio/ProdKDivers_Municipio.csv",header = TRUE)
kprod <- kprod[Ano == 2019]
kprod[,Region := as.character(Region)]

ica_muni <- data.table::fread("data/complexidade/ASICA_Municipio.csv")
ica_div_muni <- data.table::fread("data/complexidade/ListaAtividadesDiv_Municipio.csv")

icp_muni <- data.table::fread("data/complexidade/")
icp_div_muni <- data.table::fread("data/complexidade/ListaAtividadesDiv_Municipio.csv")



pcomplex_muni <- data.table::copy(df_geral)
pcomplex_muni <- pcomplex_muni[kprod
                               ,on = c("code_muni" = "Region")
                               ,classe_grau_div_prod_mun_km0 := i.Km0]
pcomplex_muni <- pcomplex_muni[kativ
                               ,on = c("code_muni" = "Region")
                               ,classe_grau_div_ativ_mun_kr0 := i.Kr0]

pcomplex_muni
pcomplex_muni
#vcr_ativ <- fread(input = "data-raw/planilhas_basilio/ActivVCR_Municipio_2019.csv",header = TRUE)

vcr_colnames <- names(vcr_ativ)[!(names(vcr_ativ) %in% "V1")]
vcr_ativ[,kro := 0]
for(i in vcr_colnames) { # i = vcr_colnames[1]
  vcr_ativ[,kro := fcase(get(i) > 0,kro + 1,
                         get(i) <= 0,kro)]
}
vcr_ativ <- vcr_ativ[,.SD,.SDcols = c("V1","kro")]

vcr_ativ[1]
cplx_ativ_raw <- data.table::fread("data-raw/planilhas_basilio/AS_Total_AtiviDivMun.csv"
                                   ,header = TRUE)
cplx_ativ_raw[1:2]


dt <- data.table::copy(df_geral)
cmplx_dt <- data.table::copy(cplx_ativ_raw)
cmplx_dt[1:2]
cmplx_dt[,num_ativ_div := uniqueN(NomeDiv),by = .(CO_MUN)]
cmplx_dt[,rank_ativ_div := 1:.N,by = .(CO_MUN)]

ordem_nomes <- c(
  "CO_MUN", "Nome_Municipio" , "ICE" 
  ,"ICE_New", "Rem", "Rem_New" , "Emp", "Emp_New" 
  ,"ICADiv", "ICADiv_New" , "NomeDiv" 
  ,"rank_ativ_div"
)

dt <- dt[,.SD,.SDcols = ordem_nomes]
data.table::setnames(dt,"NomeDiv","name_atv_potencial_div")
data.table::setnames(dt,"CO_MUN","code_muni")
data.table::setnames(dt,"Nome_Municipio","name_muni")

# save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = dt
                           ,ss = link_gdocs
                           ,sheet = "COMPLEXIDADE ATIVIDADE") 

#  6) COMPLEX PROD----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)
cplx_prod_raw <- data.table::fread("data-raw/planilhas_basilio/ListaProdutos_Municipio.csv")

df <- data.table::copy(cplx_prod_raw)
df[,Municipio := as.integer(Municipio)]

df
# add name muni
input_file$pop_censo_br[,municipio_codigo := as.integer(municipio_codigo)]
df[input_file$pop_censo_br, on = c("Municipio" = "municipio_codigo")
   ,name_muni := i.municipio]
# remove columns
df[,":="(UF = NULL,RegiaoIntermediaria = NULL,RegiaoImediata = NULL)]

data.table::setnames(df,"Municipio","code_muni")
df <- df[,.SD
         ,.SDcols = c("code_muni","name_muni"
                      ,"ICPBase","ICPDiv"
                      ,"GrupoBase","ProdBase","NomeBase"
                      ,"GrupoDiv","CorDiv","NomeDiv")]
data.table::setorder(df,code_muni,ICPDiv)
df[,rank_prod_potencial_div := 1:.N,by = .(code_muni)]
df[code_muni == 1100023]
# save----
googlesheets4::gs4_auth()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "COMPLEXIDADE PRODUTO") 
# 7) INFRAESTRUTURA ----
## a) INFRA_TELE -------
# telefonia
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)
teltm_raw <- fread("data-raw/acessos_telefonia_movel/Densidade_Telefonia_Movel.csv"
                   ,dec = ",")
names(teltm_raw) <- janitor::make_clean_names(names(teltm_raw))

teltm_raw <- teltm_raw[nivel_geografico_densidade == "Municipio" & mes == 12 & ano == 2020]
teltm_raw <- teltm_raw[,.SD,.SDcols = c("codigo_ibge","densidade")]
setnames(teltm_raw,"densidade","D_TM")

bd_raw <- fread("data-raw/acessos_banda_larga_fixa/Densidade_Banda_Larga_Fixa.csv"
                ,dec = ",")
names(bd_raw) <- janitor::make_clean_names(names(bd_raw))
bd_raw <- bd_raw[nivel_geografico_densidade == "Municipio" & mes == 12 & ano == 2020]
bd_raw <- bd_raw[,.SD,.SDcols = c("codigo_ibge","densidade")]
setnames(bd_raw,"densidade","D_BLF")

df <- bd_raw[teltm_raw,on = "codigo_ibge"]
# add name muni
setnames(df,"codigo_ibge","code_muni")
df  <- df[df_geral, on = "code_muni"]

df <- df[,.SD,.SDcols = c(names(df_geral),"D_BLF","D_TM")]

# [1] Baixo: até 86.6 acessos / 100 hab.
# [2] Médio: de 86.6 - 97.2 acessos / 100 hab.
# [3] Alto: acima de 97.2 acessos / 10 hab.
df[,classe_D_TM := data.table::fcase(D_TM >= 97.2,3,
                                     D_TM >= 86.6 & D_TM <= 97.2,2,
                                     D_TM < 86.6,1)]

# [1] Baixo: até 29.2 acessos / 100 hab.
# [2] Médio: de 29.2 - 51.5 acessos / 100 hab.
# [3] Alto: acima de 51.5 acessos / 10 hab., "
df[,classe_D_BLF := data.table::fcase(D_BLF >= 51.5,3,
                                      D_BLF >= 29.2 & D_BLF <= 51.5,2,
                                      D_BLF < 29.2,1)]
df[,pont_infr_tele := classe_D_TM + classe_D_BLF]
# save----
googlesheets4::gs4_auth()
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "INFRA_TELE") 

readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/INFRA_TELE.rds")

## b.1) INFRA_SAN  -----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)
# IN022 - Consumo médio per capita de água. (L/hab/dia)
# IN049 - Índice de perdas na distribuição. (%)
# IN023 - Índice de atend. urb. água. (% populaç.)
# IN024 - Índice atend. Urb. esgoto / munic. atendidos c água. (% população)
# IN046 - Índice de esgoto tratado / água consumida. (%)


dt1 <- fread("data-raw/planilhas_rafael/IN022 e IN049.csv")
names(dt1) <- janitor::make_clean_names(names(dt1))

dt2 <- fread("data-raw/planilhas_rafael/IN023 e in024 MUNIC. ATENDIDOSC C AGUA.csv")
names(dt2) <- janitor::make_clean_names(names(dt2))

dt3 <- fread("data-raw/planilhas_rafael/IN046 - Índice de esgoto tratado.csv")
names(dt3) <- janitor::make_clean_names(names(dt3))

df <- data.table::copy(df_geral)
df[,code_muni := as.integer(code_muni)]
df[dt1, on = c("code_muni" = "codigo_municipio_completo")
   ,":="(IN022 = i.in022_consumo_medio_per_capita_de_agua_l_hab_dia
         ,IN049 = i.in049_indice_de_perdas_na_distribuicao_percent)]
df[dt2, on = c("code_muni" = "cod")
   ,":="(IN023 = i.in023_indice_de_atend_urb_agua_percent_populac
         ,IN024 = i.in024_munic_atendidos_c_agua_percent_populacao)]
df[dt3, on = c("code_muni" = "cod")
   ,":="(IN046 = i.in046_indice_de_esgoto_tratado)]


# IN022>=153,9 120,6=<IN022<153,9 IN022<120,6
df[,classe_IN022 := data.table::fcase(IN022 >= 153.9,3,
                                      IN022 >= 120.6 & IN022 <= 153.9,2,
                                      IN022 < 120.6,1)]
# IN049=<39,2 39,2<IN049=<45,7 IN049>45,7
df[,classe_IN049 := data.table::fcase(IN049 >= 45.7,3,
                                      IN049 >= 39.2 & IN049 <= 45.7,2,
                                      IN049 < 39.2,1)]
# IN023>=92,9 88,2>IN023>=92,9 IN023<88,2
df[,classe_IN023 := data.table::fcase(IN023 >= 92.9,3,
                                      IN023 >= 88.2 & IN023 <= 92.9,2,
                                      IN023 < 88.2,1)]
# IN024>=61,9 36,7<IN024=<61,9 IN024<36,7
df[,classe_IN024 := data.table::fcase(IN024 >= 61.9,1,
                                      IN024 >= 36.7 & IN024 <= 61.9,2,
                                      IN024 < 36.7,3)]
# IN046>=49,1 33,7>IN046>=49,1 IN046<33,7
df[,classe_IN046 := data.table::fcase(IN046 >= 49.1,3,
                                      IN046 >= 33.7 & IN046 <= 49.1,2,
                                      IN046 < 33.7,1)]
# pont_infra_san
df[,pont_infra_san := classe_IN022 + classe_IN049 + classe_IN023 +
     classe_IN024 + classe_IN046]
# save----
googlesheets4::gs4_auth()
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "INFRA_SAN") 

readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/INFRA_SAN.rds")

## c) INFRA_EE---------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)
ce_dt <- openxlsx::read.xlsx("data-raw/Conjuntos Elétricos/CONJUNTO.xlsx")
names(ce_dt) <- janitor::make_clean_names(names(ce_dt))
setDT(ce_dt)

df <- data.table::copy(df_geral)
tmp_pop <- input_file$municipality
input_file$intermediate_region[,code_muni := as.integer(code_muni)]
tmp_pop[input_file$intermediate_region, on = "code_muni"
        ,code_intermediate := i.code_intermediate]
tmp_pop[,code_state := NULL]
tmp_pop[,abbrev_state := NULL]
tmp_pop[,geom := NULL]

tmp_pop
ce_dt[,decapurado := as.numeric(decapurado)]
ce_dt[,no_deconsumidores := as.numeric(no_deconsumidores)]
ce_dt[,fecapurado := as.numeric(fecapurado)]
ce_dt <- ce_dt[,list(decapurado_w = weighted.mean(x = decapurado,w = no_deconsumidores)
                     ,fecapurado_w = weighted.mean(x = fecapurado,w = no_deconsumidores))
               , by = cd_rgint]
ce_dt[,cd_rgint := as.character(cd_rgint)]
tmp_pop <- tmp_pop[ce_dt,on = c("code_intermediate" = "cd_rgint")]


tmp_pop[,code_muni := as.character(code_muni)]
df[tmp_pop,on = "code_muni",":="(DECAPTURADO =  decapurado_w,
                                 FECAPTURADO = fecapurado_w)]
# classe
# DEC=<11,5 DEC<11,5=<14,3 DEC>14,3
df[DECAPTURADO <= 11.5                     ,classe_DEC := 3]
df[DECAPTURADO > 11.5 & DECAPTURADO <= 14.3,classe_DEC := 2]
df[DECAPTURADO > 14.3                      ,classe_DEC := 1]

# FEC=<6,2 FEC<6,1=<6,2 FEC>6,2
df[FECAPTURADO <= 6.1                    ,classe_FEC := 3] # "D"     
df[FECAPTURADO > 6.1 & FECAPTURADO <= 6.2,classe_FEC := 2] # "P"     
df[FECAPTURADO > 6.2                     ,classe_FEC := 1] # "F_base"

# pont_infra_ee
df[,pont_infra_ee := classe_DEC + classe_FEC]
# save----

readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/INFRA_EE.rds")

googlesheets4::gs4_auth()
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "INFRA_EE") 



# 10) DES HUMANO ok ----------------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

df <- data.table::copy(df_geral)
des_dt_raw <- readr::read_rds("data/ivs_idhm_muni.rds")
data.table::setnames(des_dt_raw,"codigo_ibge","code_muni")
des_dt_raw[,code_muni := as.character(code_muni)]
des_dt_raw[,":="(uf_sigla = NULL
                 , name_muni = NULL
                 , microrregiao = NULL
                 , nome_da_uf = NULL
                 , code_intermediate  = NULL
                 , name_intermediate = NULL
                 , code_imediate  = NULL
                 , name_imediate  = NULL
                 , taxa_de_crescimento_geometrico_do_pib_per_capita_trienal   = NULL
                 , tipologia_sub_regional   = NULL)]

data.table::setnames(des_dt_raw,"renda_status","pndr_renda")
data.table::setnames(des_dt_raw,"dinamismo_status","pndr_dinamismo")
data.table::setnames(des_dt_raw,"renda_per_capita_corrigida_e_ajustada"
                     ,"renda_per_capita")


names(des_dt_raw)


des_dt_raw[,classe_idhm := fcase(idhm < 0.499,"Muito Baixo",
                                 idhm >= 0.500 & 
                                   idhm < 0.599, "Baixo",
                                 idhm >= 0.600 & 
                                   idhm < 0.699, "Médio",
                                 idhm >= 0.700 & 
                                   idhm < 0.799, "Alto",
                                 idhm > 0.800,"Muito Alto")]

des_dt_raw[,classe_ivs := fcase(ivs < 0.200,"Muito Baixa",
                                ivs >= 0.201 & 
                                  ivs < 0.300, "Baixa",
                                ivs >= 0.301 & 
                                  ivs < 0.400, "Média",
                                ivs >= 0.401 & 
                                  ivs < 0.500, "Alta",
                                ivs > 0.501,"Muito Alta")]
# merge
df <- df[des_dt_raw, on = "code_muni"]

# save----
googlesheets4::gs4_auth()


f_sub <- function(i){gsub("\\.", "\\,",x = i)}
df[,names(df) := lapply(.SD,f_sub)
   ,.SDcols = names(df)]

googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "DESV. HUM."
) 

readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/DESV_HUM.rds")
# 11) EMPREGO -------------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])

emp_dt_raw <- data.table::fread("data-raw/Empregos e remuneracao/ActivitySpace2019.csv")
emp_dt_name <- data.table::fread("data-raw/Empregos e remuneracao/classeCnae.csv")

data.table::setnames(emp_dt_raw,old = "Codigo_Municipio",new = "code_muni")
emp_dt_raw[,code_muni := as.character(code_muni)]
emp_dt_raw <- emp_dt_raw[df_geral, on = "code_muni"]
emp_dt_raw[,N_empresas := 1,by = code_muni]
# emprego estado
emp_estado <- data.table::copy(emp_dt_raw) %>% 
  .[,lapply(.SD,sum,na.rm = TRUE),by = .(abbrev_state)
    ,.SDcols = c("Soma_Remun", "Contagem","N_empresas")]


# muni

emp_dt <- data.table::copy(emp_dt_raw) %>% 
  .[,lapply(.SD,sum,na.rm = TRUE),by = .(code_muni,name_muni,abbrev_state)
    ,.SDcols = c("Soma_Remun", "Contagem","N_empresas")]
emp_dt[emp_estado,on = "abbrev_state"
       ,":="(remun_total_Estado = i.Soma_Remun,
             empregos_Estado = i.Contagem,
             N_empresas_Estado = i.N_empresas)]
emp_dt[,remun_capita := Soma_Remun/Contagem]
data.table::setnames(emp_dt
                     ,old = c("Soma_Remun", "Contagem")
                     ,new = c("remun_total", "empregos"))
# condicao
emp_dt[,empregos_RGINT_perc_empregos_Estado :=
         fcase(
           empregos/empregos_Estado < 0.1,1
           ,empregos/empregos_Estado < 0.2,2
           ,empregos/empregos_Estado < 0.3,3
           ,empregos/empregos_Estado >= 0.3,4)
       ,by = . (code_muni)]
emp_dt[,rem_RGINT_perc_rem_Estado :=
         fcase(
           remun_total/remun_total_Estado < 0.1,1
           ,remun_total/remun_total_Estado < 0.2,2
           ,remun_total/remun_total_Estado < 0.3,3
           ,remun_total/remun_total_Estado >= 0.3,4)
       ,by = . (code_muni)]
# N_empresas
emp_dt[,empresas_RGINT_perc_empresas_Estado :=
         fcase(
           N_empresas/N_empresas_Estado < 0.1,1
           ,N_empresas/N_empresas_Estado < 0.2,2
           ,N_empresas/N_empresas_Estado < 0.3,3
           ,N_empresas/N_empresas_Estado >= 0.3,4)
       ,by = . (code_muni)]

# 
emp_dt1 <- data.table::copy(emp_dt_raw)
emp_dt1 <- emp_dt1[,sum(Contagem),by = .(code_muni,CNAE20)]
emp_dt1[,num_grupo_atv_20      := fifelse(V1 <= 19             ,1,0)]
emp_dt1[,num_grupo_atv_20_49   := fifelse(V1 >= 20  & V1 <= 49 ,1,0)]
emp_dt1[,num_grupo_atv_50_99   := fifelse(V1 >= 50  & V1 <= 99 ,1,0)]
emp_dt1[,num_grupo_atv_100_500 := fifelse(V1 >= 100 & V1 <= 500,1,0)]
emp_dt1[,num_grupo_atv_500     := fifelse(V1 >= 500            ,1,0)]
emp_dt1 <- emp_dt1[,lapply(.SD,sum)
                   ,by = .(code_muni)
                   ,.SDcols = c("num_grupo_atv_20"
                                ,"num_grupo_atv_20_49"  
                                ,"num_grupo_atv_50_99"  
                                ,"num_grupo_atv_100_500"
                                ,"num_grupo_atv_500")]
emp_dt <- emp_dt[emp_dt1,on = "code_muni"]


emp_dt3 <- data.table::copy(emp_dt_raw)
emp_dt_name[,cor_lower := tolower(Cor)]
emp_dt3[emp_dt_name, on = "Cor",name_grupo := i.Nome]
emp_dt3 <- emp_dt3[,lapply(.SD,sum),by = .(code_muni,CNAE20,name_grupo)
                   ,.SDcols = c("Soma_Remun","Contagem","N_empresas")]
emp_dt3 <- emp_dt3[name_grupo != "Seguridade Social Obrigatória", {
  id <- which.max(Contagem)
  list("CNAE20" = CNAE20[id]
       ,"name_grupo" = name_grupo[id]
       , "Soma_Remun" = Soma_Remun[id]
       , "Contagem" = Contagem[id])
},by = .(code_muni)]
emp_dt <- emp_dt[emp_dt3, on = "code_muni"
                 ,":=" ("atv_top_empregos_name" =   i.name_grupo
                        ,"atv_top_empregos" = i.Contagem)]
# atv_top_remun_capita_name
emp_dt4 <- data.table::copy(emp_dt_raw)
emp_dt_name[,cor_lower := tolower(Cor)]
emp_dt4[emp_dt_name, on = "Cor",name_grupo := i.Nome]
emp_dt4 <- emp_dt4[,lapply(.SD,sum),by = .(code_muni,CNAE20,name_grupo)
                   ,.SDcols = c("Soma_Remun","Contagem","N_empresas")]
emp_dt4 <- emp_dt4[name_grupo != "Seguridade Social Obrigatória", {
  id <- which.max(Soma_Remun/Contagem)
  list("CNAE20" = CNAE20[id]
       ,"name_grupo" = name_grupo[id]
       , "Rem_capita" =  Soma_Remun[id]/Contagem[id]
  )
},by = .(code_muni)]
emp_dt[emp_dt4, on = c("code_muni")
       ,":=" ("atv_top_remun_capita_name" =   i.name_grupo
              ,"atv_top_remun_capita" = i.Rem_capita)]

emp_dt[,name_muni := NULL]
emp_dt[,remun_total_Estado := NULL]
emp_dt[,empregos_Estado := NULL]
emp_dt[,N_empresas_Estado := NULL]


old_names <- names(emp_dt)[!(names(emp_dt) %in% c("code_muni","abbrev_state" ))]
emp_dt <- emp_dt[df_geral, on = c("code_muni","abbrev_state")]

emp_dt <- emp_dt[,.SD,.SDcols = c(names(df_geral),old_names)]
# pontuacao final
emp_dt[,total_filtro_6 := empregos_RGINT_perc_empregos_Estado + 
         rem_RGINT_perc_rem_Estado + empresas_RGINT_perc_empresas_Estado]

# save----
readr::write_rds(emp_dt,"data/complexidade/complexidade_muni_prep_data/EMPREGO.rds")


googlesheets4::gs4_auth()
dt <- dt %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = emp_dt
                           ,ss = link_gdocs
                           ,sheet = "EMPREGO") 


# 12) BALANCA COMERCIAL -------------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])

imp_dt <- data.table::fread("data-raw/Importacao e exportacao/IMP_2020_MUN.csv")
exp_dt <- data.table::fread("data-raw/Importacao e exportacao/EXP_2020_MUN.csv")
dic_imp <- data.table::fread("data-raw/Importacao e exportacao/codigos_sh4.csv")

imp_dt <- rbind(imp_dt[,tipo := "importacao"]
                ,exp_dt[,tipo := "exportacao"])

imp_dt[dic_imp,on = c("SH4" = "CO_SH4"),name_sh4_por := i.NO_SH4_POR]

# total imp
imp_dt1 <- copy(imp_dt)[,sum(as.numeric(VL_FOB),na.rm = TRUE),by = .(CO_MUN,tipo)]
imp_dt1[,V1 := V1/10^6]
imp_dt1 <- dcast(imp_dt1,formula = CO_MUN ~ tipo 
                 ,value.var = c("V1"))
imp_dt1[,saldo := exportacao - importacao]
imp_dt1[,exp_sim := fifelse(exportacao > 0 | !is.na(exportacao) ,1,0)]
imp_dt1[,imp_sim := fifelse(importacao > 0 | !is.na(importacao) ,1,0)]
imp_dt1[,saldo_sim := fifelse(saldo > 0 | !is.na(saldo) ,1,0)]


# valores/produtos
imp_dt2 <- copy(imp_dt)[
  ,sum(as.numeric(VL_FOB),na.rm = TRUE)
  ,by = .(CO_MUN,name_sh4_por,SH4,tipo)]
imp_dt2 <- imp_dt2[,{
  id <- which.max(V1)
  list(
    "prod_mais" = name_sh4_por[id]
    ,"valor_prod_mais" = V1[id]
  )
},by = . (CO_MUN,tipo)]

imp_dt2 <- dcast(imp_dt2,formula = CO_MUN ~ tipo 
                 ,value.var = c("valor_prod_mais","prod_mais"))


imp_dt2 <- imp_dt2[imp_dt1,on = "CO_MUN"]
# add name
imp_dt2[,CO_MUN := as.double(CO_MUN)]
setnames(imp_dt2,"CO_MUN","code_muni")
imp_dt2[,code_muni := as.character(code_muni)]
imp_dt2 <- imp_dt2[df_geral, on = "code_muni"]
# order
imp_dt2 <- imp_dt2[,.SD
                   ,.SDcols = c(names(df_geral)
                                ,"exportacao", "importacao"
                                ,"saldo", "exp_sim", "imp_sim","saldo_sim"
                                ,"prod_mais_exportacao", "prod_mais_importacao"
                                ,"valor_prod_mais_exportacao", "valor_prod_mais_importacao"
                   )]

imp_dt2 <- imp_dt2[!is.na(name_muni),]
imp_dt2

imp_dt2[,classe_exportacao :=  fcase(exportacao <=  1708335/10^6,"Baixa",
                                     exportacao <= 11532606/10^6, "Média",
                                     exportacao >  11532606/10^6,"Alta")]
imp_dt2[,classe_importacao :=  fcase(importacao <=  447914/10^6,"Baixa",
                                     importacao <= 2706424/10^6, "Média",
                                     importacao >  2706424/10^6,"Alta")]

imp_dt2[,total_filtro_7 := exp_sim + imp_sim + saldo_sim]


imp_dt2[,p_RGINT_perc_exp_Estado := exportacao / sum(exportacao,na.rm = TRUE), by = .(abbrev_state)]
imp_dt2[,p_RGINT_perc_exp_Bacia := exportacao / sum(exportacao,na.rm = TRUE), by = .(bacia)]
imp_dt2[,p_RGINT_perc_exp_RTP := exportacao / sum(exportacao,na.rm = TRUE), by = .(regiao_total)]

# save----
readr::write_rds(imp_dt2
                 ,"data/complexidade/complexidade_muni_prep_data/BALANCA_COMERCIAL.rds")


googlesheets4::gs4_auth()
imp_dt2 <- imp_dt2 %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = imp_dt2
                           ,ss = link_gdocs
                           ,sheet = "BALANCA_COMERCIAL") 


readr::write_rds(imp_dt2,"data/complexidade/complexidade_muni_prep_data/BALANCA_COMERCIAL.rds")
# 13) FNE -------------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

tab <- openxlsx::read.xlsx("data-raw/FNE/FNE - 2020_VALOR_CONTRATADO.xlsx")

names(tab) <- janitor::make_clean_names(names(tab))
setDT(tab)
tab$urbano_rural %>% unique()
tab$setor %>% unique()
tab[urbano_rural == "URBANO",]$setor %>% unique()
tab[urbano_rural == "RURAL",]$setor %>% unique()
tab[urbano_rural == "URBANO",]$objetivo %>% unique()
tab[urbano_rural == "RURAL",]$objetivo %>% unique()

# urbano_rural
tab1 <- data.table::copy(tab) %>% 
  .[,sum(valor_contratado),by = .(codigo_municipio,urbano_rural)]
tab1 <-  dcast(tab1,codigo_municipio ~ urbano_rural
               ,value.var = "V1")
# pf/pj
tab2 <- data.table::copy(tab) %>% 
  .[,sum(valor_contratado),by = .(codigo_municipio,tipo_pessoa)]
tab2 <-  dcast(tab2,codigo_municipio ~ tipo_pessoa
               ,value.var = "V1")
# setores
tab3 <- data.table::copy(tab) %>% 
  .[,sum(valor_contratado),by = .(codigo_municipio,setor)]
tab3 <-  dcast(tab3
               ,codigo_municipio ~ setor
               ,value.var = "V1",fill = 0)
names(tab3) <- janitor::make_clean_names(names(tab3))
# numero contrato
tab4 <- data.table::copy(tab) %>% 
  .[,.N,by = .(codigo_municipio)]
# merge
tab1 <- tab1[tab2, on = "codigo_municipio"]
tab1 <- tab1[tab3, on = "codigo_municipio"]
tab1 <- tab1[tab4, on = "codigo_municipio"]
tab1[,codigo_municipio := as.character(codigo_municipio)]
setnames(tab1,"codigo_municipio","code_muni")


setnames(tab1,
         old = c("RURAL","URBANO","PF","PJ"
                 ,"agroindustria","agricola","comercio_e_servicos", "industria","infraestrutura"
                 ,"pecuaria","pessoa_fisica","turismo","N"),
         new =  c("inv_rural","inv_urbano","inv_pf"
                  ,"inv_pj","inv_agroind","inv_agr","inv_comercio"
                  ,"inv_ind","inv_infra","inv_pec","inv_pf1"
                  ,"inv_turismo","num_contratos"))
tab1[,inv_pf1 := NULL]
old_names <- names(tab1)[!(names(tab1) %in% "code_muni")]
tab1 <- tab1[df_geral, on = "code_muni"]
# contratos
tab1[,classe_num_contratos :=  fcase(num_contratos <= 41,"Baixo",
                                     num_contratos <= 87, "Médio",
                                     num_contratos > 87,"Alto")]
# tab1 <- tab1[,.SD,.SDcols = 
#                c(names(df_geral),"inv_urbano","inv_rural"
#                  ,"inv_pf","inv_pj","inv_infra","inv_comercio","inv_ind"
#                  ,"inv_pec","inv_agr","inv_agroind","inv_turismo"
#                  ,"num_contratos")]
# save----

readr::write_rds(tab1,"data/complexidade/complexidade_muni_prep_data/FNE.rds")

googlesheets4::gs4_auth()
tab1 <- tab1 %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = tab1
                           ,ss = link_gdocs
                           ,sheet = "FNE") 



# 14) EDUCACAO ----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)


tab1 <- fread("data-raw/Educação/Educaç╞o/INDICADORES_2019.CSV"
              ,encoding = "Latin-1")
tab1 <- tab1[CO_MUNICIPIO != "NÃO SE APLIC",]
tab1 <- tab1[NO_INDICADOR %like% "Investimento educacional",]
tab1[,VL_DECLARADO := gsub(",",".",VL_DECLARADO) %>% as.numeric()]
tab1 <- tab1[,sum(VL_DECLARADO),by = .(CO_MUNICIPIO,NO_MUNICIPIO,NO_INDICADOR)]
tab1[,NO_INDICADOR1 := gsub("Investimento educacional por aluno "
                            ,"Inv_"
                            ,NO_INDICADOR)]
tab1[,NO_INDICADOR1 := NO_INDICADOR1 %>% gsub("da","",.) %>% 
       gsub("de","",.)  %>% gsub("do","",.) %>% gsub("Inv_ ","Inv_",.) %>% 
       gsub(" ","_",.) %>%  gsub("_-_","_",.) %>% gsub("educação","educ",.) %>% 
       gsub("Investimento_","Inv_",.) %>% gsub("educacional_","educ_",.) %>% 
       gsub("__","_",.) %>% 
       gsub("médio","medio",.) %>% gsub("básica","basica",.)]
tab1 <- dcast(tab1
              ,formula = CO_MUNICIPIO + NO_MUNICIPIO ~ NO_INDICADOR1
              ,value.var = "V1",fill = 0)


vect_muni <- tab1$CO_MUNICIPIO
new_code <- sapply(vect_muni,function(i){
  id <- grepl(pattern = paste0("^",i)
              ,x = input_file$pop_2020_br$municipio_codigo)
  return(input_file$pop_2020_br[id,municipio_codigo])
})
length(new_code)
tab1[,code_muni := new_code]
tab1[,CO_MUNICIPIO := NULL]
data.table::setnames(tab1
                     ,c("NO_MUNICIPIO")
                     ,c("name_muni"))
tab1[,code_muni := as.character(code_muni)]
tab1[,name_muni := NULL]

tab1 <- tab1[df_geral,on = "code_muni"]

tab1 <- tab1[,.SD,.SDcols = c( names(df_geral), 'Inv_educ_basica'
                               , 'Inv_educ_especial'
                               , 'Inv_educ_infantil'
                               , 'Inv_educ_infantil_creche'
                               , 'Inv_educ_infantil_pre-escola'
                               , 'Inv_educ_jovens_e_adultos'
                               , 'Inv_educ_por_aluno', 'Inv_educ_profissional'
                               , 'Inv_educ_superior', 'Inv_ensino_funmental'
                               , 'Inv_ensino_medio')]


tab1[,classe_inv_educ_infantil :=  fcase(Inv_educ_infantil <= 4288 ,1,
                                         Inv_educ_infantil <= 5757 ,2,
                                         Inv_educ_infantil > 5757  ,3)]
tab1[,classe_inv_ensino_funmental :=  fcase(Inv_ensino_funmental <= 7008 ,1,
                                            Inv_ensino_funmental <= 8398 ,2,
                                            Inv_ensino_funmental > 8398  ,3  )]
tab1[,classe_inv_educ_especial :=  fcase(Inv_educ_especial <=  3179 ,1,
                                         Inv_educ_especial <= 4508  ,2,
                                         Inv_educ_especial > 4508   ,3 )]
tab1[,total_filtro_13 := classe_inv_educ_infantil + classe_inv_ensino_funmental +
       classe_inv_educ_especial]

# save----
googlesheets4::gs4_auth()
tab1 <- tab1 %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = tab1
                           ,ss = link_gdocs
                           ,sheet = "EDUCACAO") 
readr::write_rds(tab1,"data/complexidade/complexidade_muni_prep_data/EDUCACAO.rds")

# 15) MEIO AMBIENTE ---------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

tab <- fread("data/uso_antropico_natural_mun_mapbio.csv",dec = ",")
tab[,CD_MUN := as.character(CD_MUN)]
all_names <- names(tab)[!(names(tab) %in% "CD_MUN")]
setnames(tab,"CD_MUN","code_muni")
tab <- tab[df_geral, on = c("code_muni")]
tab[1]
tab <- tab[,.SD,.SDcols = c(names(df_geral),all_names)]
tab <- tab[!is.na(AREAMUN_Ha)]

tab
# save----
googlesheets4::gs4_auth()
tab <- tab %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = tab
                           ,ss = link_gdocs
                           ,sheet = "MEIO_AMBIENTE") 

readr::write_rds(tab,"data/complexidade/complexidade_muni_prep_data/MEIO_AMBIENTE.rds")

## b) SANEAMENTO -----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

muni_agua <- fread("data/saneamento/MunSanAgua2.csv")
muni_esgo <- fread("data/saneamento/MunSanEsgo_.csv")
muni_pluv <- fread("data/saneamento/MunSanPluv.csv")
muni_resi <- fread("data/saneamento/MunSanResi.csv",dec=",")
muni_agua[,Mun := NULL]
muni_esgo[,Mun := NULL]
muni_pluv[,Mun := NULL]
muni_resi[,Mun := NULL]

df <- data.table::copy(df_geral)
df[,code_muni := as.integer(code_muni)]
df <- merge.data.table(x = df, y = muni_agua
                       ,by.x = "code_muni",by.y = "Cod"
                       ,all = TRUE)
df <- merge.data.table(x = df, y = muni_esgo
                       ,by.x = "code_muni",by.y = "Cod"
                       ,all = TRUE)
df <- merge.data.table(x = df, y = muni_pluv
                       ,by.x = "code_muni",by.y = "Cod"
                       ,all = TRUE)
df <- merge.data.table(x = df, y = muni_resi
                       ,by.x = "code_muni",by.y = "Cod"
                       ,all = TRUE)


## save----
googlesheets4::gs4_auth()
df <- df %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = df
                           ,ss = link_gdocs
                           ,sheet = "SANEAMENTO") 

readr::write_rds(df,"data/complexidade/complexidade_muni_prep_data/SANEAMENTO.rds")

### RES old-----
# res <- fread("data/Municipios - SanRes.csv")
# names(res) <- janitor::make_clean_names(names(res))
# setnames(res,"codigo_municipio_completo","code_muni")
# all_names <- names(res)[!(names(res) %in% c( "code_muni","nome_municipio"))]
# 
# res <- res[df_geral,on = "code_muni"]
# res <- res[,.SD,.SDcols = c(names(df_geral),all_names)]
# res[,in031 := fifelse(in031 == "Sim",1,0)]
# res[,ca004 := fifelse(ca004 == "Sim",2,0)]
# res[,ca005 := fifelse(ca005 == "Sim",3,0)]
# res[,total_filtro_11 := in031 + ca004 + ca005]
# 16) HIDRICO ---------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

tab <- fread("data/Hidrico.csv",dec=",")
names(tab) <- janitor::make_clean_names(names(tab))
tab[1:3]
tab <- tab[,.SD,.SDcols = c("cod_mun","i1_intervalo_perc")]
tab[,i1_intervalo_perc := gsub(",",".",i1_intervalo_perc)]
tab[i1_intervalo_perc == ">90",i1_intervalo_perc := "> 90"]
tab[i1_intervalo_perc == ">90",i1_intervalo_perc := "> 90"]

tab[i1_intervalo_perc == "0 – 24.9" ,grau_compr := 1] #  "Muito baixo"
tab[i1_intervalo_perc == "25 - 49.9",grau_compr := 2] #  "Baixo"      
tab[i1_intervalo_perc == "50 - 74.9",grau_compr := 3] #  "Médio"      
tab[i1_intervalo_perc == "75 - 89.9",grau_compr := 4] #  "Alto"       
tab[i1_intervalo_perc == "> 90"     ,grau_compr := 5] #  "Muito Alto" 
tab[i1_intervalo_perc == "Sem informações ou com informações parciais"
    ,grau_compr := "Sem informações ou com informações parciais"]
names(tab) <- c("code_muni","interv_comprom_rh","classe_comprom_rh")

tab[,code_muni := as.character(code_muni)]
tab <- tab[df_geral,on = "code_muni"]
tab <- tab[,.SD,.SDcols = c(names(df_geral),"interv_comprom_rh","classe_comprom_rh")]

# save----
googlesheets4::gs4_auth()
tab <- tab %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = tab
                           ,ss = link_gdocs
                           ,sheet = "REC_HIDRICO") 

readr::write_rds(tab,"data/complexidade/complexidade_muni_prep_data/REC_HIDRICO.rds")

# 17) IRRIGACAO -----------
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

tab <- readr::read_rds("data/agro_table5457.rds")
#tab <- fread("data-raw/Sidra - Rebanhos, aquicultura, silvicultura e area plantada/tabela5457.csv")
setDT(tab)
names(tab) <- janitor::make_clean_names(names(tab))
names(tab)
tab$unidade_de_medida %>% unique()
tab$variavel %>% unique()
tab$unidade_de_medida %>% unique()
tab$produto_das_lavouras_temporarias_e_permanentes %>% unique()

## Total 
tab1 <- data.table::copy(tab)
tab1 <- tab1[produto_das_lavouras_temporarias_e_permanentes == "Total"]
tab1[1:5]

tab1 <- dcast(data = tab1,
              formula = municipio_codigo ~ variavel
              ,value.var = "valor")
names(tab1) <- janitor::make_clean_names(names(tab1))

tab1[,quantidade_produzida := NULL]
tab1[,rendimento_medio_da_producao := NULL]
tab1[,valor_da_producao_percentual_do_total_geral := NULL]
tab1[,area_colhida_percentual_do_total_geral := NULL]
tab1[,area_plantada_ou_destinada_a_colheita_percentual_do_total_geral := NULL]
tab1[1]

# most
tab2 <- data.table::copy(tab)
tab2 <- tab2[produto_das_lavouras_temporarias_e_permanentes != "Total"]
tab2 <- tab2[unidade_de_medida == "Hectares"]
tab2 <- tab2[variavel == "Área plantada ou destinada à colheita"]
tab2 <- tab2[!is.na(valor)]
tab2 <- tab2[order(-rank(valor), municipio_codigo),]

tab2 <- tab2[,.SD[1],by = .(municipio_codigo)]

tab1 <- tab1[tab2,on = "municipio_codigo"
             ,":="(valor_prod_principal = i.valor
                   ,prod_principal = i.produto_das_lavouras_temporarias_e_permanentes)]
tab1[1]
# add ref
setnames(tab1,"municipio_codigo","code_muni")
tab1[,code_muni := as.character(code_muni)]
tab1 <- tab1[df_geral,on = "code_muni"]
tab1 <- tab1[,.SD,.SDcols = c(names(df_geral),"valor_da_producao"
                              ,"area_colhida"
                              ,"area_plantada_ou_destinada_a_colheita"
                              ,"prod_principal"
                              ,"valor_prod_principal")]

# save----
googlesheets4::gs4_auth()
tab1 <- tab1 %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = tab1
                           ,ss = link_gdocs
                           ,sheet = "EMPREGO") 

readr::write_rds(tab1,"data/complexidade/complexidade_muni_prep_data/EMPREGO.rds")

# 18) AGRO -----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
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
# agro_rgint | estado | pais
agro_area <- copy(agro)
agro_area <- agro_area[df_geral
                       , on = c("municipio_codigo" = "code_muni")
                       ,":="(code_intermediate   = i.code_intermediate
                             ,abbrev_state = i.abbrev_state)]

agro_bacia <- copy(agro_area)[df_geral
                              , on = c("municipio_codigo" = "code_muni")
                              ,":="(mun_pisf = mun_pisf
                                    , mun_bsf  = mun_bsf
                                    , mun_bpar = mun_bpar)]
agro_bacia <- agro_bacia[ mun_pisf + mun_bsf + mun_bpar > 0,]
agro_bacia[mun_pisf > 0,bacia := "pisf"]
agro_bacia[mun_bsf > 0 ,bacia := "bsf"]
agro_bacia[mun_bpar > 0,bacia := "bpar"]
agro_bacia1 <- agro_bacia[,sum(valor_da_producao_mil_reais,na.rm = TRUE)
                          ,by = .(bacia)]
agro_bacia <- agro_bacia[agro_bacia1, on = "bacia"
                         ,valor_bacia := i.V1]
agro_bacia <- agro_bacia[,.SD[1],by = .(code_intermediate)]
agro_bacia <- agro_bacia[,.SD,.SDcols = c("code_intermediate","bacia","valor_bacia")]
rm(agro_bacia1)
#
agro_est <- copy(agro_area)[,
                            sum(valor_da_producao_mil_reais,na.rm = TRUE)
                            ,by = .(abbrev_state)]
agro_pais <- copy(agro_area)[, sum(valor_da_producao_mil_reais,na.rm = TRUE)]


agro_rgint <- copy(agro_area)[,
                              sum(valor_da_producao_mil_reais,na.rm = TRUE)
                              ,by = .(code_intermediate,abbrev_state)]

agro_rgint <- agro_rgint[agro_est,on = "abbrev_state",V1_est := i.V1]
agro_rgint[,p_rgint_estado_valor_da_producao := my_division(V1,V1_est)]

agro_rgint <- agro_rgint[agro_bacia, on = "code_intermediate",V1_bacia := i.valor_bacia]
agro_rgint[,p_rgint_bacia_valor_da_producao  := my_division(V1,V1_bacia)]

agro_rgint[,p_rgint_pais_valor_da_producao  := my_division(V1,agro_pais)]




rm(agro_est)
rm(agro_pais)
rm(agro_area)
rm(agro_bacia)

# agro_mais_prod_reais

agro_dt1 <- data.table::copy(agro) %>% 
  .[order(valor_da_producao_mil_reais,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"produto_das_lavouras_temporarias_e_permanentes"
                 ,"valor_da_producao_mil_reais"
    )] %>% 
  data.table::setnames(.
                       ,old = c("valor_da_producao_mil_reais",
                                "produto_das_lavouras_temporarias_e_permanentes")
                       ,new = c("valor_agro_mais_prod_reais",
                                "agro_mais_prod_reais"))
# agro_mais_prod_rend
agro_dt2 <- data.table::copy(agro) %>% 
  .[order(rendimento_medio_da_producao_quilogramas_por_hectare,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"produto_das_lavouras_temporarias_e_permanentes"
                 ,"rendimento_medio_da_producao_quilogramas_por_hectare"
    )] %>% 
  data.table::setnames(.
                       ,old = c("rendimento_medio_da_producao_quilogramas_por_hectare"
                                , "produto_das_lavouras_temporarias_e_permanentes")
                       ,new = c("valor_agro_mais_prod_rend"
                                ,"agro_mais_prod_rend"))

# agro_mais_prod_area_plantada

agro_dt3 <- data.table::copy(agro) %>% 
  .[order(area_plantada_ou_destinada_a_colheita_hectares,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"produto_das_lavouras_temporarias_e_permanentes"
                 ,"area_plantada_ou_destinada_a_colheita_hectares"
    )] %>% 
  data.table::setnames(.
                       ,old = c("area_plantada_ou_destinada_a_colheita_hectares"
                                , "produto_das_lavouras_temporarias_e_permanentes")
                       ,new = c("valor_agro_mais_prod_area_plantada"
                                ,"agro_mais_prod_area_plantada"))

# silv_mais_prod_area # 5930

silv <- readr::read_rds("data/agro_table5930.rds")
silv$municipio_codigo %>% uniqueN()
silv$variavel %>% unique()
silv$especie_florestal %>% unique()

# agro_muni
silv_muni <- copy(silv)[,
                        
                        list(muni_valor_silv = 
                               sum(valor,na.rm = TRUE))
                        
                        ,by = municipio_codigo]
# SILV | ringt | estado | pais
silv_area <- copy(silv)
silv_area <- silv_area[df_geral
                       , on = c("municipio_codigo" = "code_muni")
                       ,":="(code_intermediate   = i.code_intermediate
                             ,abbrev_state = i.abbrev_state)]

silv_bacia <- copy(silv_area)[df_geral
                              , on = c("municipio_codigo" = "code_muni")
                              ,":="(mun_pisf = mun_pisf
                                    , mun_bsf  = mun_bsf
                                    , mun_bpar = mun_bpar)]
silv_bacia <- silv_bacia[ mun_pisf + mun_bsf + mun_bpar > 0,]
silv_bacia[mun_pisf > 0,bacia := "pisf"]
silv_bacia[mun_bsf > 0 ,bacia := "bsf"]
silv_bacia[mun_bpar > 0,bacia := "bpar"]
silv_bacia1 <- silv_bacia[,sum(valor,na.rm = TRUE)
                          ,by = .(bacia)]
silv_bacia <- silv_bacia[silv_bacia1, on = "bacia"
                         ,valor_bacia := i.V1]
silv_bacia <- silv_bacia[,.SD[1],by = .(code_intermediate)]
silv_bacia <- silv_bacia[,.SD,.SDcols = c("code_intermediate","bacia","valor_bacia")]
rm(silv_bacia1)

# 

silv_rgint <- copy(silv_area)[,
                              sum(valor,na.rm = TRUE)
                              ,by = .(code_intermediate,abbrev_state)]
silv_est <- copy(silv_area)[,
                            sum(valor,na.rm = TRUE)
                            ,by = .(abbrev_state)]
silv_pais <- copy(silv_area)[, sum(valor,na.rm = TRUE)]

silv_rgint <- silv_rgint[silv_est,on = "abbrev_state",V1_est := i.V1]
silv_rgint[,p_rgint_estado_area_silv := my_division(V1,V1_est)]

silv_rgint <- silv_rgint[silv_bacia,on = "code_intermediate",V1_bacia := i.valor_bacia]
silv_rgint[,p_rgint_bacia_area_silv := my_division(V1,V1_bacia)]


silv_rgint[,p_rgint_pais_area_silv  := my_division(V1,silv_pais)]

rm(silv_est)
rm(silv_bacia)
rm(silv_pais)
rm(silv_area)
# continuacao
silv <- data.table::copy(silv)[!is.na(valor) &
                                 especie_florestal != "Total"]
silv <- dcast(silv
              ,formula = municipio_codigo + municipio +
                especie_florestal + 
                especie_florestal_codigo ~ 
                variavel + unidade_de_medida
              ,value.var = "valor") 
names(silv) <- janitor::make_clean_names(names(silv))

silv_dt <- data.table::copy(silv) %>% 
  .[order(area_total_existente_em_31_12_dos_efetivos_da_silvicultura_hectares,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"especie_florestal"
                 ,"area_total_existente_em_31_12_dos_efetivos_da_silvicultura_hectares"
    )] %>% 
  data.table::setnames(.
                       ,old = c("area_total_existente_em_31_12_dos_efetivos_da_silvicultura_hectares"
                                , "especie_florestal")
                       ,new = c("valor_silv_mais_prod_area"
                                ,"silv_mais_prod_area"))
silv_dt[1:4]
# AQUICULTURA # 3940
aqui <- readr::read_rds("data/agro_table3940.rds")
aqui$municipio_codigo %>% uniqueN()
aqui$variavel %>% unique()
aqui$tipo_de_produto_da_aquicultura %>% unique()

#  AQUI_muni
aqui_muni <- copy(aqui)[,
                        
                        list(muni_valor_aqui = 
                               sum(valor,na.rm = TRUE))
                        
                        ,by = municipio_codigo]
# AQUICULTURA | ringt | estado | pais
aqui_area <- copy(aqui)
aqui_area <- aqui_area[df_geral
                       , on = c("municipio_codigo" = "code_muni")
                       ,":="(code_intermediate   = i.code_intermediate
                             ,abbrev_state = i.abbrev_state)]

aqui_bacia <- copy(aqui_area)[df_geral
                              , on = c("municipio_codigo" = "code_muni")
                              ,":="(mun_pisf = mun_pisf
                                    , mun_bsf  = mun_bsf
                                    , mun_bpar = mun_bpar)]
aqui_bacia <- aqui_bacia[ mun_pisf + mun_bsf + mun_bpar > 0,]
aqui_bacia[mun_pisf > 0,bacia := "pisf"]
aqui_bacia[mun_bsf > 0 ,bacia := "bsf"]
aqui_bacia[mun_bpar > 0,bacia := "bpar"]
aqui_bacia1 <- aqui_bacia[,sum(valor,na.rm = TRUE)
                          ,by = .(bacia)]
aqui_bacia <- aqui_bacia[aqui_bacia1, on = "bacia"
                         ,valor_bacia := i.V1]
aqui_bacia <- aqui_bacia[,.SD[1],by = .(code_intermediate)]
aqui_bacia <- aqui_bacia[,.SD,.SDcols = c("code_intermediate","bacia","valor_bacia")]
rm(aqui_bacia1)
# 
aqui_rgint <- copy(aqui_area)[,
                              sum(valor,na.rm = TRUE)
                              ,by = .(code_intermediate,abbrev_state)]
aqui_est <- copy(aqui_area)[,
                            sum(valor,na.rm = TRUE)
                            ,by = .(abbrev_state)]
aqui_pais <- copy(aqui_area)[, sum(valor,na.rm = TRUE)]

aqui_rgint <- aqui_rgint[aqui_est,on = "abbrev_state",V1_est := i.V1]
aqui_rgint[,p_rgint_estado_area_aqui := my_division(V1,V1_est)]

aqui_rgint <- aqui_rgint[aqui_bacia,on = "code_intermediate",V1_bacia := i.valor_bacia]
aqui_rgint[,p_rgint_bacia_area_aqui := my_division(V1,V1_bacia)]

aqui_rgint[,p_rgint_pais_area_aqui  := my_division(V1,aqui_pais)]

rm(aqui_pais)
rm(aqui_bacia)
rm(aqui_est)
rm(aqui_area)
aqui_rgint
# continuacao

aqui <- data.table::copy(aqui)[!is.na(valor) &
                                 tipo_de_produto_da_aquicultura != "Total"]
aqui <- dcast(aqui
              ,formula = municipio_codigo + municipio +
                tipo_de_produto_da_aquicultura + 
                tipo_de_produto_da_aquicultura_codigo ~ 
                variavel + unidade_de_medida
              ,value.var = "valor") 
names(aqui) <- janitor::make_clean_names(names(aqui))

aqui_dt <- data.table::copy(aqui) %>% 
  .[order(producao_da_aquicultura_milheiros,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"tipo_de_produto_da_aquicultura"
                 ,"producao_da_aquicultura_milheiros"
    )] %>% 
  data.table::setnames(.
                       ,old = c("producao_da_aquicultura_milheiros"
                                , "tipo_de_produto_da_aquicultura")
                       ,new = c("valor_aqui_mais_prod_reais"
                                ,"aqui_mais_prod_reais"))
aqui_dt[is.na(valor_aqui_mais_prod_reais),valor_aqui_mais_prod_reais := 0]
aqui_dt[1:4]

# REBANHO # 3939
reb <- readr::read_rds("data/agro_table3939.rds")

reb$municipio_codigo %>% uniqueN()
reb$variavel %>% unique()
reb$tipo_de_rebanho %>% unique()
reb$unidade_de_medida %>% unique()

reb <- data.table::copy(reb)[!is.na(valor)]
reb <- reb[!(tipo_de_rebanho %like% "total")]
#reb[1]
reb <- dcast(reb
             ,formula = municipio_codigo + municipio +
               tipo_de_rebanho + 
               tipo_de_rebanho_codigo ~ 
               variavel + unidade_de_medida
             ,value.var = "valor") 
names(reb) <- janitor::make_clean_names(names(reb))
#reb[1]
reb_dt <- data.table::copy(reb) %>% 
  .[order(efetivo_dos_rebanhos_cabecas,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo"
                 ,"tipo_de_rebanho"
                 ,"efetivo_dos_rebanhos_cabecas"
    )] %>% 
  data.table::setnames(.
                       ,old = c("efetivo_dos_rebanhos_cabecas"
                                , "tipo_de_rebanho")
                       ,new = c("valor_reban_mais_prod_cabeca"
                                ,"reban_mais_prod_cabeca"))
#reb_dt[1]
reb_dt[is.na(valor_reban_mais_prod_cabeca),valor_reban_mais_prod_cabeca := 0]
reb_dt
# irri 
# 6858
irr <- readr::read_rds("data/agro_table6858.rds")
irr[1]
irr$municipio_codigo %>% uniqueN()
irr$unidade_de_medida %>% unique()
irr$variavel %>% unique()
irr$tipologia %>% unique()
irr$condicao_do_produtor_em_relacao_as_terras %>% unique()
irr$origem_da_orientacao_tecnica_recebida %>% unique()
irr$metodo_utilizado_para_irrigacao %>% unique()
irr$tipologia %>% unique()
irr$tipologia %>% unique()
irr <- irr[variavel %like% "Número de estabelecimentos agropecuários"]
irr <- irr[tipologia == "Total"]
irr <- irr[,.SD,.SDcols = c("municipio_codigo","valor","tipologia")]

irr <- irr[tipologia == "Total"]
irr <- irr[,.SD[1],by = municipio_codigo]
irr <- irr[,.SD,.SDcols = c("municipio_codigo","valor")]
setnames(irr,"valor","valor_tipo_irrig_num_estab")
irr
# merge agro
# agrO-dt1,2,3//silv_dt//aqui_dt//reb_dt
#all_muni <- data.table::copy(df_geral)
all_muni <- data.table::copy(df_geral)
all_muni[,code_muni := as.character(code_muni)]
all_muni <- merge.data.table(x = all_muni, y = agro_muni
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni <- merge.data.table(x = all_muni, y = agro_dt1
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni
all_muni <- merge.data.table(x = all_muni, y = agro_dt2
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni <- merge.data.table(x = all_muni, y = agro_dt3
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
# agro
all_muni <- all_muni[agro_rgint,on = c("code_intermediate")
                     ,":="(
                       p_rgint_estado_agro_valor_da_producao = p_rgint_estado_valor_da_producao,
                       p_rgint_bacia_valor_da_producao = p_rgint_bacia_valor_da_producao,
                       p_rgint_pais_agro_valor_da_producao = p_rgint_pais_valor_da_producao
                     )]
all_muni[, classe_muni_valor_da_producao := as.character( 
  cut(muni_valor_da_producao_mil_reais
      , breaks = Hmisc::wtd.quantile(
        x = muni_valor_da_producao_mil_reais
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c("Baixo","Baixo","Médio","Alto","Alto")))]

# silv
all_muni <- merge.data.table(x = all_muni, y = silv_dt
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni <- all_muni[silv_rgint,on = c("code_intermediate")
                     ,":="(
                       p_rgint_estado_area_silv = p_rgint_estado_area_silv,
                       p_rgint_bacia_area_silv = p_rgint_bacia_area_silv,
                       p_rgint_pais_area_silv = p_rgint_pais_area_silv
                     )]
all_muni <- merge.data.table(x = all_muni, y = silv_muni
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni[, classe_muni_valor_silv := as.character( 
  cut(muni_valor_silv
      , breaks = Hmisc::wtd.quantile(
        x = muni_valor_silv
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c("Baixo","Baixo","Médio","Alto","Alto")))]

table(all_muni$classe_muni_valor_silv)
# aqui
all_muni <- merge.data.table(x = all_muni, y = aqui_dt
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni <- all_muni[aqui_rgint,on = c("code_intermediate")
                     ,":="(
                       p_rgint_estado_area_aqui = p_rgint_estado_area_aqui,
                       p_rgint_bacia_area_aqui = p_rgint_bacia_area_aqui,
                       p_rgint_pais_area_aqui = p_rgint_pais_area_aqui
                     )]
all_muni <- merge.data.table(x = all_muni, y = aqui_muni
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
all_muni[, classe_muni_valor_aqui := as.character( 
  cut(muni_valor_aqui
      , breaks = Hmisc::wtd.quantile(
        x = muni_valor_aqui
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c("Baixo","Baixo","Médio","Alto","Alto")))]

table(all_muni$classe_muni_valor_aqui)
# reb
all_muni <- merge.data.table(x = all_muni, y = reb_dt
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)
# irr
all_muni <- merge.data.table(x = all_muni, y = irr
                             ,by.x = "code_muni"
                             ,by.y = "municipio_codigo"
                             ,all = TRUE)

# save----
googlesheets4::gs4_auth()
all_muni <- all_muni %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = all_muni
                           ,ss = link_gdocs
                           ,sheet = "AGRO") 
readr::write_rds(all_muni,"data/complexidade/complexidade_muni_prep_data/AGRO.rds")


# 19) ORIENTACAO TECNICA IRRIGACAO------ 

irr$variavel %>% unique()
irr$tipologia %>% unique()
irr[,condicao_do_produtor_em_relacao_as_terras := NULL]
irr[,grupos_de_atividade_economica  := NULL]
irr[,origem_da_orientacao_tecnica_recebida := NULL]
irr[,metodo_utilizado_para_irrigacao := NULL]
irr[,condicao_do_produtor_em_relacao_as_terras_codigo := NULL]
irr[,grupos_de_atividade_economica_codigo  := NULL]
irr[,origem_da_orientacao_tecnica_recebida_codigo := NULL]
irr[,metodo_utilizado_para_irrigacao_codigo := NULL]

irr[1]
irr <- data.table::copy(irr)[!is.na(valor)]
irr <- irr[!(tipo_de_irranho %like% "total")]
irr[1]
irr <- dcast(irr
             ,formula = municipio_codigo + municipio +
               tipo_de_irranho + 
               tipo_de_irranho_codigo ~ 
               variavel + unidade_de_medida
             ,value.var = "valor") 
names(irr) <- janitor::make_clean_names(names(irr))
irr[1]
irr_dt <- data.table::copy(irr) %>% 
  .[order(efetivo_dos_irranhos_cabecas,decreasing = TRUE)] %>%
  .[,.SD[1],by = municipio_codigo] %>% 
  .[,.SD
    ,.SDcols = c("municipio_codigo","municipio"
                 ,"tipo_de_irranho"
                 ,"efetivo_dos_irranhos_cabecas"
    )] %>% 
  data.table::setnames(.
                       ,old = c("efetivo_dos_irranhos_cabecas"
                                , "tipo_de_irranho")
                       ,new = c("valor_irran_mais_prod_cabeca"
                                ,"irran_mais_prod_cabeca"))
irr_dt[1]
irr_dt[is.na(valor_irran_mais_prod_cabeca),valor_irran_mais_prod_cabeca := 0]
irr_dt
# 20) POLITICAS URBANAS ----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

urb <- fread("data/VariavelControleUrbano.csv",dec=",")
urb

all_muni <- data.table::copy(df_geral)


urb[,cod_municipio := as.character(cod_municipio)]
all_muni <- all_muni[urb,on = c("code_muni" = "cod_municipio")]
all_muni[1:4]

all_muni <- all_muni[,.SD,.SDcols = c(names(df_geral),"total_con_urb")]

# save----
googlesheets4::gs4_auth()
all_muni <- all_muni %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = all_muni
                           ,ss = link_gdocs
                           ,sheet = "URBANISMO") 

readr::write_rds(all_muni,"data/complexidade/complexidade_muni_prep_data/URBANISMO.rds")

# 21) POLITICA RURAL ----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

irr <- readr::read_rds("data/agro_table6858.rds")
irr[1]

irr$ano %>% unique()
irr$origem_da_orientacao_tecnica_recebida %>% unique()
irr$condicao_do_produtor_em_relacao_as_terras %>% unique()
irr$grupos_de_atividade_economica   %>% unique()
irr$metodo_utilizado_para_irrigacao  %>% unique()
irr$condicao_do_produtor_em_relacao_as_terras_codigo  %>% unique()
irr$grupos_de_atividade_economica_codigo   %>% unique()
irr$origem_da_orientacao_tecnica_recebida_codigo  %>% unique()
irr$metodo_utilizado_para_irrigacao_codigo  %>% unique()
irr$variavel %>% unique()
irr$tipologia %>% unique()
irr$variavel %>% unique()
irr$unidade_de_medida %>% unique()


# estab total
irr1 <- copy(irr)
irr1 <- irr1[origem_da_orientacao_tecnica_recebida == "Total" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia == "Total" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Número de estabelecimentos agropecuários com uso de irrigação" & 
               unidade_de_medida == "Unidades"]

nrow(irr1)
irr1 <- irr1[,.SD[1], by = municipio_codigo]
irr1 <- irr1[,.SD,.SDcols = c("municipio_codigo","valor")]
setnames(irr1,"valor","num_estab_total")

# agricultura familiar
irr2 <- copy(irr)
irr2 <- irr2[origem_da_orientacao_tecnica_recebida == "Total" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia %like% "Agricultura familiar" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Número de estabelecimentos agropecuários com uso de irrigação" & 
               unidade_de_medida == "Unidades"]

nrow(irr2)
irr2 <- irr2[,.SD[1], by = municipio_codigo]
irr2 <- irr2[,.SD,.SDcols = c("municipio_codigo","valor")]
setnames(irr2,"valor","num_estab_agr_familiar")

irr2[, classe_num_estab_agr_familiar := as.character( 
  cut(num_estab_agr_familiar
      , breaks = Hmisc::wtd.quantile(
        x = num_estab_agr_familiar
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

# classe_area_irrig_orient_tec
irr3 <- copy(irr)
irr3 <- irr3[origem_da_orientacao_tecnica_recebida == "Total" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia == "Total" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Área irrigada dos estabelecimentos agropecuários" & 
               unidade_de_medida == "Hectares"]

nrow(irr3)
irr3 <- irr3[,.SD[1], by = municipio_codigo]
irr3 <- irr3[,.SD,.SDcols = c("municipio_codigo","valor")]

irr3[, classe_area_irrig_orient_tec := as.character( 
  cut(valor
      , breaks = Hmisc::wtd.quantile(
        x = valor
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

irr3[,valor := NULL]

# classe_num_estab_orient_total

irr31 <- copy(irr)
irr31 <- irr31[origem_da_orientacao_tecnica_recebida == "Total" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia == "Total" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Número de estabelecimentos agropecuários com uso de irrigação" & 
               unidade_de_medida == "Unidades"]

irr31 <- irr31[,.SD[1], by = .(municipio_codigo,origem_da_orientacao_tecnica_recebida)]
irr31 <- irr31[,sum(valor,na.rm = TRUE),by = .(municipio_codigo)]

irr31[, classe_num_estab_orient_total := as.character( 
  cut(V1
      , breaks = Hmisc::wtd.quantile(
        x = V1
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

irr31[,V1 := NULL]

# classe_num_estab_orient_outrasNAOgov

irr4 <- copy(irr)
irr4 <- irr4[origem_da_orientacao_tecnica_recebida != "Governo (federal, estadual ou municipal)" &
               origem_da_orientacao_tecnica_recebida != "Total" &
               origem_da_orientacao_tecnica_recebida != "Não recebe" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia == "Total" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Número de estabelecimentos agropecuários com uso de irrigação" & 
               unidade_de_medida == "Unidades"]

irr4 <- irr4[,.SD[1], by = .(municipio_codigo,origem_da_orientacao_tecnica_recebida)]
irr4 <- irr4[,sum(valor,na.rm = TRUE),by = .(municipio_codigo)]

irr4[, classe_num_estab_orient_outrasNAOgov := as.character( 
  cut(V1
      , breaks = Hmisc::wtd.quantile(
        x = V1
        ,probs=seq(0, 1, by=0.2)
        , na.rm=T
      ),
      include.lowest= TRUE
      , labels= c(0.25,0.25,0.5,0.75,0.75)))]

irr4[,V1 := NULL]

# classe_orient_tec_xxxxxxx

irr5 <- copy(irr)
irr5 <- irr5[origem_da_orientacao_tecnica_recebida != "Total" &
               origem_da_orientacao_tecnica_recebida != "Não recebe" &
               origem_da_orientacao_tecnica_recebida != "Recebe" &
               origem_da_orientacao_tecnica_recebida != "Outra" &
               condicao_do_produtor_em_relacao_as_terras == "Total" & 
               metodo_utilizado_para_irrigacao == "Total" & 
               tipologia == "Total" & 
               grupos_de_atividade_economica == "Total" & 
               variavel == "Número de estabelecimentos agropecuários com uso de irrigação" & 
               unidade_de_medida == "Unidades"]

setnames(irr5,"origem_da_orientacao_tecnica_recebida","origem_orint_tec")


irr5$origem_orint_tec %>% unique()
irr5[,origem_orint_tec :=
       data.table::fcase(
         origem_orint_tec == "Sistema S","sistemaS",
         origem_orint_tec == "Própria ou do próprio produtor","proprio_produtor",
         origem_orint_tec == "Cooperativas","cooperativas",
         origem_orint_tec == "Empresas privadas de planejamento","empresas",
         origem_orint_tec == "Empresas integradoras","empresas",
         origem_orint_tec == "Organização não-governamental (ONG)","ong",
         origem_orint_tec == "Governo (federal, estadual ou municipal)","gov"
       )]

irr5[, classe_orint_tec := {
  my_ranks <- rank(valor, ties.method = "first")
  decile <- cut(my_ranks
                , quantile(my_ranks, probs=0:5/5)
                , include.lowest=TRUE
                , labels=FALSE
  )
}
, by = .(origem_orint_tec)]


irr5[,classe_orint_tec := fcase(classe_orint_tec == 1,0.25,
                                classe_orint_tec == 2,0.25,
                                classe_orint_tec == 3,0.50,
                                classe_orint_tec == 4,0.75,
                                classe_orint_tec == 5,0.75)]

irr5 <- data.table::dcast(data = irr5
                          ,formula = municipio_codigo  ~ paste0("classe_orint_tec_",origem_orint_tec)
                          ,value.var = "classe_orint_tec"
                          ,fun.aggregate = sum
                          ,fill = NA)

irr5
# merge
dt_irr <- df_geral %>%
  merge.data.table(x = ., y = irr1
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) %>% 
  merge.data.table(x = ., y = irr2
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) %>% 
  merge.data.table(x = ., y = irr3
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) %>% 
  merge.data.table(x = ., y = irr31
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) %>% 
  merge.data.table(x = ., y = irr4
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) %>% 
  merge.data.table(x = ., y = irr5
                   ,by.x = "code_muni"
                   ,by.y = "municipio_codigo"
                   ,all = TRUE) 

dt_irr[,soma_classes_pol_rurais := 
         as.numeric(classe_area_irrig_orient_tec) +
         as.numeric(classe_num_estab_agr_familiar) +
         as.numeric(classe_num_estab_orient_total) +
         as.numeric(classe_num_estab_orient_outrasNAOgov)]

# save----
readr::write_rds(dt_irr,"data/complexidade/complexidade_muni_prep_data/POLIT_RURAL.rds")


googlesheets4::gs4_auth()
dt_irr <- dt_irr %>% format(.,decimal.mark = ",") %>% as.data.frame() %>% setDT()
googlesheets4::write_sheet(data = dt_irr
                           ,ss = link_gdocs
                           ,sheet = "POLIT. RURAL") 


# 22) DESASTRES NATURAIS  ----
rm(list = ls()[!(ls() %in% c("input_file","link_gdocs","my_division","df_geral"))])
gc(reset = TRUE)

desn <- openxlsx::read.xlsx("data-raw/Banco_de_Dados_Completo_Dicionário/DesNaturais.xlsx"
                            ,sheet = "Planilha1")
readr::write_rds(desn,"data/complexidade/complexidade_muni_prep_data/DES_NATURAL.rds")


# end -----
