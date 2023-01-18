gc(reset = TRUE)
library(readr)
library(ggplot2)
library(data.table)
library(magrittr)
library(officedown)
library(flextable)
library(officer)

# ListaProdutos-----------
find <- list.files("/home/joaobazzo/EspacoProduto"
                   ,pattern = "ListaProdutos"
                   ,recursive = TRUE
                   ,full.names = TRUE)
find <- find[!(find %like% "old.csv")]
find <- find[!(find %like% "aPS06_")]
find <- sort(find)
find <- find[-2]
find
dt_ListaProdutos <- lapply(find,fread) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
names(dt_ListaProdutos)
dt_ListaProdutos[is.na(Regiao),Regiao := fcase(
  !is.na(Municipio),Municipio,
  !is.na(RegiaoImediata),RegiaoImediata,
  !is.na(RegiaoIntermediaria),RegiaoIntermediaria,
  !is.na(UF),UF
)] %>% setnames(.,"Regiao","code_muni") %>% 
  .[,code_muni := as.character(code_muni)]

readr::write_rds(dt_ListaProdutos,"data/complexidade/ListaProdutos_all_regions.rds")


# ListaAtividadesDiv-----------
find <- list.files("/home/joaobazzo/EspacoAtividade"
           ,pattern = "ListaAtividadesDiv"
           ,recursive = TRUE
           ,full.names = TRUE)
find <- find[!(find %like% "aAS06_")]
find <- find[!(find %like% "Old.csv")]
find <- sort(find)
find
find[4]

dt <- fread(find[4])
dt[,uniqueN(ActivBase),by = .(RegiaoIntermediaria)]
dt[RegiaoIntermediaria == s_input,]$NomeBase %>% unique()
dt[RegiaoIntermediaria == s_input,]$NomeDiv %>% unique() %>% paste0(.,collapse = ", ")
dt_ListaAtividadesDiv <- lapply(find,fread) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
dt_ListaAtividadesDiv[is.na(Regiao),][1:5]
dt_ListaAtividadesDiv[is.na(Regiao),Regiao := fcase(
  !is.na(Municipio),Municipio,
  !is.na(RegiaoImediata),RegiaoImediata,
  !is.na(RegiaoIntermediaria),RegiaoIntermediaria,
  !is.na(UF),UF
)] %>% setnames("Regiao","code_muni") %>% 
  .[,code_muni := as.character(code_muni)]
dt_ListaAtividadesDiv

readr::write_rds(dt_ListaAtividadesDiv,"data/complexidade/dt_ListaAtividadesDiv_all_regions.rds")

# ActivK-----------
find <- list.files("/home/joaobazzo/EspacoAtividade"
                   #,pattern = "ActivKDivers"
                   ,pattern = "ActivKDivers"
                   ,recursive = TRUE
                   ,full.names = TRUE)
find <- sort(find)
find
dt_ActivK <- lapply(find[3],fread) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
dt_ActivK <- dt_ActivK[Ano == 2019]
dt_ActivK[is.na(Region),Region := Reg] %>% setnames("Region","code_muni") %>% 
  .[,code_muni := as.character(code_muni)]

readr::write_rds(dt_ActivK,"data/complexidade/dt_ActivK_all_regions.rds")
# PSICE_-----------
find <- list.files("/home/joaobazzo/EspacoProduto"
                   ,pattern = "PSICE"
                   ,recursive = TRUE
                   ,full.names = TRUE)
find <- sort(find)
dt_PSICE <- lapply(find,fread) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
dt_PSICE <- dt_PSICE %>% .[Ano == 2020,] %>% .[,V1 := NULL] %>% 
  .[is.na(Region),Region := Mun] %>% 
  .[is.na(ICE),ICE := ICEMediano] %>% 
  .[,":="(ICEMediano = NULL,ICEMedio = NULL,Mun = NULL,Ano = NULL)] %>%
   setnames(.,"Region","code_muni") %>% 
  .[,code_muni := as.character(code_muni)]
dt_PSICE[]

readr::write_rds(dt_PSICE,"data/complexidade/dt_PSICE_all_regions.rds")

# PSExpY-----------
find <- list.files("/home/joaobazzo/EspacoProduto"
                   ,pattern = "PSExpY"
                   ,recursive = TRUE
                   ,full.names = TRUE)
find <- sort(find)
dt_PSExpY <- lapply(find,fread) %>% data.table::rbindlist(use.names = TRUE,fill = TRUE)
dt_PSExpY <- dt_PSExpY %>% .[Ano == 2018,] %>% .[,V1 := NULL] %>% 
.[is.na(Region),Region := Reg] %>% 
.[is.na(ExpY),ExpY := ExpYmedio] %>% 
  .[,":="(ExpYmedio = NULL,ExpYmediano = NULL)] %>%
  setnames("Region","code_muni") %>% 
  .[,code_muni := as.character(code_muni)]
dt_PSExpY[]

readr::write_rds(dt_PSExpY,"data/complexidade/dt_PSExpY_all_regions.rds")


