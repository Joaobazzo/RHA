rm(list=ls())
library(magrittr)
library(data.table)

raw_CNAE = fread("data-raw/Empregos e remuneracao/ActivitySpace2019.csv",encoding="UTF-8")
dic_raw_CNAE = fread("data-raw/Empregos e remuneracao/classeCnae.csv",encoding = "UTF-8")
grupo_raw_CNAE = fread("data-raw/Empregos e remuneracao/cnae_grupos.csv",encoding = "UTF-8")

input_file <- geobr::read_municipality()
capitais_raw <- geobr::read_state()

data.table::setDT(input_file)
data.table::setDT(capitais_raw) 
  
raw_CNAE[1:5] %>% str()
dic_raw_CNAE[1:5]
grupo_raw_CNAE[1:4]

# dic_raw_CNAE | classe
dic_raw_CNAE[,ID_grupo  := stringr::str_sub(ID,1,1)]
dic_raw_CNAE[,CNAE20 := stringr::str_sub(ID,2,10)]
dic_raw_CNAE[,CNAE20_int := as.integer(CNAE20)]
data.table::setnames(dic_raw_CNAE
                     ,old = c("ID","Nome","Total de Empregos")
                     , new = c("ID_classe","Nome_classe","Emprego_classe"))

# dic grupo_raw_CNAE
grupo_raw_CNAE[1:4]
data.table::setnames(grupo_raw_CNAE
                     ,old = c("ID","Nome","Total de Empregos")
                     , new = c("ID_grupo","Nome_grupo","Emprego_grupo"))

#  raw_CNAE
raw_CNAE[dic_raw_CNAE
         , on = c("CNAE20" = "CNAE20_int")
         ,":="(
           Nome_classe = Nome_classe
           ,ID_classe = ID_classe
           ,ID_grupo = ID_grupo)]
raw_CNAE[grupo_raw_CNAE
         , on = c("ID_grupo")
         ,":="(Nome_grupo = Nome_grupo)]

#  make clean names
names(raw_CNAE) <- janitor::make_clean_names(names(raw_CNAE))

# tamanho de vinculo

raw_CNAE[,tamanho_vinculo := fcase(contagem >= 500,"+ de 500 vínculos",
                                   contagem < 500  & contagem >= 100,"de 100 a 499 vínculos",
                                   contagem < 100  & contagem >= 50 ,"de 50 a 99 vínculos",
                                   contagem < 50  & contagem >= 20  ,"de 20 a 49 vínculos",
                                   contagem < 20  & contagem >= 10  ,"de 10 a 19 vínculos",
                                   contagem < 10  & contagem >= 1   ,"de 1 a 9 vínculos")]

# add estado
raw_CNAE[input_file
         , on = c("codigo_municipio" = "code_muni")
         ,abbrev_state := i.abbrev_state]                                 
raw_CNAE[capitais_raw
         , on = c("abbrev_state")
         ,name_state := i.name_state]   


raw_CNAE[,bacia := fcase(mun_pisf == 1,"PISF",
                         mun_bsf == 1,"BSF",
                         mun_bpar == 1,"BPAR")]

# add contas
sdt_jobs <- list(
   data.table::copy(raw_CNAE)[regiao_total == 1,][,local_id := regiao_intermediaria]      # RGINT
  ,data.table::copy(raw_CNAE)[!is.na(bacia),][,local_id := bacia]                         # bacia
  ,data.table::copy(raw_CNAE)[regiao_total == 1,][,local_id := "Região Total do Projeto"] # Reg. total
  ,data.table::copy(raw_CNAE)[,local_id := abbrev_state]                                  # Estado
  ,data.table::copy(raw_CNAE)[,local_id := "Brasil"]                                      # Brasil
) %>% data.table::rbindlist()


sdt_jobs <- sdt_jobs[,lapply(.SD,sum,na.rm = TRUE)
    , by =.(local_id)
    , .SDcols = c("soma_remun", "soma_horas", "contagem")]
sdt_jobs <- sdt_jobs[!is.na(local_id),]

# salva
readr::write_rds(list("raw_CNAE" = raw_CNAE
                      ,"resumo" = sdt_jobs),"data/CNAE_empregos.rds",compress = "gz")
