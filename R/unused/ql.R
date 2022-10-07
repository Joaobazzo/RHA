# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(googlesheets4)


link_ql1 <- "https://docs.google.com/spreadsheets/d/1VWNaPbITGLOV_naewhAHvw8kF21N3bFc_93AEEQVO_k/edit#gid=1999503180"
link_ql2 <- "https://docs.google.com/spreadsheets/d/18WSYiAYYZvwOtBLkgqyvjLI_p4yo1M8fxHE4V6M0IFM/edit#gid=684292618"


googlesheets4::gs4_auth_configure()
googlesheets4::gs4_auth()
df_ql1 <- googlesheets4::read_sheet(ss = link_ql1,sheet = "DADOS") 
setDT(df_ql1)
df_ql1

df_ql2 <- googlesheets4::read_sheet(ss = link_ql2,sheet = "Sheet 1") 
setDT(df_ql2)
df_ql2[,cnae20 := as.character(cnae20)]

df_ql1 <- df_ql1[df_ql2, on = c("ActivBase" = "cnae20")
                 ,total_de_empregos_por_cnae20 := i.total_de_empregos_por_cnae20]


googlesheets4::write_sheet(data = df_ql1
                           ,ss = link_ql1
                           ,sheet = "DADOS_1") 
