
# load packages -----


library(magrittr)
library(data.table)
library(geobr)
library(sidrar)
#library(mapview)
library(readr)
rm(list=ls())
gc(reset=T)

# 1) read_data ------
pib_raw <- openxlsx::read.xlsx("data-raw/vab/Planilha Completa.xlsx")
setDT(pib_raw)
names(pib_raw) <- janitor::make_clean_names(names(pib_raw))
pib_raw <- pib_raw[,.SD,.SDcols = names(pib_raw)[1:29]]

# save-----
readr::write_rds(x = pib_raw
                 ,file = "data/pib_total_prep.rds"
                 ,compress = "gz")
