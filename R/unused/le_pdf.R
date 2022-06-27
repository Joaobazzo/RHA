rm(list=ls())

library(data.table)
library(magrittr)
library(ggplot2)
library(stringr)
library(tabulizer)


# le inicio
out3 <- tabulizer::extract_areas("data-raw/nt522017-pdf.pdf",pages = 15)

out3
name_out3 <- sapply(1:8,function(i){out3[[1]][1:5,i] %>% paste(.,collapse = " ")})

out3_df <- as.data.frame(out3[[1]][-c(1:5),])
names(out3_df) <- name_out3
setDT(out3_df)

# le fim

dt <- lapply(16:215,function(i){ # i = 21
  message(i)
  out2 <- tabulizer::extract_tables("data-raw/nt522017-pdf.pdf",pages = i)
  out2 <- as.data.frame(out2)
  return(out2)
})
