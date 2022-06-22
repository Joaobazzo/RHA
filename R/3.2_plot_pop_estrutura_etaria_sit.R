# 1) Load packages ----
rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)

# Read data ------
input_file <- readr::read_rds("data/munis_list.rds")
estet_raw <- readr::read_rds("data/IBGE_estrutura_etaria_situacao_domicilio.rds")

# function plot ------

make_plot <- function(sigla_input,sigla_name){
  
  #sigla_name = "Caicó"
  #sigla_input = "2402"
  
  
  tmp_plot <- est[code_intermediate == sigla_input 
                  ,sum(valor,na.rm = TRUE)
                  ,by = .(situacao_do_domicilio
                          ,idade
                          ,idade_codigo
                          ,ano_codigo
                          ,variavel
                          ,code_intermediate)]
  
  tmp_plot[,pop_mil := fifelse(situacao_do_domicilio == "Rural"
                               ,-V1 / 1000
                               , V1 / 1000)]
  
  factor_idade <- 
    c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 17 anos", "18 ou 19 anos", "20 a 24 anos"
      , "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"
      , "50 a 54 anos", "55 a 59 anos","60 a 64 anos", "65 a 69 anos"
      ,"70 a 74 anos", "75 a 79 anos", "80 a 89 anos", "90 a 99 anos")
  tmp_plot[,idade_f := factor(
    x = idade
    ,levels = factor_idade
    ,labels = gsub(" anos","",factor_idade)
  ),by = situacao_do_domicilio]
  tmp_plot[
    , percent := (100 * (V1 / sum(V1))) %>% round(.,1) %>% format(.,dec = ",") %>% paste0(.,"%")
    , by = situacao_do_domicilio]
  
  
  plot_pop <- ggplot(tmp_plot) +
    geom_col(aes(x = pop_mil
                 ,y = idade_f
                 ,fill = situacao_do_domicilio)
             ,color = "black", alpha = 0.75) +
    scale_fill_manual(values = c('#a1d99b','#cccccc'))+
    geom_text(
      aes(x = pop_mil
          ,y = idade_f
          ,label = percent
          , hjust = fifelse(situacao_do_domicilio == "Urbana",-0.25,+1.1)
      )
      , size = 3
      #, vjust = 1
    ) + 
    labs(x = "População (mil hab.)", y = "Faixa etária (anos)",
         subtitle = sprintf("%s (%s)",sigla_name,sigla_input),
         caption = 'Fonte: Censo IBGE (2010)',
         fill = "Situação do domicílio")+
    scale_x_continuous(limits = 1.15*c(-max(abs(tmp_plot$pop_mil))
                                  ,max(abs(tmp_plot$pop_mil)))
                         #,expand = c(0.25, 0.25)
                       , breaks = seq(-max(abs(tmp_plot$pop_mil))
                                      ,max(tmp_plot$pop_mil)
                                      ,length.out = 12)
                       , labels = seq(-max(abs(tmp_plot$pop_mil))
                                      ,max(tmp_plot$pop_mil)
                                      ,length.out = 12) %>% abs() %>% round(0)
    ) + 
    theme_bw() + 
    theme(legend.position = "bottom")
  
  plot_pop
  
  ggplot2::ggsave(filename = sprintf("figures/estrutura_etaria_sit_rgint/%s_v1.jpg"
                                     ,sigla_input)
                  ,plot = plot_pop
                  ,scale = 0.65
                  ,width = 20
                  ,height = 20
                  ,bg = "white"
                  ,units = "cm"
                  ,dpi = 300)
  return(NULL)
}



# apply loop function -----
dir.create("figures/estrutura_etaria_sit_rgint/") %>% suppressWarnings()

est <- data.table::copy(estet_raw)

my_sigla_input <- unique(input_file$intermediate_region$code_intermediate)
my_sigla_name <-  unique(input_file$intermediate_region$name_intermediate)

# exclude files
list_files_pop <- list.files("figures/estrutura_etaria_sit_rgint/",full.names = TRUE)
unlink(x = list_files_pop)

# loop plots
lapply(seq_along(my_sigla_input),function(i){ # seq_along(my_sigla_input)
  make_plot(sigla_input = my_sigla_input[i]
            ,sigla_name = my_sigla_name[i])
})
