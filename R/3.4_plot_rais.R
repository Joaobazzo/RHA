rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)
library(patchwork)

# le
tmp_cnae <- readr::read_rds("data/CNAE_empregos.rds")
tmp_all <- tmp_cnae$raw_CNAE
my_division <- function(a,b){round(100 * a / b , 2)}

# Read data ------
input_file <- readr::read_rds("data/munis_list.rds")
# prep 

make_plot <- function(sigla_input,sigla_name){
  
  tmp_plot <- tmp_all %>% 
    .[regiao_intermediaria == sigla_input] %>% 
    .[,list(
      vinculos = sum(contagem) %>% as.numeric()
      , vinculo_cap = sum(soma_remun) / sum(contagem)
    ),by = .(tamanho_vinculo)] 
  
  
  tmp_plot <-  melt(data = tmp_plot
                    ,id.vars = "tamanho_vinculo"
                    ,measure.vars = c("vinculos","vinculo_cap")
                    ,value.name = "valor"
                    ,variable.name = "tipo_vinculo"
  )
  
  
  tmp_plot[,tamanho_vinculo_f := factor(tamanho_vinculo
                                        ,levels = c("de 1 a 9 vínculos" 
                                                    , "de 10 a 19 vínculos"
                                                    , "de 20 a 49 vínculos"
                                                    , "de 50 a 99 vínculos" 
                                                    , "de 100 a 499 vínculos"
                                                    , "+ de 500 vínculos")
                                        ,labels = c("de 1 a 9" 
                                                    , "de 10 a 19"
                                                    , "de 20 a 49"
                                                    , "de 50 a 99" 
                                                    , "de 100 a 499"
                                                    , "+ de 500")
  )]
  tmp_plot[,tipo_vinculo_f := factor(tipo_vinculo
                                     ,levels = c("vinculos","vinculo_cap")
                                     ,labels = c("Número total de vínculos","Remuneração per capita (R$)")
  )]
  
  plot_pop <- ggplot(tmp_plot) +
    geom_col(aes(x = tamanho_vinculo_f,y = valor)
             ,fill = '#00324a',alpha = 0.75,width = 0.75)+
    facet_wrap(~tipo_vinculo_f,nrow = 2,scales = "free")+
    labs(x = "Número de funcionários", y = NULL
         , subtitle = sprintf("%s (%s)",sigla_name,sigla_input),)+
    theme_minimal()
  
  dir.create("figures/vinculo_cnae/") %>% suppressWarnings()
  
  ggplot2::ggsave(filename = sprintf("figures/pop_situacao_rgint/%s.jpg"
                                     ,sigla_input)
                  ,plot = plot_pop
                  ,scale = 0.65
                  ,width = 20
                  ,height = 20
                  ,bg = "white"
                  ,units = "cm"
                  ,dpi = 300)
}

my_sigla_input <- unique(input_file$intermediate_region$code_intermediate)
my_sigla_name <-  unique(input_file$intermediate_region$name_intermediate)

lapply(seq_along(my_sigla_input),function(i){ # i = 1 
  make_plot(sigla_input = my_sigla_input[i]
            ,sigla_name = my_sigla_name[i])
})