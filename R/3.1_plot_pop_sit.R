# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)

# Read data ------
input_file <- readr::read_rds("data/munis_list.rds")


make_plot <- function(sigla_input,sigla_name){
  #sigla_name = "Caicó"
  #sigla_input = "2402"
  
  my_input <- data.table::copy(input_file$intermediate_region)[
    code_intermediate %in% as.character(sigla_input)
    ,
  ] 
  
  my_pop <- data.table::copy(input_file$pop_censo_br)[
    municipio_codigo %in% as.character(unique(my_input$code_muni))
    ,
  ]
  # prepare plot ----
  
  pop_br1 <- data.table::copy(my_pop) %>% 
    .[situacao_do_domicilio != "Total",] %>% 
    .[,":="(pop = sum(valor,na.rm = TRUE)),by = .(ano,situacao_do_domicilio)] %>% 
    .[,.SD[1],by = .(ano,situacao_do_domicilio)] %>% 
    .[,prop := round(100 * pop/ sum(pop,na.rm = TRUE),1), by = .(ano)]
  
  pop_br2 <- data.table::melt(data = pop_br1
                              ,id.vars = c("situacao_do_domicilio","ano")
                              ,measure.vars = c("pop"))
  # condition population size
  pop_br2[,value_pop := data.table::fifelse(test = (variable == "pop") & (max(value) > 900000)
                                        ,yes = value / (10^6)
                                        ,no = value / (10^3) )]
  pop_br2[,value_pop := data.table::fifelse(test = (variable == "prop")
                                            ,yes = value, no = value_pop )]
  name_title <- data.table::fifelse(nchar(max(pop_br2$value)) >= 7
                                    ,"População total (milhões)"
                                    ,"População total (mil)")
  #
  pop_br2[,variable_f := factor(variable
                                ,levels = c("pop")
                                ,labels = c("População"))]
  
  pop_br2
  plot_pop <- ggplot(data = pop_br2) +
    geom_bar(stat = "identity",
             aes(x = ano
                 #, y = format(value_pop,dec = ",")
                 , y = value_pop
                 ,fill = situacao_do_domicilio)
             ,width = 0.75)+
    #scale_fill_brewer(palette = "Pastel1")+
    scale_fill_manual(values = c('#00324a','#cccccc'))+
    facet_wrap(~variable,ncol=1,scales = "free_y",
               labeller = as_labeller(c('prop' = "Proporção da população (%)",
                                        'pop' = name_title)))+
    labs(x = NULL, y = NULL,
         subtitle = sprintf("%s (%s)",sigla_name,sigla_input),
         caption = 'Fonte: Censos IBGE (1991, 2000, 2010)',
         fill="Situação do \ndomicílio")+
    theme_minimal()
  plot_pop
  
  dir.create("figures/pop_situacao_rgint/") %>% suppressWarnings()
  
  ggplot2::ggsave(filename = sprintf("figures/pop_situacao_rgint/%s.jpg"
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

my_sigla_input <- unique(input_file$intermediate_region$code_intermediate)
my_sigla_name <-  unique(input_file$intermediate_region$name_intermediate)

# exclude files
list_files_pop <- list.files("figures/pop_situacao_rgint/",full.names = TRUE)
unlink(x = list_files_pop)

# loop plots
lapply(seq_along(my_sigla_input),function(i){ # 
  make_plot(sigla_input = my_sigla_input[i]
            ,sigla_name = my_sigla_name[i])
})
