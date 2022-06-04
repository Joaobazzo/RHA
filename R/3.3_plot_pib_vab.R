# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)

# Read data ------
input_file <- readr::read_rds("data/munis_list.rds")

pib_raw <- readr::read_rds("data/pib_total_prep.rds")

make_plot <- function(sigla_input,sigla_name){
 #sigla_name = "Caicó"
 #sigla_input = "2402"
  
  my_input <- data.table::copy(input_file$intermediate_region)[
    code_intermediate %in% as.character(sigla_input)
    ,
  ] 
  
  my_pib <- data.table::copy(pib_raw)[
    municipio_codigo %in% as.character(unique(my_input$code_muni))
    ,
  ]
  
  my_pop <- data.table::copy(input_file$pop_censo_br)[
    municipio_codigo %in% as.character(unique(my_input$code_muni))
    ,
  ]
  # prepare plot ----
  
  pib_br1 <- data.table::copy(my_pib) %>% 
    .[!(variavel_codigo %in% c("37","498")),] %>% 
    .[,vab := sum(valor,na.rm = TRUE),by = .(ano,name_intermediate,variavel_codigo)] %>% 
    .[,.SD[1],by = .(ano,name_intermediate,variavel_codigo)] %>% 
    .[,prop := round(100 * vab/ sum(vab,na.rm = TRUE),1), by = .(ano,name_intermediate)] %>% 
    .[,.SD,.SDcols = c("ano","name_intermediate","vab","prop","variavel")] %>% 
    data.table::setnames(.,"variavel","tipo_vab")
  
  pib_br2 <- data.table::melt(data = pib_br1
                              ,id.vars = c("tipo_vab","ano")
                              ,measure.vars = c("vab","prop"))
  pib_br2
  
  # condition population size
  pib_br2[,value_vab := data.table::fifelse(test = (variable == "vab") & (max(value) > 900000)
                                            ,yes = value / (10^6)
                                            ,no = value / (10^3) )]
  pib_br2[,value_vab := data.table::fifelse(test = (variable == "prop")
                                            ,yes = value, no = value_vab )]
  name_title <- data.table::fifelse(nchar(max(pib_br2$value)) >= 7
                                    ,"Produto Interno Bruto (bilhões R$)"
                                    ,"Produto Interno Bruto (milhões R$)")
  
  
  pib_br2[tipo_vab == "Impostos, líquidos de subsídios, sobre produtos a preços correntes"
          , tipo_vab_f := "Impostos"]
  pib_br2[tipo_vab == "Valor adicionado bruto a preços correntes da agropecuária"
          , tipo_vab_f := "VAB agropecuária"]
  pib_br2[tipo_vab == "Valor adicionado bruto a preços correntes da indústria"
          , tipo_vab_f := "VAB indústria"]
  pib_br2[tipo_vab == "Valor adicionado bruto a preços correntes dos serviços, exclusive administração, defesa, educação e saúde públicas e seguridade social"
          , tipo_vab_f := "VAB serviços"]
  pib_br2[tipo_vab == "Valor adicionado bruto a preços correntes da administração, defesa, educação e saúde públicas e seguridade social"
          , tipo_vab_f := "VAB administração"]
  
  plot_pop <- ggplot(data = pib_br2) +
    geom_bar(stat = "identity",
             aes(x = ano
                 , y = value_vab
                 ,fill = tipo_vab_f),alpha = 0.65
    )+
    scale_fill_brewer(palette = "Set1")+
    #scale_fill_manual(values = c('#00324a','#cccccc'))+
    facet_wrap(~variable,ncol=1,scales = "free_y"
               ,labeller = as_labeller(c('vab' = name_title,
                                         'prop' = "Proporção (%)")))+
    labs(x = NULL, y = NULL
         ,subtitle = sprintf("%s (%s)",sigla_name,sigla_input)
         , caption = 'Fonte: IBGE (2002 - 2019)\n*VAB: Valor Adicionado Bruto'
         , fill = "Categoria")+
    theme_minimal()+
    theme(legend.position = "bottom"
          ,axis.text.x = element_text(angle = 45, vjust = 1.25, hjust=1)
          ,legend.title.align = 0.5)+
    guides(fill = guide_legend(nrow = 2, byrow = TRUE
                               ,title.position = "top"))
  
  plot_pop
  dir.create("figures/pib_vab/") %>% suppressWarnings()
  
  ggplot2::ggsave(filename = sprintf("figures/pib_vab/%s.jpg"
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
list_files_pop <- list.files("figures/pib_vab/",full.names = TRUE)
unlink(x = list_files_pop)

# loop plots
lapply(seq_along(my_sigla_input),function(i){ # 
  make_plot(sigla_input = my_sigla_input[i]
            ,sigla_name = my_sigla_name[i])
})
