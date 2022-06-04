



# 999 fazer:::
- fazer por bacia
- ordenar por produto (ProdBase)
- inserir VCR, valor exportação, ICM (Indice Complexidade do Municipio)
- Ver onde estão os dados acima 
- add potencialidade/fragilidade
- fazer templante PIB
- 
# load libraries ----

rm(list=ls())
gc(reset=T)
library(magrittr)
library(data.table)
library(geobr)
library(mapview)
library(readr)
library(ggplot2)
library(patchwork)

# read -----
munis_list <- readr::read_rds("data/munis_list.rds")
munis_list$municipality[,code_muni := as.character(code_muni)]



muni_cmplx <- data.table::fread("data-raw/Municipio/ListaProdutos_Municipio.csv")
data.table::setDT(muni_cmplx)
muni_cmplx[,Municipio := as.character(Municipio)]

# join ----

 muni_join <- data.table::copy(muni_cmplx)[
   munis_list$municipality
   , on = c("Municipio" = "code_muni")
   , name_muni := i.name_muni]
muni_join <- muni_join[!is.na(name_muni)]

# list order

lista_icpdiv <- data.table::copy(muni_join)[
  ,{
    order_ICPDiv <- order(ICPDiv,decreasing = T)[1:5]
    list(
      "RegiaoIntermediaria" = RegiaoIntermediaria[order_ICPDiv]
      ,"RegiaoImediata"     = RegiaoImediata[order_ICPDiv]
      ,"code_muni"          = Municipio[order_ICPDiv]
      ,"name_muni"          = name_muni[order_ICPDiv]
      ,"Cod_Grupo_Base"     = ProdBase[1]
      ,"Rank_Produto_Base"  = 1:3
      ,"Nome_Grupo_Base"    = GrupoBase[order_ICPDiv]
      ,"Nome_Produto_Base"  = NomeBase[order_ICPDiv]
      ,"ICP_Base"           = format(ICPBase[order_ICPDiv],dec=",")
      ,"Phi"                = format(Phi[order_ICPDiv],dec=",")
      ,"ICPDiv"             = format(ICPDiv[order_ICPDiv],dec=",")
      ,"Cor_Grupo_Div"      = ProdDiv[order_ICPDiv]
      ,"Nome_Grupo_Div"     = GrupoDiv[order_ICPDiv]
      ,"Nome_Produto_Div"   = NomeDiv[order_ICPDiv]
    )
  }
  ,by = .(ProdBase)
]

lista_icpdiv[1:3]
lista_icpdiv %>% View()
# save xlsx ----
openxlsx::write.xlsx(x = lista_icpdiv
                     ,file = "data/rank_ICPdiv_por_ProdBase.xlsx"
                     ,overwrite = TRUE)

# simple map

map_sf <- sf::st_as_sf(lista_icpdiv)
# plot ----
list_group <- unique(lista_icpdiv$GrupoBase)
p <- list()

for(i in seq_along(list_group)){
  p[[i]] <- ggplot()+
    geom_sf(data = map_sf[map_sf$bacia == "BSF" & 
                            map_sf$GrupoBase == list_group[i],]
            ,fill = "lightblue")+
    geom_sf(data = map_sf[map_sf$bacia == "BSF",]
            ,color = 'black',fill=NA)+
    labs(subtitle = fifelse(i == 7
                            ,"Artigo de Pedra\n e outros metais"
                            ,list_group[i]))+
    theme_void()
}
p[[7]]
map_20 <- setDT(map_sf)[bacia == "BSF"]
map_20[,Num := .N,by = name_muni]
map_20 <- map_20[order(Num,decreasing = T),]


pf <-  (p[[01]]|p[[02]]|p[[03]]|p[[04]]|p[[05]]) /
  (p[[16]]|p[[07]]|p[[08]]|p[[09]]|p[[10]]) /
  (p[[11]]|p[[12]]|p[[13]]|p[[14]]|p[[15]]) /
  (p[[16]]|p[[17]]|p[[18]]|p[[19]])

pf1 <- pf + 
  plot_annotation(title = '3 maiores "ICPDiv" da bacia BSF dividido por GrupoBase')
ggsave(plot = pf1
       ,filename = "figures/BSF.png"
       ,height = 20
       ,width = 25
       ,units = "cm"
       ,dpi = 300)
