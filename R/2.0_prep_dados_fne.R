dt_fne_raw <- openxlsx::read.xlsx("data-raw/FNE/FNE - 2020_VALOR_CONTRATADO.xlsx")
names(dt_fne_raw) <- janitor::make_clean_names(names(dt_fne_raw))
data.table::setDT(dt_fne_raw)
dt_fne_raw$codigo_municipio
dt_fne_raw[df_geral,on = c("codigo_municipio" = "code_muni")
           ,code_intermediate := i.code_intermediate]
dt_fne_v1 <- dt_fne_raw[,sum(valor_contratado,na.rm = TRUE)
                        ,by = .(tipo_pessoa,code_intermediate,urbano_rural,programa)]
dt_fne_v1 <- dt_fne_v1 %>% 
  dcast(code_intermediate + programa + urbano_rural ~ tipo_pessoa,value.var = "V1",fill = 0) %>% 
  .[,Total := PF + PJ] 


dt_fne_v2 <- dt_fne_raw[,sum(valor_contratado,na.rm = TRUE)
                        ,by = .(code_intermediate,finalidade,setor,urbano_rural)] 
dt_fne_v2 <-  dt_fne_v2 %>% 
  dcast(code_intermediate + setor + urbano_rural ~ finalidade,value.var = "V1",fill = 0) %>% 
  .[,Total := `Capital de giro isolado` +  `Comercialização` + 
      `Custeio isolado` + Investimento]

readr::write_rds(dt_fne_v1,"data/fne_q25.rds")
readr::write_rds(dt_fne_v2,"data/fne_q26.rds")
