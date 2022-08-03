library(data.table)
library(magrittr)

# AGRO ok -----
info2020 <- sidrar::info_sidra(x = 5457)


info2020$variable$cod
VecLoop <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25
             , 26, 27, 28, 29, 31, 32, 33, 35, 41, 42
             , 43, 50, 51, 52, 53)
VarLoop <- c(8331, 1008331, 216, 1000216, 214, 112, 215, 1000215)
CatLoop <- c(0, 40129, 40092, 45982, 40329, 40130
             , 40099, 40100, 40101, 40102, 40103
             , 40131, 40136, 40104, 40105, 40137
             , 40468, 40138, 40139, 40140, 40141
             , 40330, 40106, 40331, 40142, 40143
             , 40107, 40108, 40109, 40144, 40145
             , 40146, 40147, 40110, 40111, 40112
             , 40148, 40113, 40114, 40149, 40150
             , 40115, 40151, 40152, 40116, 40260
             , 40117, 40261, 40118, 40119, 40262
             , 40263, 40264, 40120, 40121, 40122
             , 40265, 40266, 40267, 40268, 40269
             , 40123, 40270, 40124, 40125, 40271
             , 40126, 40127, 40128, 40272, 40273, 40274)
popList <- lapply(VecLoop,function(i){
  tmp <- lapply(VarLoop,function(j){
    tmp <- lapply(CatLoop,function(k){
      message(sprintf("State %s | Var %s | Cat %s",i,j,k))
      pop202 <- sidrar::get_sidra(x = 5457
                                  , variable = c(j)#c(8331, 1008331, 216, 1000216, 214, 112, 215, 1000215)
                                  , period = "2019"
                                  , geo = "City"
                                  , classific = c("c782")
                                  , category = list(k)
                                  , geo.filter = list("State" = i)
      )
      return(data.table::as.data.table(pop202))
    }) %>% data.table::rbindlist()
    return(tmp)
  }) %>% data.table::rbindlist()
  return(tmp)
}) %>% data.table::rbindlist()


names(popList) <- janitor::make_clean_names(names(popList))

readr::write_rds(popList,"data/agro_table5457.rds")

# SILVICULTURA ok-----------

info2020 <- sidrar::info_sidra(x = 5930)

info2020
info2020$variable$cod
#VecLoop <- c(21,22,23,24,25,26,27,28,31,32,52,53)
VarLoop <- c(6549)
CatLoop <- c(0,39326,39327,39328)

popList <- lapply(VecLoop,function(i){
  tmp <- lapply(VarLoop,function(j){
    tmp <- lapply(CatLoop,function(k){
      message(sprintf("State %s | Var %s | Cat %s",i,j,k))
      pop202 <- sidrar::get_sidra(x = 5930
                                  , variable = c(j)#c(8331, 1008331, 216, 1000216, 214, 112, 215, 1000215)
                                  , period = "2020"
                                  , geo = "City"
                                  , classific = c("c734")
                                  , category = list(k)
                                  , geo.filter = list("State" = i)
      )
      return(data.table::as.data.table(pop202))
    }) %>% data.table::rbindlist()
    return(tmp)
  }) %>% data.table::rbindlist()
  return(tmp)
}) %>% data.table::rbindlist()


names(popList) <- janitor::make_clean_names(names(popList))

readr::write_rds(popList,"data/agro_table5930.rds")

# AQUICULTURA ok -----


info2020 <- sidrar::info_sidra(x = 3940)

info2020
info2020$variable$cod %>% as.numeric()
tmp_cat <- info2020$classific_category$`c654 = Tipo de produto da aquicultura (25):` %>% as.data.frame()
tmp_cat <- c(as.numeric(tmp_cat$cod),as.numeric(tmp_cat$desc))
tmp_cat <- tmp_cat[!is.na(tmp_cat)]

VecLoop <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25
             , 26, 27, 28, 29, 31, 32, 33, 35, 41, 42
             , 43, 50, 51, 52, 53)
VarLoop <- c(4146,215,1000215)
CatLoop <- tmp_cat

popList <- lapply(VecLoop,function(i){
  tmp <- lapply(VarLoop,function(j){
    tmp <- lapply(CatLoop,function(k){
      message(sprintf("State %s | Var %s | Cat %s",i,j,k))
      pop202 <- sidrar::get_sidra(x = 3940
                                  , variable = c(j)
                                  , period = "2020"
                                  , geo = "City"
                                  , classific = c("c654")
                                  , category = list(k)
                                  , geo.filter = list("State" = i)
      )
      return(data.table::as.data.table(pop202))
    }) %>% data.table::rbindlist()
    return(tmp)
  }) %>% data.table::rbindlist()
  return(tmp)
}) %>% data.table::rbindlist()


names(popList) <- janitor::make_clean_names(names(popList))

readr::write_rds(popList,"data/agro_table3940.rds")

# REBANHOS ok  -----


info2020 <- sidrar::info_sidra(x = 3939)

info2020
info2020$variable$cod %>% as.numeric()
tmp_cat <- info2020$classific_category$`c79 = Tipo de rebanho (10):`[,1]
tmp_cat <- as.numeric(tmp_cat)

#VecLoop <- c(21,22,23,24,25,26,27,28,31,32,52,53)
VarLoop <- c(105)
CatLoop <- tmp_cat

popList <- lapply(VecLoop,function(i){
  tmp <- lapply(VarLoop,function(j){
    tmp <- lapply(CatLoop,function(k){
      message(sprintf("State %s | Var %s | Cat %s",i,j,k))
      pop202 <- sidrar::get_sidra(x = 3939
                                  , variable = c(j)
                                  , period = "2020"
                                  , geo = "City"
                                  , classific = c("c79")
                                  , category = list(k)
                                  , geo.filter = list("State" = i)
      )
      return(data.table::as.data.table(pop202))
    }) %>% data.table::rbindlist()
    return(tmp)
  }) %>% data.table::rbindlist()
  return(tmp)
}) %>% data.table::rbindlist()


names(popList) <- janitor::make_clean_names(names(popList))

readr::write_rds(popList,"data/agro_table3939.rds")

# IRRIGACAO ok-----


info2020 <- sidrar::info_sidra(x = 6858)

info2020
info2020$classific_category %>% names()
info2020$variable$cod %>% as.numeric()


VecLoop <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25
             , 26, 27, 28, 29, 31, 32, 33, 35, 41, 42
             , 43, 50, 51, 52, 53)
VarLoop <- c(2372,2373)
ClasseLoop <- c("c829","c12604","c218","c12567","c12517")

#i = 21;j=2372;k="c218"
popList <- lapply(VecLoop,function(i){
  tmp <- lapply(VarLoop,function(j){
    tmp <- lapply(ClasseLoop,function(k){
      message(sprintf("State %s | Var %s | Classe %s",i,j,k))
      idCat <- which(names(info2020$classific_category) %like% k)
      getCat <- info2020$classific_category[[idCat]]$cod
      getCat <- as.numeric(getCat)
      
      pop202 <- sidrar::get_sidra(x = 6858
                                  , variable = c(j)
                                  , period = "2017"
                                  , geo = "City"
                                  , classific = k
                                  , category = list(getCat)
                                  , geo.filter = list("State" = i)
      )
      return(data.table::as.data.table(pop202))
    }) %>% data.table::rbindlist(use.names=TRUE)
    return(tmp)
  }) %>% data.table::rbindlist(use.names=TRUE)
  return(tmp)
}) %>% data.table::rbindlist(use.names=TRUE)


names(popList) <- janitor::make_clean_names(names(popList))


readr::write_rds(popList,"data/agro_table6858.rds",compress = "gz")
# end-----