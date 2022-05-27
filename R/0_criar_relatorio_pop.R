criar_relatorio_pop <- function(grade, sigla_muni, nome_muni) {
  
  # cria caminho pro arquivo com normalizePath porque senão o rmarkdown
  # interpreta errado
  
  
  input_file <- readr::read_rds("data/munis_list.rds")
  sigla_input <- input_file$intermediate_region$code_intermediate[1]
  
  arquivo_resultado <- normalizePath(
    paste0("inst/rmarkdown/", sigla_input, ".docx")
  )
  
  # envia parâmetros pro esqueleto do relatório
  rmarkdown::render(
    input = "inst/test.Rmd"
    ,output_file = "test.docx")
  
  rmarkdown::render(
    input = "inst/rmarkdown/relatorio_pop.Rmd"
    , output_file = arquivo_resultado
    , params = list(
      input_code_intermediate = "2101"
    )
    ,quiet = TRUE
  )
  
  return(invisible(grade))
  
}