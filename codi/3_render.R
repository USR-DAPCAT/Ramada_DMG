
rm(list = ls())
gc()
rmarkdown::render(here::here("codi","2_Analisis_rama.Rmd"),
                  params = list(fitxers_test=T),
                  output_file = here::here("outputs",paste0("Informe_exploratori_mostra",Sys.Date())))

rm(list = ls())
gc()
rmarkdown::render(here::here("codi","2_Analisis_rama.Rmd"),
                  params = list(fitxers_test=F),
                  output_file = here::here("outputs",paste0("Informe_exploratori",Sys.Date())))