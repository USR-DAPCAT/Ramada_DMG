#  Lectura 


rm(list = ls())
gc()
directori_origen <- "../DADES/epiDMGCAT/SIDIAP_NOV21"  # "../DADES/epiDMGCAT/SIDIAP_NOV21" "../DADES/epiDMGCAT/SIDIAP_NOV21/mostra"
# directori_origen <- "../DADES/epiDMGCAT/SIDIAP_NOV21/mostra"  # "../DADES/epiDMGCAT/SIDIAP_NOV21" "../DADES/epiDMGCAT/SIDIAP_NOV21/mostra"


rmarkdown::render(here::here("codi","1_lectura.Rmd"),
                  params = list(directori_origen= directori_origen),
                  output_file = here::here("outputs",paste0("Informe_exploratori_mostra",Sys.Date())))


# Analisis
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
