

## Identificació de diferents episodis d'embaraç en funció de les probes de cribatge
# 
# Es considera nou episodi d'embaraç quan passen 6 o més mesos (180 dies) entre última prueba de cribaje i la seguent
# Les probes de cribatge considerades són: "SOG50_0","SOG50_60","SOG100_0","SOG100_60","SOG100_120","SOG100_180","SOG75_120"
# Data index es la primera data del Cribatge: Per tant es a les 24-28 setmanes (7e més d'embaraç)

generacio_num_episodi_index<-function() {
  
  library(lubridate)
  # directori_dades_origen<-"../DADES/epiDMGCAT/SIDIAP"
  # Open analitiques 
  dt_analitiques<-
    readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_analitiques_20210222_142117.rds" )) %>% 
    filter(cod%in% c("SOG50_0","SOG50_60","SOG100_0","SOG100_60","SOG100_120","SOG100_180","SOG75_120")) %>% 
    filter(!is.na(val))  # Em carrego missings (NA) en aquestes variables sense valor 
 
  dt_temp<-
    dt_analitiques %>% 
    select(idp,dat) %>% arrange(idp,dat) %>% mutate(num=0) %>% 
    group_by(idp) %>% 
    mutate(dies=lubridate::ymd(dat)-lag(lubridate::ymd(dat))) %>% 
    mutate(dies=as.numeric(dies)) %>% 
    filter(dies>0 | is.na(dies)) %>% # Elimino dates repetides dins d'un individu
    mutate (num=if_else(dies>180,lag(num)+1,lag(num))) %>% ungroup() 
  
  dt_index_long<-
    dt_temp %>% 
    mutate(num=if_else(is.na(num),0,num)) %>% 
    group_by(idp) %>% 
    mutate(numcum=cumsum(num)) %>% 
    ungroup() %>% 
    mutate(numcum=numcum+1) %>% 
    transmute(idp,id_episodi=numcum,dat) 
  }

generacio_num_episodi_index2<-function() {
  
  library(lubridate)
  # directori_dades_origen<-"../DADES/epiDMGCAT/SIDIAP"
  # Open analitiques 
  dt_analitiques<-
    readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_analitiques_20211124_123210.rds" )) %>% 
    filter(cod%in% c("SOG50_0","SOG50_60","SOG100_0","SOG100_60","SOG100_120","SOG100_180","SOG75_120")) %>% 
    filter(!is.na(val))  # Em carrego missings (NA) en aquestes variables sense valor 
  
  dt_temp<-
    dt_analitiques %>% 
    select(idp,dat) %>% arrange(idp,dat) %>% mutate(num=0) %>% 
    group_by(idp) %>% 
    mutate(dies=lubridate::ymd(dat)-lag(lubridate::ymd(dat))) %>% 
    mutate(dies=as.numeric(dies)) %>% 
    filter(dies>0 | is.na(dies)) %>% # Elimino dates repetides dins d'un individu
    mutate (num=if_else(dies>180,lag(num)+1,lag(num))) %>% ungroup() 
  
  dt_index_long<-
    dt_temp %>% 
    mutate(num=if_else(is.na(num),0,num)) %>% 
    group_by(idp) %>% 
    mutate(numcum=cumsum(num)) %>% 
    ungroup() %>% 
    mutate(numcum=numcum+1) %>% 
    transmute(idp,id_episodi=numcum,dat) 
}



generacio_data_index<-function(dt_index_long) {

  # Agrego per id episodi 
  dt_index<-dt_index_long %>% group_by(idp,id_episodi) %>% summarise(dtindex=min(dat)) %>% ungroup()
  
  }

# Classificació de grups en DGM / no DMG

# Segons els seguents condicionals
# - **a) DMG**: 
#   
#   -Definició: SOG50_60min > 140 i >2 valors de la SOG100 (SOG100_0min > 95; SOG100_60min > 180; SOG100_120min > 155; SOG100_180min > 140)
# -Es comenta que en les dones que tinguin SOG50_60min > 140 però que no disposem de SOG100 o aquesta estigui incompleta (per vòmits, intolerancia, etc) es farà una estimació del % de possibles DMG en funció del percentatge de dones amb cribratge positiu (SOG50_60min > 140) i test diagnòstic positiu (SOG100 > 2 valors)
# 
# 
# - **b) NO DMG**:
#   -Definició:	SOG50_60min < 140 o
# SOG50_60min > 140 i <2 valors de la SOG100 (SOG100_0min > 95; SOG100_60min > 180; SOG100_120min > 155; SOG100_180min > 140)
# -Cal tenir present que una mateixa dona pot tenir en una mateixa gestació varies SOG100 amb 1 valor alterat però que no és diagnòstic de DMG. 
# 


classificacio_DMG<-function(dt_index_long,dt_index) {
  
  # directori_dades_origen<-"../DADES/epiDMGCAT/SIDIAP"
  
  # Open analitiques 
  dt_temp<-
    readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_analitiques_20210222_142117.rds" )) %>% 
    filter(cod%in% c("SOG50_0","SOG50_60","SOG100_0","SOG100_60","SOG100_120","SOG100_180","SOG75_120")) %>% 
    select(idp,cod,dat,val) %>% 
    filter(!is.na(val))  # Em carrego missings sense valor
  
  dt_temp2<-
    dt_index_long %>% left_join (dt_temp,by=c("idp","dat")) %>% 
    select(idp,id_episodi,dat,cod,val) %>% 
    mutate(alterat1= if_else (cod=="SOG100_0" & val>95,1,0,missing = 0),
           alterat2= if_else (cod=="SOG100_60" & val>180,1,0,missing = 0),
           alterat3= if_else (cod=="SOG100_120" & val>155,1,0,missing = 0),
           alterat4= if_else (cod=="SOG100_180" & val>140,1,0,missing = 0)) %>% 
    group_by(idp,id_episodi,dat) %>% 
      mutate(Nalterats=sum(alterat1+alterat2+alterat3+alterat4)) %>% 
    ungroup() %>% 
    group_by(idp,id_episodi) %>% 
      mutate(grup=if_else(Nalterats>=2,1,0)) %>% 
    ungroup()
  
  # Agrego per episodi grup 
  dt_index<- dt_index %>% 
    left_join(dt_temp2,by=c("idp","id_episodi")) %>% 
    select(idp,id_episodi,dtindex,grup) %>% 
    group_by(idp,id_episodi,dtindex) %>% 
    summarise(grup=max(grup)) %>% 
    ungroup()
  
  
  }



classificacio_DMG2<-function(dt_index_long,dt_index) {
  
  # directori_dades_origen<-"../DADES/epiDMGCAT/SIDIAP"
  
  # Open analitiques 
  dt_temp<-
    readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_analitiques_20211124_123210.rds" )) %>% 
    filter(cod%in% c("SOG50_0","SOG50_60","SOG100_0","SOG100_60","SOG100_120","SOG100_180","SOG75_120")) %>% 
    select(idp,cod,dat,val) %>% 
    filter(!is.na(val))  # Em carrego missings sense valor
  
  dt_temp2<-
    dt_index_long %>% left_join (dt_temp,by=c("idp","dat")) %>% 
    select(idp,id_episodi,dat,cod,val) %>% 
    mutate(alterat1= if_else (cod=="SOG100_0" & val>95,1,0,missing = 0),
           alterat2= if_else (cod=="SOG100_60" & val>180,1,0,missing = 0),
           alterat3= if_else (cod=="SOG100_120" & val>155,1,0,missing = 0),
           alterat4= if_else (cod=="SOG100_180" & val>140,1,0,missing = 0)) %>% 
    group_by(idp,id_episodi,dat) %>% 
    mutate(Nalterats=sum(alterat1+alterat2+alterat3+alterat4)) %>% 
    ungroup() %>% 
    group_by(idp,id_episodi) %>% 
    mutate(grup=if_else(Nalterats>=2,1,0)) %>% 
    ungroup()
  
  # Agrego per episodi grup 
  dt_index<- dt_index %>% 
    left_join(dt_temp2,by=c("idp","id_episodi")) %>% 
    select(idp,id_episodi,dtindex,grup) %>% 
    group_by(idp,id_episodi,dtindex) %>% 
    summarise(grup=max(grup)) %>% 
    ungroup()
  
  
}




netejar_val_txt<-function(dades) {
  dades %>% mutate (
    
    val_txt=stringi::stri_trans_general(val_txt, id="Latin-ASCII"),
    
    val_txt =         stringr::str_replace_all(val_txt,"\\,","") %>% 
                      stringr::str_replace_all("\\º","") %>% 
                      stringr::str_replace_all("\\.","") %>% 
                      stringr::str_replace_all("\\±","") %>% 
                      stringr::str_replace_all("\\^","") %>% 
                      stringr::str_replace_all("\\[","") %>% 
                      stringr::str_replace_all("\\'","") %>%
                      stringr::str_replace_all("\\`","") %>% 
                      stringr::str_replace_all("\\³","") %>% 
                      stringr::str_replace_all("\\-","") %>% 
                      stringr::str_replace_all("\\>","") %>% 
                      stringr::str_replace_all("\\<","") %>% 
                      stringr::str_replace_all("\\;","") %>%
                      stringr::str_replace_all("\\§","") %>% 
                      stringr::str_replace_all("\\¡","") %>% 
                      stringr::str_replace_all('\\"','') ) %>% 
    mutate(val_txt=toupper(val_txt))}




##############  Llista de funcions agregació #################

# Construcció de variable tabac en funció de variables

# dt_tabac<-generacio_historic_tabac(dt_cataleg_tabac,dt_vars_assir)

generacio_historic_tabac<-function(cataleg=dt_cataleg) {
  
  dt<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_assir_20210222_142117.rds" )) 
  
  dt_cataleg_tabac<-cataleg %>% filter(AGR2=="tabac") %>% select(cod,agr,AGR2)
  
  # PARM007	PARM007==1 --> Tabac= 1 (si)
  dt_tabac<-dt %>% 
    select(-c(agr,cod_embaras)) %>% 
    semi_join(dt_cataleg_tabac,by="cod") %>% 
    mutate(valor=ifelse(val>0,1,0)) %>% 
    mutate(val_txt=stringr::str_trim(val_txt)) %>%  # Trec espais en blanc
    mutate(valor_txt2=
             case_when (val_txt %in% c("NO","no","No","nO","N",""," ")~"No",
                        val_txt == "" ~ "No",
                        is.na(val_txt) ~ "No",
                        TRUE  ~ "Si" )) %>% 
    mutate(valor=if_else(valor_txt2=="Si" | valor==1,1,0,missing = 0)) %>% 
    transmute(idp,cod="tabac",agr="tabac",dat,val=valor)
  
  
  
}

generacio_historic_tabac2<-function(cataleg=dt_cataleg) {
  
  dt<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_assir_20211124_123210.rds" )) 
  
  dt_cataleg_tabac<-cataleg %>% filter(AGR2=="tabac") %>% select(cod,agr,AGR2)
  
  # PARM007	PARM007==1 --> Tabac= 1 (si)
  dt_tabac<-dt %>% 
    select(-c(agr,cod_embaras)) %>% 
    semi_join(dt_cataleg_tabac,by="cod") %>% 
    mutate(valor=ifelse(val>0,1,0)) %>% 
    mutate(val_txt=stringr::str_trim(val_txt)) %>%  # Trec espais en blanc
    mutate(valor_txt2=
             case_when (val_txt %in% c("NO","no","No","nO","N",""," ")~"No",
                        val_txt == "" ~ "No",
                        is.na(val_txt) ~ "No",
                        TRUE  ~ "Si" )) %>% 
    mutate(valor=if_else(valor_txt2=="Si" | valor==1,1,0,missing = 0)) %>% 
    transmute(idp,cod="tabac",agr="tabac",dat,val=valor)
  
  
  
}



agregacio_diagnostics<-function(dt_index=dt_index,dt_cataleg=dt_cataleg) {

  dt_diagnostics_HOSP<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_cmbdh_diagnostics_20210222_142117.rds" )) 
  #v)
  dt_diagnostics_AP<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_diagnostics_20210222_142117.rds" )) 
  
  # Fusiono diagnostics Hospitalaris + Diagnostics AP 
  dt_diagnostics_AP_HOSP<-dt_diagnostics_AP%>%transmute(idp,cod=as.character(cod),dat,agr)%>%
    bind_rows(select(dt_diagnostics_HOSP,idp,cod,dat,agr))
  rm(dt_diagnostics_AP,dt_diagnostics_HOSP)
  
  
  # Cataleg de diagnostics
  dt_cataleg_DG<-dt_cataleg %>% filter(domini %in% c("diagnostics","cmbdh_diagnostics"))
  
  dtagr_diagnostic<-
    dt_diagnostics_AP_HOSP %>% 
    agregar_problemes(bd.dindex = dt_index,dt.agregadors = dt_cataleg_DG,finestra.dies=c(-Inf,+180),cataleg_mana = T)
 
  }

agregacio_analitiques<-function(dt_tabac,dt_index) {
  
  # directori_dades_origen<-"../DADES/epiDMGCAT/mostra"
  # Open analitiques 
  dt_analitiques<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_analitiques_20210222_142117.rds" )) 
  
  dt_cliniques<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_cliniques_20210222_142117.rds" )) 
  
  # vars_pregestacional --> IMC i pes ho faig de 6mesos (182dies) previs a 21 mesos (638dies) previs
  dt_pregestacional<-dt_cliniques

  # Fusiono cliniques + analitques + tabac
  dt_vars<-dt_analitiques %>% 
    bind_rows(dt_cliniques) %>% select(-c(val_txt,rimap)) 
  
  # fusió amb tabac
  dt_vars<-bind_rows(dt_vars,dt_tabac)
  
  # Agregació de base de dades en data index
  dt_temp<-dt_index %>% select(idp,dtindex)
  

  # Analitiques + variables
  dtagr_analitiques<-dt_vars %>% select(idp,cod=agr,dat,val) %>% 
    agregar_analitiques(bd.dindex = dt_temp,finestra.dies = c(-365,+180),fun="last") %>% 
    mutate(dtindex=as_date(dtindex) %>% data.to.string() %>% as.integer())
  
  # Cliniques_pregestacional
  dtagr_pregestacional<-
    dt_pregestacional %>% select(idp,cod=agr,dat,val) %>% 
    agregar_analitiques(bd.dindex = dt_temp,finestra.dies = c(-638,-182),fun="last",sufix = c(".valor_pregest",".dies_pregest")) %>% 
    mutate(dtindex=as_date(dtindex) %>% data.to.string() %>% as.integer())
  
  dtagr_analitiques<-dtagr_analitiques %>% left_join(dtagr_pregestacional,by=c("idp","dtindex"))
  
  }


agregacio_facturacio<-function(dt_index=dt_index,dt_cataleg=dt_cataleg) {

  # directori_dades_origen<-"../DADES/epiDMGCAT/SIDIAP"
  
  dt_farmacs_facturats<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_farmacs_facturats_20210222_142117.rds" )) %>% select(-agr)
  
  # Cataleg 
  # library(dplyr)
  # dt_cataleg<-targets::tar_read(dades_cataleg)
  # dt_index<-targets::tar_read(dt_index)
  
  dt_cataleg_FX<-dt_cataleg %>% filter(domini %in% c("farmacs")) %>% select(cod,agr)
  
  
  dt_index<-dt_index %>% select(idp,dtindex)
  
  # Farmacs facturats
  dtagr_farmacs_facturats<-
    dt_farmacs_facturats %>% 
    agregar_facturacio(finestra.dies=c(-365,+180),dt.agregadors=dt_cataleg_FX,bd.dindex=dt_index,cataleg_mana=T) %>% 
    mutate(dtindex=data.to.string(dtindex) %>% as.integer())

  }
  

agregacio_prescripcio<-function(dt_index=dt_index,dt_cataleg=dt_cataleg) {
  
  dt_farmacs_prescrits<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_farmacs_prescrits_20210222_142117.rds" )) %>% select(-agr)
  
  # Cataleg 
  # dt_cataleg<-targets::tar_read(dades_cataleg)
  dt_cataleg_FX<-dt_cataleg %>% filter(domini %in% c("farmacs")) %>% select(cod,agr) %>% distinct()

  dt_index<-dt_index %>% select(idp,dtindex)
  
  # Farmacs prescrits
  dtagr_farmacs_prescrits<-
    dt_farmacs_prescrits %>% 
    agregar_prescripcions(finestra.dies=c(-180,+180),dt.agregadors=dt_cataleg_FX,bd.dindex=dt_index,cataleg_mana = T) %>% 
    mutate(dtindex=data.to.string(dtindex) %>% as.integer())
  
  }

# Fusiono ambit agafo un sol ambit per idp 

vinculacio_dades_agregades<-function(dt_index,dt_DMGgrup,dtagr_analitiques,dtagr_diagnostic,
                                     dtagr_farmacs_facturats,dtagr_farmacs_prescrits) {

  dt_geosanitaries<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_geosanitaries_20210222_142117.rds" )) 
  
  dt_socioeconomiques<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_variables_socioeconomiques_20210222_142117.rds" )) 
  
  dt_poblacio<-readRDS(here::here(directori_dades_origen,"EPIDMGCAT_entregable_poblacio_20210222_142117.rds"))
  
  # Agafo un unic pacient
  dt_ambit<-dt_geosanitaries %>% select(idp,ambit) %>% group_by(idp,ambit) %>% slice(1L) %>% ungroup()
  
  # Ull!!! s¡'ha d'assegurar que nomes tinguem un pacient per àmbit
  
  # N_pacients_ambit<-dt_ambit %>% count() %>% pull(n)
  # N_poblacio<-dt_poblacio %>% count() %>% pull(n)
  
  # if (N_pacients_ambit!=N_poblacio) cat("Ull !!!! cal revisar el tema dels ambits, on un pacient pot estar en diferents ambits") 
  
  dades<-dt_index %>% 
    left_join(dt_DMGgrup) %>% 
    left_join(dt_poblacio,by="idp") %>% 
    left_join(dt_socioeconomiques,by="idp") %>% 
    left_join(dt_ambit,by="idp") %>% 
    left_join(dtagr_analitiques,by=c("idp","dtindex")) %>% 
    left_join(dtagr_diagnostic,by=c("idp","dtindex")) %>% 
    left_join(dtagr_farmacs_facturats,by=c("idp","dtindex")) %>% 
    left_join(dtagr_farmacs_prescrits,by=c("idp","dtindex"))  
  
  
  
  }




