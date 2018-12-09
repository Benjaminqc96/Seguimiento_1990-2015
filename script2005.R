require(foreign)
######################################read the dbf file of the state############################
#person
bas01<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_17_dbf//trpmue17.DBF")
bas02<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_18_dbf//trpmue18.DBF")
bas03<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_19_dbf//trpmue19.DBF")
bas04<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_20_dbf//trpmue20.DBF")
bas05<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_21_dbf//trpmue21.DBF")
bas06<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_22_dbf//trpmue22.DBF")
bas07<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_23_dbf//trpmue23.DBF")
bas08<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_24_dbf//trpmue24.DBF")
#home
basev01<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_17_dbf//trvmue17.DBF")
basev02<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_18_dbf//trvmue18.DBF")
basev03<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_19_dbf//trvmue19.DBF")
basev04<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_20_dbf//trvmue20.DBF")
basev05<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_21_dbf//trvmue21.DBF")
basev06<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_22_dbf//trvmue22.DBF")
basev07<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_23_dbf//trvmue23.DBF")
basev08<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo2005//dbfs//cpv2005_24_dbf//trvmue24.DBF")
#paste te database
basper<-rbind(bas01,bas02,bas03,bas04,bas05,bas06,bas07,bas08)
basviv<-rbind(basev01,basev02,basev03,basev04,basev05,basev06,basev07,basev08)
#####################################generate the folio id_viv######################################
#transform the variables to numerical
#person
basper$ENT<-as.numeric(as.character(basper$ENT))
basper$CONS_MUN<-as.numeric(as.character(basper$CONS_MUN))
basper$CONS_VIV<-as.numeric(as.character(basper$CONS_VIV))
basper$EDAD<-as.numeric(as.character(basper$EDAD))
basper$SEXO<-as.numeric(as.character(basper$SEXO))
basper$NIV_ESCO<-as.numeric(as.character(basper$NIV_ESCO))
basper$PARENT<-as.numeric(as.character(basper$PARENT))
#home
basviv$ENT<-as.numeric(as.character(basviv$ENT))
basviv$CONS_MUN<-as.numeric(as.character(basviv$CONS_MUN))
basviv$CONS_VIV<-as.numeric(as.character(basviv$CONS_VIV))
basviv$CLAVIVPA<-as.numeric(as.character(basviv$CLAVIVPA))
#generate the folio
#ent*10000000000
#mun*10000
#viv*1
basper$FOLIO<-as.numeric((basper$ENT*10000000000)+(basper$CONS_MUN*10000)+basper$CONS_VIV)
basviv$FOLIO<-as.numeric((basviv$ENT*10000000000)+(basviv$CONS_MUN*10000)+basviv$CONS_VIV)
#################################get the subset of the age that we want##############################
basem<-subset(basper,(basper$EDAD>=15 & basper$EDAD<=19))
#generate the discriminitation
disc<-basper$FOLIO %in% basem$FOLIO
basper$DISC<-disc
#################################get the subset of the family boss###################################
basejf<-subset(basper,(basper$DISC==TRUE & basper$PARENT==101))
#get the home
discviv<-basviv$FOLIO %in% basejf$FOLIO
basviv$DISC<-discviv
bacvivjf<-subset(basviv,basviv$DISC==TRUE)
########################################recode the variables#########################################
#edad
basejf$EDAD[basejf$EDAD>=15 & basejf$EDAD<=29]<-1
basejf$EDAD[basejf$EDAD>=30 & basejf$EDAD<=59]<-2
basejf$EDAD[basejf$EDAD>=60 & basejf$EDAD<=130]<-3
basejf$EDAD[basejf$EDAD==999]<-4
########################################factor the variables########################################
basejf$SEXO<-factor(basejf$SEXO,levels = c(1,2),labels = c("Hombre","Mujer"))
basejf$EDAD<-factor(basejf$EDAD,levels = c(1,2,3,4),labels = c("15 a 29","30 a 59",
                                                    "60 y mas","No especificado"))
basejf$NIV_ESCO<-factor(basejf$NIV_ESCO,levels = c(0,1,2,3,4,5,6,7,8,9,99),
                            labels = c("Nnguno","Preescolar","Primaria","Secundaria","Preparatoria",
                            "Normal","Carrera tecnica o comercial","Profesional","Maestria",
                            "Doctorado","No especificado"))
bacvivjf$CLAVIVPA<-factor(bacvivjf$CLAVIVPA,levels = c(1,2,3,4,5,6,7,9),
                  labels = c("Casa independiente","Departamento en edificio","Vivienda en vecindad",
                  "Vivienda en la azotea","Local no construido para habitacion",
                  "Vivienda movil","Refugio","No especificado"))
########################################construct the tables#########################################
#person
tabedad<-table(basejf$EDAD,basejf$SEXO)
tabniv<-table(basejf$NIV_ESCO,basejf$SEXO)
#home
tabtip<-table(bacvivjf$CLAVIVPA)
#######################################write the tables in csv format################################
write.csv(tabedad,"C://Users//lenovo//Desktop//servicio//conteo2005//Tablas//edadjf17-24.csv")
write.csv(tabniv,"C://Users//lenovo//Desktop//servicio//conteo2005//Tablas//escoljf17-24.csv")
write.csv(tabtip,"C://Users//lenovo//Desktop//servicio//conteo2005//Tablas//tiphog17-24.csv")
