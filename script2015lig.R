#######################################read the database###########################################
#persona
basp01<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_PERSONA10.CSV")
basp02<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_PERSONA11.CSV")
basp03<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_PERSONA13.CSV")
basp04<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_PERSONA17.CSV")
#vivienda
bas01<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_VIVIENDA10.CSV")
bas02<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_VIVIENDA11.CSV")
bas03<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_VIVIENDA13.CSV")
bas04<-read.csv("C://Users//lenovo//Desktop//servicio//eic2015//TR_VIVIENDA17.CSV")
#####################################paste the database###########################################
basep<-rbind(basp01,basp02,basp03,basp04)
basev<-rbind(bas01,bas02,bas03,bas04)
###########################extract the variables for the treatment################################
personas<-data.frame(basep$ID_VIV,basep$SEXO,basep$EDAD,basep$PARENT,basep$NIVACAD,basep$SITUA_CONYUGAL)
viviendas<-data.frame(basev$ID_VIV,basev$CLAVIVP,basev$TENENCIA)
######################subset of persons with age between 25 & 29 years############################
basemen<-subset(personas,(personas$basep.EDAD>=25 & personas$basep.EDAD<=29))
personas$DISC<-personas$basep.ID_VIV %in% basemen$basep.ID_VIV
viviendas$DISC<-viviendas$basev.ID_VIV %in% basemen$basep.ID_VIV
################################subset of the home boss###########################################
jfper<-subset(personas,(personas$DISC==TRUE & personas$basep.PARENT==1))
jfviv<-subset(viviendas,viviendas$DISC==TRUE)
###################################recode variables###############################################
jfper$basep.EDAD[jfper$basep.EDAD>=12 & jfper$basep.EDAD<=29]<-1
jfper$basep.EDAD[jfper$basep.EDAD>=30 & jfper$basep.EDAD<=59]<-2
jfper$basep.EDAD[jfper$basep.EDAD>=60 & jfper$basep.EDAD<=110]<-3
jfper$basep.EDAD[jfper$basep.EDAD==999]<-4
###################################factor the variables###########################################
#person
jfper$basep.SEXO<-factor(jfper$basep.SEXO,levels = c(1,3),labels = c("Hombre","Mujer"))
jfper$basep.EDAD<-factor(jfper$basep.EDAD,levels = c(1,2,3,4),labels = c("12-29","30-59",
                                                          "60 y mas","No especificado"))
jfper$basep.NIVACAD<-factor(jfper$basep.NIVACAD,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,99),
                            labels = c("Ninguno","Preescolar","Primaria","Secundaria","Preparatoria",
                                       "bachillerato tecnologico","Carrera tecnica con primaria",
                                       "Carrera tecnica con secundaria",
                                       "Carrera tecnica con preparatoria",
                                       "Normal con primaria o secundaria"
                                       ,"Normal de licenciatura","Licenciatura","Especialidad",
                                       "Maestria","Doctorado","No especificado"))
jfper$basep.SITUA_CONYUGAL<-factor(jfper$basep.SITUA_CONYUGAL,levels = c(1,2,3,4,5,6,9),
                                   labels = c("Union libre","Separado(a)","Divorciado(a)","Viudo(a)"
                                              ,"Casado(a) ","Soltero(a)",
                                              "No especificado"))
#home
jfviv$basev.CLAVIVP<-factor(jfviv$basev.CLAVIVP,levels = c(1,2,3,4,5,6,7,8,9,99), 
                            labels = c("Casa ind.","Casa que comparte terreno","Casa duplex o triple",
                                       "Departamento en edificio",
                                       "Vivienda en vecindad",
                                       "Vivienda en la azotea","Local no construido para habitacion",
                                       "Vivienda movil","Refugio","No especificado"))
jfviv$basev.TENENCIA<-factor(jfviv$basev.TENENCIA,levels = c(1,2,3,4,9),labels = c("Vive el dueño",
                      "Pagan renta","Es e un familiar","Otra situacion","No especificado"))
###########################################construct the tables########################################
#person
tabedad<-table(jfper$basep.EDAD,jfper$basep.SEXO)
tabniv<-table(jfper$basep.NIVACAD,jfper$basep.SEXO)
tabcon<-table(jfper$basep.SITUA_CONYUGAL,jfper$basep.SEXO)
#home
tabcla<-table(jfviv$basev.CLAVIVP)
tabten<-table(jfviv$basev.TENENCIA)
#########################################write csv format############################################
write.csv(tabedad,"C://Users//lenovo//Desktop//servicio//eic2015//tablas//edad28-32.csv")
write.csv(tabniv,"C://Users//lenovo//Desktop//servicio//eic2015//tablas//niv28-32.csv")
write.csv(tabcon,"C://Users//lenovo//Desktop//servicio//eic2015//tablas//cony28-32.csv")
write.csv(tabcla,"C://Users//lenovo//Desktop//servicio//eic2015//tablas//clav28-32.csv")
write.csv(tabten,"C://Users//lenovo//Desktop//servicio//eic2015//tablas//tene28-32.csv")