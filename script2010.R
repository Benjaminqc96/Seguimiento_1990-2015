require(foreign)
##########################################read the database##########################################
###viviendas y hogares
bas01<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_27.dbf")
bas02<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_28.dbf")
bas03<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_29.dbf")
bas04<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_30.dbf")
bas05<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_31.dbf")
bas06<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_32.dbf")
#bas07<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_27.dbf")
#bas08<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Viviendas_28.dbf")
###personas
basp01<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_27.dbf")
basp02<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_28.dbf")
basp03<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_29.dbf")
basp04<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_30.dbf")
basp05<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_31.dbf")
basp06<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_32.dbf")
#basp07<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_23.dbf")
#basp08<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2010//dbf//Personas_24.dbf")
########################################paste the dbf################################################
ba1<-rbind(basp01,basp02,basp03,basp04,basp05,basp06)
ba2<-rbind(bas01,bas02,bas03,bas04,bas05,bas06)
#####################################extract the variables###########################################
baseperso<-data.frame(ba1$ID_VIV,ba1$SEXO,ba1$EDAD,ba1$PARENT,ba1$NIVACAD,ba1$ESTCON)
basehog<-data.frame(ba2$ID_VIV,ba2$CLAVIVP,ba2$TENVIV)
##convert to numerical
#perso
baseperso$ba1.ID_VIV<-as.numeric(as.character(baseperso$ba1.ID_VIV))
baseperso$ba1.SEXO<-as.numeric(as.character(baseperso$ba1.SEXO))
baseperso$ba1.EDAD<-as.numeric(as.character(baseperso$ba1.EDAD))
baseperso$ba1.PARENT<-as.numeric(as.character(baseperso$ba1.PARENT))
baseperso$ba1.NIVACAD<-as.numeric(as.character(baseperso$ba1.NIVACAD))
baseperso$ba1.ESTCON<-as.numeric(as.character(baseperso$ba1.ESTCON))
#hog
basehog$ba2.ID_VIV<-as.numeric(as.character(basehog$ba2.ID_VIV))
basehog$ba2.CLAVIVP<-as.numeric(as.character(basehog$ba2.CLAVIVP))
basehog$ba2.TENVIV<-as.numeric(as.character(basehog$ba2.TENVIV))
#age between 20 &24
basemen<-subset(baseperso,(baseperso$ba1.EDAD>=20 & baseperso$ba1.EDAD<=24))
baseperso$DISc<-baseperso$ba1.ID_VIV %in% basemen$ba1.ID_VIV
basejf<-subset(baseperso, (baseperso$ba1.PARENT==1 & baseperso$DISc==TRUE))
basehog$DISC<-basehog$ba2.ID_VIV %in% basemen$ba1.ID_VIV
basejfhog<-subset(basehog,basehog$DISC==TRUE)
#############################################recode##################################################
basejf$ba1.EDAD[basejf$ba1.EDAD>=12 & basejf$ba1.EDAD<=29]<-1
basejf$ba1.EDAD[basejf$ba1.EDAD>=30 & basejf$ba1.EDAD<=59]<-2
basejf$ba1.EDAD[basejf$ba1.EDAD>=60 & basejf$ba1.EDAD<=130]<-3
basejf$ba1.EDAD[basejf$ba1.EDAD==999]<-4
#####################################factor the variables############################################
#personas
basejf$ba1.NIVACAD<-factor(basejf$ba1.NIVACAD,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,99),
                                labels = c("Preescolar","Primaria","Secundaria","Preparatoria",
                                           "Normal","Carrera tecnica con primaria",
                                           "Carrera tecnica con secundaria",
                                           "Carrera tecnica con preparatoria"
                                           ,"Normal de licenciatura","Licenciatura","Maestria",
                                           "Doctorado","No especificado"))
basejf$ba1.EDAD<-factor(basejf$ba1.EDAD, levels = c(1,2,3,4),labels = c("12-29","30-59",
                                                                  "60 y mas","No especificado"))
basejf$ba1.SEXO<-factor(basejf$ba1.SEXO,levels = c(1,3),labels = c("Hombre","Mujer"))
basejf$ba1.ESTCON<-factor(basejf$ba1.ESTCON,levels = c(1,2,3,4,5,6,7,8,9),
                               labels = c("Union libre","Separado(a)","Divorciado(a)","Viudo(a)"
                                          ,"Casado(a) por el civil","Casado(a) por la iglesia",
                                          "Casado(a) civil y religiosamente","Soltero",
                                          "No especificado"))
#viviendas
basejfhog$ba2.CLAVIV<-factor(basejfhog$ba2.CLAVIV,levels = c(1,2,3,4,5,6,7,9), 
                               labels = c("Casa ind.","Departamento en edificio","Vivienda en vecindad"
                                          ,"Vivienda en la azotea","Local no construido para habitacion",
                                          "Vivienda movil","Refugio","No especificado"))
basejfhog$ba2.TENVIV<-factor(basejfhog$ba2.TENVIV,levels = c(1,2,3,9),labels = c("Vive el dueño",
                                                                      "Pagan renta","Otra situacion",
                                                                      "No especificado"))
#########################################construct the tables############################################
#perso
tabedad<-table(basejf$ba1.EDAD,basejf$ba1.SEXO)
tabniv<-table(basejf$ba1.NIVACAD,basejf$ba1.SEXO)
tabesc<-table(basejf$ba1.ESTCON,basejf$ba1.SEXO)
#viv
tabcla<-table(basejfhog$ba2.CLAVIVP)
tabten<-table(basejfhog$ba2.TENVIV)
############################################write csv format#############################################
write.csv(tabedad,"C://Users//lenovo//Desktop//servicio//censo 2010//tablas//edad27-32.csv")
write.csv(tabniv,"C://Users//lenovo//Desktop//servicio//censo 2010//tablas//niva27-32.csv")
write.csv(tabesc,"C://Users//lenovo//Desktop//servicio//censo 2010//tablas//cony27-32.csv")
write.csv(tabcla,"C://Users//lenovo//Desktop//servicio//censo 2010//tablas//clasv27-32.csv")
write.csv(tabten,"C://Users//lenovo//Desktop//servicio//censo 2010//tablas//tenen27-32.csv")


