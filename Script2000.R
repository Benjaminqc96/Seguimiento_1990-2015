require(foreign)
##########################################read the database##########################################
###viviendas y hogares
bas01<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F01.DBF")
bas02<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F02.DBF")
bas03<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F03.DBF")
bas04<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F04.DBF")
bas05<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F05.DBF")
bas06<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F06.DBF")
bas07<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F07.DBF")
bas08<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//VHO_F08.DBF")
###personas
basp01<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F01.DBF")
basp02<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F02.DBF")
basp03<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F03.DBF")
basp04<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F04.DBF")
basp05<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F05.DBF")
basp06<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F06.DBF")
basp07<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F07.DBF")
basp08<-read.dbf("C://Users//lenovo//Desktop//servicio//censo 2000//PER_F08.DBF")
########################################paste the dbf################################################
basep<-rbind(basp01,basp02,basp03,basp04,basp05,basp06,basp07,basp08)
baseviv<-rbind(bas01,bas02,bas03,bas04,bas05,bas06,bas07,bas08)
###########################extract the variables for the treatment###################################
##variables
#ent,upm,numviv,claviv,tenviv,tenprop
subbasev<-data.frame(baseviv$ENT,baseviv$UPM,baseviv$NUMVIV,baseviv$FACTOR,baseviv$CLAVIV,
                     baseviv$TENVIV,baseviv$TENPROP)
##variables
#ent,upm,numviv,edad,sexo,nivacad,estcon, otropare
subbasep<-data.frame(basep$ENT,basep$UPM,basep$NUMVIV,basep$FACTOR,basep$EDAD,basep$SEXO,
                     basep$NIVACAD,basep$OTROPARE_C,basep$ESTCON)
######################################convert to numeric#############################################
###vivienda
subbasev$baseviv.NUMVIV<-as.numeric(as.character(subbasev$baseviv.NUMVIV))
subbasev$baseviv.UPM<-as.numeric(as.character(subbasev$baseviv.UPM))
subbasev$baseviv.ENT<-as.numeric(as.character(subbasev$baseviv.ENT))
subbasev$FOLIO<-(subbasev$baseviv.ENT*1000000000)+(subbasev$baseviv.UPM*100000)+
  (subbasev$baseviv.NUMVIV)
subbasev$baseviv.CLAVIV<-as.numeric(as.character(subbasev$baseviv.CLAVIV))
subbasev$baseviv.TENVIV<-as.numeric(as.character(subbasev$baseviv.TENVIV))
subbasev$baseviv.TENPROP<-as.numeric(as.character(subbasev$baseviv.TENPROP))
###personas
subbasep$basep.NUMVIV<-as.numeric(as.character(subbasep$basep.NUMVIV))
subbasep$basep.UPM<-as.numeric(as.character(subbasep$basep.UPM))
subbasep$basep.ENT<-as.numeric(as.character(subbasep$basep.ENT))
subbasep$FOLIO<-(subbasep$basep.ENT*1000000000)+(subbasep$basep.UPM*100000)+
  (subbasep$basep.NUMVIV)
subbasep$basep.EDAD<-as.numeric(as.character(subbasep$basep.EDAD))
subbasep$basep.OTROPARE_C<-as.numeric(as.character(subbasep$basep.OTROPARE_C))
###################################begin the treatment###############################################
#home with childs with age between 10 and 14
#discriminant
basemen<-subset(subbasep,(subbasep$basep.EDAD<=14 & subbasep$basep.EDAD>=10))
discmen<-subbasep$FOLIO %in% basemen$FOLIO
discviv<-subbasev$FOLIO %in% basemen$FOLIO
subbasep$DISC<-discmen
subbasev$DISC<-discviv
#subset of the homes boss with childs with age beween 10 & 14
baseperso<-subset(subbasep,(subbasep$basep.OTROPARE_C==100 & subbasep$DISC==TRUE))
basehog<-subset(subbasev, subbasev$DISC==TRUE)
###################################recode the variables##############################################
baseperso$basep.EDAD[baseperso$basep.EDAD>=12 & baseperso$basep.EDAD<=29]<-1
baseperso$basep.EDAD[baseperso$basep.EDAD>=30 & baseperso$basep.EDAD<=59]<-2
baseperso$basep.EDAD[baseperso$basep.EDAD>=60 & baseperso$basep.EDAD<=130]<-3
baseperso$basep.EDAD[baseperso$basep.EDAD==999]<-4
#####################################factor the variables############################################
#personas
baseperso$basep.NIVACAD<-factor(baseperso$basep.NIVACAD,levels = c(1,2,3,4,5,6,7,8,9),
                        labels = c("Preescolar","Primaria","Secundaria","Preparatoria",
                                   "Normal","Carrera tecnica","Profesional","Maestria o doctorado"
                                   ,"No especificado"))
baseperso$basep.EDAD<-factor(baseperso$basep.EDAD, levels = c(1,2,3,4),labels = c("12-29","30-59",
                                                          "60 y mas","No especificado"))
baseperso$basep.SEXO<-factor(baseperso$basep.SEXO,levels = c(1,2),labels = c("Hombre","Mujer"))
baseperso$basep.ESTCON<-factor(baseperso$basep.ESTCON,levels = c(1,2,3,4,5,6,7,8,9),
                               labels = c("Union libre","Separado(a)","Divorciado(a)","Viudo(a)"
                                          ,"Casado(a) por el civil","Casado(a) por la iglesia",
                                          "Casado(a) civil y religiosamente","Soltero",
                                          "No especificado"))
#viviendas
basehog$baseviv.CLAVIV<-factor(basehog$baseviv.CLAVIV,levels = c(1,2,3,4,5,6,7,9), 
                            labels = c("Casa ind.","Departamento en edificio","Vivienda en vecindad"
                              ,"Vivienda en la azotea","Local no construido para habitacion",
                              "Vivienda movil","Refugio","No especificado"))
basehog$baseviv.TENVIV<-factor(basehog$baseviv.TENVIV,levels = c(1,2,9),labels = c("Si","No",
                                                                                   "No espec"))
basehog$baseviv.TENPROP<-factor(basehog$baseviv.TENPROP,levels = c(3,4,5,6,7,9), 
                                labels = c("Pagandose","Totalmente pagada","En otra situacion",
                                           "Rentada","Prestada","No especificado"))
#######################################construct the tables##########################################
#personas
tabedad<-table(baseperso$basep.EDAD,baseperso$basep.SEXO)
tabniv<-table(baseperso$basep.NIVACAD,baseperso$basep.SEXO)
tabesc<-table(baseperso$basep.ESTCON,baseperso$basep.SEXO)
#viviendas
tabcla<-table(basehog$baseviv.CLAVIV)
tabtev<-table(basehog$baseviv.TENVIV)
tabprop<-table(basehog$baseviv.TENPROP)
#########################################write csv format###########################################
#personas
#write.csv(tabedad,"C://Users//lenovo//Desktop//servicio//censo 2000//personas//edad26-32.csv")
#write.csv(tabniv,"C://Users//lenovo//Desktop//servicio//censo 2000//personas//nivacad26-32.csv")
#write.csv(tabesc,"C://Users//lenovo//Desktop//servicio//censo 2000//personas//estcon26-32.csv")
#viviendas
#write.csv(tabcla,"C://Users//lenovo//Desktop//servicio//censo 2000//viviendas//claviv26-32.csv")
#write.csv(tabtev,"C://Users//lenovo//Desktop//servicio//censo 2000//viviendas//tenviv26-32.csv")
write.csv(tabprop,"C://Users//lenovo//Desktop//servicio//censo 2000//viviendas//prop1-8.csv")

