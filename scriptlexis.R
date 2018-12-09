require(foreign)
####################################read the dbf format##############################################
bas01<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9517.dbf")
bas02<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9518.dbf")
bas03<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9519.dbf")
bas04<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9520.dbf")
bas05<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9521.dbf")
bas06<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9522.dbf")
bas07<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9523.dbf")
bas08<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9524.dbf")
bas09<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9525.dbf")
bas10<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9526.dbf")
bas11<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9527.dbf")
bas12<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9528.dbf")
bas13<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9529.dbf")
bas14<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9530.dbf")
bas15<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9531.dbf")
bas16<-read.dbf("C://Users//lenovo//Desktop//servicio//conteo 1995//datgen9532.dbf")
#############################paste the dbf in one database#########################################
base<-rbind(bas01,bas02,bas03,bas04,bas05,bas06,bas07,bas08,bas09,bas10,bas11,bas12,bas13,bas14,
           bas15,bas16)
################################transform to numerical#############################################
base$P3_6<-as.numeric(as.character(base$P3_6))
#############################check teh age for the treatment#######################################
basemen<-subset(base,(base$P3_6>=5 & base$P3_6<=9))
