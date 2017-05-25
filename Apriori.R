firstdata<-c(c("I1","I2","I5"),c("I2","I4"),c("I2","I3"),c("I1","I2","I4"),c("I1","I3"),c("I2","I3"),c("I1","I3"),c("I1","I2","I3","I5"),c("I1","I2","I3"))
datajihe<-factor(firstdata)
datajihe
lev<-levels(datajihe)
lev
n<-(length(firstdata))
smlist<-list(xiangji="",zhicidu="")
lenjihe<-length(datajihe)
lenjihe
zcdcount<-function(datajihe){
  lenlev<-length(lev)
  for(i in 1:lenjihe){
    for(j in 1:lenlev){
      if(lev[j]==datajihe[[i]]){
        if(!(datajihe[[i]] %in% smlist$xiangji)){
          smlist$xiangji<-append(lev[j],smlist$xiangji,after=lenjihe)
          smlist$zhicidu<-append(sum(lev[j]==datajihe[]),smlist$zhicidu,after = lenjihe)
        }
      }
    }
  }
  return(smlist)
}
saomiao<-zcdcount(datajihe)
saomiao$xiangji<-saomiao$xiangji[-length(saomiao$xiangji)]
saomiao$zhicidu<-saomiao$zhicidu[-length(saomiao$zhicidu)]
saomiao
houxuanji<-combn(lev,2)




