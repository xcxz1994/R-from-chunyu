firstdata<-list(c("I1","I2","I5"),c("I2","I4"),c("I2","I3"),c("I1","I2","I4"),c("I1","I3"),c("I2","I3"),c("I1","I3"),c("I1","I2","I3","I5"),c("I1","I2","I3"))
n<-(length(firstdata))
firstsm<-function(firstdata){
  i<-0
  datajihe<-list()
  for(i in 1:n){
    datajihe<-append(datajihe,firstdata[i],after = n)
    i<-i+1
  }
  return(unlist(datajihe))
}

firstsm(firstdata)

datajihe<-factor(firstsm(firstdata))
typeof(datajihe)
datajihe
lev<-levels(datajihe)

smlist<-list(xiangji="",zhicidu="")

smlist
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


