firstdata<-list(c("I1","I2","I5"),c("I2","I4"),c("I2","I3"),c("I1","I2","I4"),c("I1","I3"),c("I2","I3"),c("I1","I3"),c("I1","I2","I3","I5"),c("I1","I2","I3"))
datajihe<-factor(unlist(firstdata))
lev<-levels(datajihe)
n<-(length(firstdata))
smlist<-list(xiangji="",zhicidu="")
lenjihe<-length(datajihe)
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
saomiao$zhicidu
houxuanji<-combn(saomiao$xiangji,2)
c2<-function(){
     for(i in 1: ncol(houxuanji))
      {
          count<-0
          x<-houxuanji[,i]
          for(j in 1: n){
            if(sum(x %in% firstdata[[j]])==length(x)){
              count<-count+1
            }
          }
          if(count>=2){
            smlist$xiangji<-append(houxuanji[,i],smlist$xiangji,after = 10)
            smlist$zhicidu<-append(count,smlist$zhicidu,after = 10)
          }
     }
  return(smlist)
}
saomiaoc2<-c2()
saomiaoc2$xiangji<-saomiaoc2$xiangji[-length(saomiaoc2$xiangji)]
saomiaoc2$zhicidu<-saomiaoc2$zhicidu[-length(saomiaoc2$zhicidu)]
l3<-seq(1,length(saomiaoc2$xiangji),by=2)
c3<-function(){
  for(i in l3){
    houxuanc3<-union(saomiaoc2$xiangji[i],saomiaoc2$xiangji[i+1])
    print(houxuanc3)
  }
  
}
c3()





