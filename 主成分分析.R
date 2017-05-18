
agri=read.table("F:/R/《数据分析：R语言实战》代码及数据/数据包/agriculture.txt",head=TRUE)

pca<-function(mydata){
  le<-length(mydata)
  mydata=mydata[,-1]        #
  bzh<-lapply(mydata,scale)
  juz<-data.frame(bzh)
  xfc<-cov(juz)
  tezz<-eigen(xfc)

  return(tezz)
}

pca(agri)

zcf<-pca(agri)

pca2<-function(zcf){
  su<-sum(zcf[[1]])
  un<-unlist(zcf[[1]])
  le<-length(zcf[[1]])
  i<-1
  cn<-un[1]
  while(cn/su<0.85){
      i<-i+1
      cn<-cn+un[i]
      gxl<-cn/su
    }
  return(un[1]:un[i])
}

pca2(zcf)

