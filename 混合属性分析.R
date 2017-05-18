dissimilarity<-function(data){
  datas<-data
  rownum<-nrow(datas)
  colnum<-ncol(datas)
  nominal<-(colnames(datas)=="nominal")
  binarys<-(colnames(datas)=="binarys")
  ordinal<-(colnames(datas)=="ordinal")
  numerics<-(colnames(datas)=="numerics")
  #序数属性
  for(i in 1:colnum){
    if(ordinal[i]){
      a<-datas[,i]
      a<-as.numeric(a)
      #计算排位
      for(j in 1:length(a)){
        a[j]<-(a[j]-1)/(max(a)-1)
      }
      datas[,i]<-a
    }}
  
  num2<-matrix(0,nrow=rownum,ncol=rownum)
  for(i in 1:colnum)
    if(ordinal[i])
      for(k in 1:rownum)
        for(j in 1:k)
          num2[k,j]<-(abs(datas[k,i]-datas[j,i]))/( max(datas[,i])-min(datas[,i]) )
  #数值属性
  num1<-matrix(0,nrow=rownum,ncol=rownum)
  for(i in 1:colnum)
    if(numerics[i])
      for(k in 1:rownum)
        for(j in 1:k)
          num1[k,j]<-(abs(datas[k,i]-datas[j,i]))/( max(datas[,i])-min(datas[,i]) )
  #指示符
  c<-matrix(0,nrow=rownum,ncol=rownum)
  #相异性矩阵
  total<-0
  m<-matrix(0,nrow=rownum,ncol=rownum)
  for(i in 1:rownum){
    for(j in 1:i){
      for(k in 1:colnum){
        
        if(k %in% which(nominal==T) || k %in% which(binarys==T)){
          if(is.null(datas[i,k]) || is.null(datas[j,k]))
            c[i,j]<-c[i,j]+0 
          else if(datas[i,k]!=datas[j,k]){
            c[i,j]<-c[i,j]+1
            total<-total+1
          }
          else if(datas[i,k]==datas[j,k] && k %in% which(binarys==T))
            total<-total+1
          else total<-total+1
        }
        if( k %in% which(ordinal==T)){
          c[i,j]<-c[i,j]+num2[i,j]
          total<-total+1
        }
        if( k %in% which(numerics==T)){
          c[i,j]<-c[i,j]+num1[i,j]
          total<-total+1
        }
      }
      
      m[i,j]<-c[i,j]/total
      if(i==4 && j==1)
        tt<-total
      total<-0
    }
  }
  lib<-list(a=num1,b=num2,c=m)
  return(lib)
}
a<-data.frame(nominal=c("A","B","C","A"),ordinal=c(3,1,2,3),numerics=c(45,22,64,28),binarys=c(0,1,0,1))
m<-dissimilarity(a)
m
