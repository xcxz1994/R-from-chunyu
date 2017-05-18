mydata<-read.table("",header=FALSE )

#读取数据
#总体架构
DissMatrix<-function(mydata){
  mydb<-mydata[,-1]
  le<-length(mydb)
  i<-1
  su<-mydb[,1]
  while(i<le){
    i<-i+1
    su<-mydb[,i]
    su.type<-typeof(su)
    if(su.type==integer)
      MaInt(su)
    if(su.type==character)
      Machar(su)
    else
      return(NULL)
  }
}

#因为目前无法区分标称和序数属性，所以按照特定的数据处理区分标称和序数
Machar<-function(su){
  if("A"%in% su)
    biaochen(su)
  if("优秀"%in% su)
    xushu(su)
  else
    return("不是标准数据类型")
}

#下面分步骤进行
su<-c("A","B","C","A")
length(su)
biaochen<-function(su){
  bcd<-matrix(0,length(su),length(su),byrow = T)
  print(bcd)
  n<-length(su)
  for(i in 1:n){
    for(j in 1:n){
      if(su[i]!=su[j])
        bcd[i,j]=1
    }
  }
  return(bcd)
}
biaochen(su)
su2<-factor(c("优秀","一般","好","优秀"))


xushu<-function(su2){
  bcd<-matrix(0,length(su2),length(su2),byrow = T)
  print(bcd)
  n<-levels(su2)
  vec<-c()
  for(i in 1:length(n)){
    vec[n[i]]<-i
    
  }
  vec<-(vec-1)/(max(vec)-1)
  
  for(i in 1:length(su2)){
    for(j in 1:length(su2)){
     bcd[i,j]<-abs(vec[su2[j]]-vec[su2[i]])
    }
  }
  return(bcd)
}
xushu(su2)

su3<-c(45,22,64,28)
MaInt<-function(su3){
  bcd<-matrix(0,length(su3),length(su3),byrow = T)
  print(bcd)
  n<-length(su)
  for(i in 1:n){
    for(j in 1:n){
      bcd[i,j]<-abs(su3[j]-su3[i])/(max(su3)-min(su3))
    }
  }
  return(bcd)
}
MaInt(su3)

