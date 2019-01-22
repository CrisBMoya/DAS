#VIP Function
VIPFUN=function(DataFromPLSR){
  
  VIP=as.data.frame(loading.weights(DataFromPLSR)[,1:2, drop=FALSE])
  VIP$Name=paste(1:nrow(VIP),substr(rownames(VIP),1,10), sep="-")
  VIP$Compound=paste(1:nrow(VIP),rownames(VIP), sep="-")
  VIP$distance=sqrt(((VIP$`Comp 1`)^2)+((VIP$`Comp 2`)^2))
  VIP=VIP[order(-VIP$distance),]
  
  return(VIP)
}