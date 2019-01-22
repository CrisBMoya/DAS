#Function to generate a matrix of categories of 1 and 0.
TransformData=function(DF, ColumnaCategoria, ColumnaMetabolitos){
  #Calculo Factores Correccion
  DF[,(ColumnaMetabolitos:ncol(DF))]=apply((DF[,(ColumnaMetabolitos:ncol(DF))]), 2, function(x) x*(1/(sd(x)+0)))
  
  #Generar matriz de datos
  counterData=as.matrix(DF[,ColumnaMetabolitos:ncol(DF)])
  rownames(counterData)=DF$Name
  
  #Matriz Categorias
  LogicalMatrix=as.data.frame(matrix(nrow=nrow(DF)))
  for(x in ColumnaCategoria){
    for(i in unique(factor(DF[,x]))){
      LogicalMatrix[,i]=(ifelse(grepl(i , DF[,x]) , 1 , 0))
    }
  }
  LogicalMatrix$V1=NULL
  
  #Generacion de matrix
  for(i in 1:ncol(LogicalMatrix)){
    LogicalMatrix[,i]=LogicalMatrix[,i]*(1/(sd(LogicalMatrix[,i])+0))
  }
  LogicalMatrix[,1]
  LogicalMatrix=as.matrix(LogicalMatrix)
  
  #Returb
  return(list("LogicalMatrix"=LogicalMatrix,
              "counterData"=counterData)
  )
}