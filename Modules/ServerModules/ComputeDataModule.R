ComputeDataModule=function(input, output, session, DFData, ColCatMin, ColCatMax, ColMeta){
  
#Preare data
#ComputeData=eventReactive(input$DummyAlert, {

#Take input
#DFData=as.data.frame(DFReact$DF)

#Compute matrix of categories in form of 1 and 0 - This are the Response or Dependent Variable
#Return also the counterData DF, which is the original data -This are the Predictors or Independt Variables
DataFrameTransformed=TransformData(DF=DFData, 
                                   ColumnaCategoria=ColCatMin:ColCatMax, 
                                   ColumnaMetabolitos=ColMeta)

#From the previous function, retrieve the Response
LogicalMatrix=DataFrameTransformed$LogicalMatrix
#From the previous function, retrieve the Predictors
counterData=DataFrameTransformed$counterData

#    ____  _     ____
#   |  _ \| |   / ___|
#   | |_) | |   \___ \
#   |  __/| |___ ___) |
#   |_|   |_____|____/
#

#Compute Partial Least Squeares PLS
plsrData=plsr(LogicalMatrix ~ counterData,
              ncomp = 7, validation = "CV",
              segments=nrow(DFData), method = "oscorespls")
##Vip 
VIP=VIPFUN(DataFromPLSR=plsrData)


#    _____   _____
#   |  __ \ / ____|   /\
#   | |__) | |       /  \
#   |  ___/| |      / /\ \
#   | |    | |____ / ____ \
#   |_|     \_____/_/    \_\
#
#

#Transform Predictors into Data Frame
counterData=as.data.frame(counterData)

##PLOT PCA CON %. "Scores" Plot en Unscrambler.
#PCA. Eigenvalues and % of PC's equal to Unscrambler
pc1=suppressWarnings(pcaFit(counterData, scale=TRUE, ncomp=7))
labelPercent=paste(substr((pc1$Percents.Explained[1:2,1]),1,4),"%", sep="")

#Datos de Score y Loadings. Muy similar a Unscrambler
pc1Nipals=pca.nipals(counterData, ncomps=7, Iters=100)
dummy=data.frame(pc1Nipals$Scores)
pcaData=dummy[1:2]

#Score plot with Hotelling T2-Test
#Generar datos de circulos para Hotelling T2-Test
ellTest=as.data.frame(simpleEllipse(pcaData[,1],pcaData[,2]))

#Computing Limits
LimitResult=LimitFUN(EllipseTestResult=ellTest)
xlimitsRes=LimitResult$xlimitsRes
ylimitsRes=LimitResult$ylimitsRes

#clPCA Data
clPCA=as.data.frame(cor(model.matrix(plsrData),pcaData))

#Take values out of event reactive
list(
  #General Data
  "LogicalMatrix"=LogicalMatrix, "counterData"=counterData, "DFData"=DFData,
  #PLS
  "plsrData"=plsrData,
  #PCA
  "pcaData"=pcaData, "labelPercent"=labelPercent, "ellTest"=ellTest,
  "xlimitsRes"=xlimitsRes, "ylimitsRes"=ylimitsRes,
  "clPCA"=clPCA,
  #VIP
  "VIP"=VIP,
  #Button Trigger
  "Status"="DONE!")

#})
}