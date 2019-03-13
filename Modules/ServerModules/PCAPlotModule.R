PCAPlotModule=function(input, output, session, pcaData, Input, labelPercent,
                       ellTest, clPCA, circ1, circ2, plsrData, themeCustom, DotSize, TextSize){
  
  #Score Plot SIN Hotellings T2-Test
  PCA_NoLimits=ggplot(data=pcaData,aes(x=as.numeric(pcaData$X1), y=as.numeric(pcaData$X2))) + 
    geom_point(size=DotSize) + 
    geom_text(label=Input[,1], nudge_y=0.5, size=TextSize) +
    labs(x=paste("PC-1 (",labelPercent[1],")", sep=""), y=paste("PC-2 (",labelPercent[2],")", sep=""))
  #Add a Cross
  PCA_NoLimits=PCA_NoLimits + geom_hline(yintercept = 0, alpha=0.2) + geom_vline(xintercept = 0, alpha=0.2)
  
  PCAP1Plotly=ggplotly(PCA_NoLimits)
  
  
  #Add Limits
  xlimits=abs(floor(min(pcaData$X1)))>abs(ceiling(max(pcaData$X1)))
  xlimitsRes=ifelse(xlimits==TRUE,abs(floor(min(pcaData$X1))),abs(ceiling(max(pcaData$X1))))
  ylimits=abs(floor(min(pcaData$X2)))>abs(ceiling(max(pcaData$X2)))
  ylimitsRes=ifelse(ylimits==TRUE,abs(floor(min(pcaData$X2))),abs(ceiling(max(pcaData$X2))))
  Scores_PCA_NoHotellings= PCA_NoLimits + scale_x_continuous(limits=c(-1*xlimitsRes,xlimitsRes), breaks=seq(-1*xlimitsRes,xlimitsRes, by=1)) +
    scale_y_continuous(limits=c(-1*ylimitsRes,ylimitsRes), breaks=seq(-1*ylimitsRes,ylimitsRes, by=1)) +
    themeCustom
  
  PCAP2Plotly=ggplotly(Scores_PCA_NoHotellings)
  
  
  
  #Grafico de Score CON Hotelling T2-Test
  Scores_PCA_Hotellings_NoLimits=ggplot(data=pcaData, aes(x=X1, y=X2)) + geom_point(size=DotSize) + 
    geom_polygon(data=ellTest,aes(x=V1, y=V2),inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    labs(x=paste("PC-1 (",labelPercent[1],")", sep=""), y=paste("PC-2 (",labelPercent[2],")", sep="")) +
    geom_text(label=Input[,1], nudge_y=0.5, size=TextSize) +
    themeCustom
  
  #With Limits
  Scores_PCA_Hotellings= Scores_PCA_Hotellings_NoLimits + scale_x_continuous(limits=c(-1*xlimitsRes,xlimitsRes), breaks=seq(-1*xlimitsRes,xlimitsRes, by=1)) +
    scale_y_continuous(limits=c(-1*ylimitsRes,ylimitsRes), breaks=seq(-1*ylimitsRes,ylimitsRes, by=1)) +
    themeCustom
  
  ###
  #Raw Plot
  CorrelationLoading_PCA=ggplot(data=clPCA, aes(x=X1, y=X2)) + geom_point(size=DotSize) +
    scale_x_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1)) + 
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1))
  #Add Cross
  CorrelationLoading_PCA=CorrelationLoading_PCA + geom_hline(yintercept = 0, alpha=0.2) + geom_vline(xintercept = 0, alpha=0.2)
  #Circle
  CorrelationLoading_PCA=CorrelationLoading_PCA + geom_polygon(data=circ1 ,aes(x=cx1, y=cy1),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
    geom_polygon(data=circ2, aes(x=cx2, y=cy2),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5)
  #Labels
  CorrelationLoading_PCA=CorrelationLoading_PCA + labs(y =paste("Factor-2 (",substr(explvar(plsrData)[2],1,5),"%)", sep=""),
                                                       x =paste("Factor-1 (",substr(explvar(plsrData)[1],1,5),"%)", sep=""))
  #Other
  CorrelationLoading_PCA= CorrelationLoading_PCA + theme(legend.position="none")
  #Chemicals Labels
  CorrelationLoading_PCA=CorrelationLoading_PCA + geom_text(data=clPCA, nudge_y=0.05, size=TextSize, aes(label=paste(1:nrow(clPCA),substr(rownames(clPCA),1,10), sep="-"))) +
    themeCustom
  
  #####
  #Raw Plot
  CorrelationLoading_PCA_NoLabel=ggplot(data=clPCA, aes(x=X1, y=X2)) + 
    scale_x_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1)) + 
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1))
  #Add Cross
  CorrelationLoading_PCA_NoLabel=CorrelationLoading_PCA_NoLabel + geom_hline(yintercept = 0, alpha=0.2) + geom_vline(xintercept = 0, alpha=0.2)
  #Circle
  CorrelationLoading_PCA_NoLabel=CorrelationLoading_PCA_NoLabel + geom_polygon(data=circ1 ,aes(x=cx1, y=cy1),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
    geom_polygon(data=circ2, aes(x=cx2, y=cy2),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5)
  #Labels
  CorrelationLoading_PCA_NoLabel=CorrelationLoading_PCA_NoLabel + labs(y =paste("Factor-2 (",substr(explvar(plsrData)[2],1,5),"%)", sep=""),
                                                                       x =paste("Factor-1 (",substr(explvar(plsrData)[1],1,5),"%)", sep=""))
  #Other
  CorrelationLoading_PCA_NoLabel= CorrelationLoading_PCA_NoLabel + theme(legend.position="none")
  CorrelationLoading_PCA_NoLabel=CorrelationLoading_PCA_NoLabel + geom_text(data=clPCA, size=TextSize, aes(label=seq(1,nrow(clPCA),by=1))) +
    themeCustom
  
  #Take values out of reactive
  list("PCA_NoLimits"=PCA_NoLimits, 
       "Scores_PCA_NoHotellings"=Scores_PCA_NoHotellings,
       "Scores_PCA_Hotellings_NoLimits"=Scores_PCA_Hotellings_NoLimits,
       "Scores_PCA_Hotellings"=Scores_PCA_Hotellings,
       "CorrelationLoading_PCA"=CorrelationLoading_PCA,
       "CorrelationLoading_PCA_NoLabel"=CorrelationLoading_PCA_NoLabel,
       "PCAP1Plotly"=PCAP1Plotly,
       "PCAP2Plotly"=PCAP2Plotly
  )
  
}