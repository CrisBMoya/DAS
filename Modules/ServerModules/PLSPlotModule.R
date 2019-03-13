PLSPlotModule=function(input, output, session, 
                       plsrData, LogicalMatrix, themeCustom, 
                       DotSize, TextSize, Shape, ShapeLogic){
  
  #Correlation Loadings for X & Y
  cl=cor(model.matrix(plsrData),(scores(plsrData)[,1:2,drop=FALSE]))
  clY=cor(LogicalMatrix, scores(plsrData)[,1:2, drop=FALSE])
  
  #Inner and Outer Circle
  radii=c(sqrt(1/2),1)
  
  #Creating Dataframes
  clplot=as.data.frame(cl)
  clplotY=as.data.frame(clY)
  
  #Computing Circles for ggplot
  #Circle 1
  theta <- seq(0, 2 * pi, length = 500)
  
  xtemp1=radii[1]*cos(theta)
  ytemp1=radii[1]*sin(theta)
  circ1=as.data.frame(cbind(xtemp1,ytemp1))
  colnames(circ1)=c("cx1","cy1")
  
  #Circle 2
  xtemp2=radii[2]*cos(theta)
  ytemp2=radii[2]*sin(theta)
  circ2=as.data.frame(cbind(xtemp2,ytemp2))
  colnames(circ2)=c("cx2","cy2")
  
  #Raw Plot
  p1=ggplot(data=clplot, aes(x=`Comp 1`, y=`Comp 2`)) + {
    if(ShapeLogic){
      geom_point(size=DotSize, shape={
        tmp=Shape[match(x=rownames(clplot), table=Shape$ID),]$shape
        tmp[is.na(tmp)]=0
        tmp
      }
      )
  }else{
    geom_point(size=DotSize)
  }
} +
  scale_x_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1)) +
  scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1))
#Add Cross
p1=p1 + geom_hline(yintercept = 0, alpha=0.2) + geom_vline(xintercept = 0, alpha=0.2)
#Circle
p1=p1 + geom_polygon(data=circ1 ,aes(x=cx1, y=cy1),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
  geom_polygon(data=circ2, aes(x=cx2, y=cy2),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5)
#Condition Points
p1=p1 + geom_point(data=clplotY, aes(x=`Comp 1`, y=`Comp 2`, color="red")) +
  geom_text(data=clplotY,nudge_x=0.02, nudge_y=0.02, fontface ="bold",aes(label=rownames(clplotY)))

#Labels
p1=p1 + labs(y =paste("Factor-2 (",substr(explvar(plsrData)[2],1,5),"%)", sep=""),
             x =paste("Factor-1 (",substr(explvar(plsrData)[1],1,5),"%)", sep=""))
#Other
p1= p1 + theme(legend.position="none")
#Chemicals Labels
p1=p1 + geom_text(data=clplot, nudge_y=0.05, size=TextSize, aes(label=paste(1:nrow(clplot),substr(rownames(clplot),1,10), sep="-"))) +
  themeCustom

#Raw Plot
p1v2=ggplot(data=clplot, aes(x=`Comp 1`, y=`Comp 2`)) +
  scale_x_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1)) +
  scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,by=0.1))
#Add Cross
p1v2=p1v2 + geom_hline(yintercept = 0, alpha=0.2) + geom_vline(xintercept = 0, alpha=0.2)
#Circle
p1v2=p1v2 + geom_polygon(data=circ1 ,aes(x=cx1, y=cy1),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
  geom_polygon(data=circ2, aes(x=cx2, y=cy2),size=0.5,inherit.aes = F, colour="black", fill=NA, alpha=0.5)
#Condition Points
p1v2=p1v2 + geom_point(data=clplotY, aes(x=`Comp 1`, y=`Comp 2`, color="red")) +
  geom_text(data=clplotY,nudge_x=0.02, nudge_y=0.02, fontface ="bold",aes(label=rownames(clplotY)))
#Labels
p1v2=p1v2 + labs(y =paste("Factor-2 (",substr(explvar(plsrData)[2],1,5),"%)", sep=""),
                 x =paste("Factor-1 (",substr(explvar(plsrData)[1],1,5),"%)", sep=""))
#Other
p1v2= p1v2 + theme(legend.position="none")
p1v2=p1v2 + geom_text(data=clplot, size=TextSize, aes(label=seq(1,nrow(clplot),by=1))) +
  themeCustom


#Plotly
p1Plotly=ggplotly(p1)
p1v2Plotly=ggplotly(p1v2)

list("p1Plotly"=p1Plotly, "p1v2Plotly"=p1v2Plotly,
     "p1"=p1, "p1v2"=p1v2,
     "clplot"=clplot, "clplotY"=clplotY, "circ1"=circ1, "circ2"=circ2)
}