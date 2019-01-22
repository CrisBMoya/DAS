LimitFUN=function(EllipseTestResult){ 
  
  xlimits=abs(floor(min(EllipseTestResult$V1)))>abs(ceiling(max(EllipseTestResult$V1)))
  xlimitsRes=ifelse(xlimits==TRUE,abs(floor(min(EllipseTestResult$V1))),abs(ceiling(max(EllipseTestResult$V1))))
  ylimits=abs(floor(min(EllipseTestResult$V2)))>abs(ceiling(max(EllipseTestResult$V2)))
  ylimitsRes=ifelse(ylimits==TRUE,abs(floor(min(EllipseTestResult$V2))),abs(ceiling(max(EllipseTestResult$V2))))
  
  return(list(
    "xlimitsRes"=xlimitsRes,
    "ylimitsRes"=ylimitsRes
  ))
  
}