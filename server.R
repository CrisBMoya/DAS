#Loading Libraries
{
  library("sp")
  library("openxlsx")
  library("pls")
  library("ggplot2")
  library("plotly")
  library("mvdalab")
  library("cowplot")
  library("tidyselect")
  library("shiny")
  library("DT")
  library("shinydashboard")
  library("shinycssloaders")
  library("shinyalert")
  library("shinyjs")
  library("V8")
}

#Seed
set.seed(101)

#Create Simple Ellipse (Hotellings)
source(file="~/RFiles/Shiny/Unscramble-Shiny/Scripts/simpleEllipse[FUNCTION].R")
source(file="~/RFiles/Shiny/Unscramble-Shiny/Scripts/LogicalMatrix[FUNCTION].R")
source(file="~/RFiles/Shiny/Unscramble-Shiny/Scripts/VIP[FUNCTION].R")
source(file="~/RFiles/Shiny/Unscramble-Shiny/Scripts/LimitFUN[FUNCTION].R")

#Blank Theme for ggplot
theme_blank <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank())



#     _____                          
#    / ____|                         
#   | (___   ___ _ ____   _____ _ __ 
#    \___ \ / _ \ '__\ \ / / _ \ '__|
#    ____) |  __/ |   \ V /  __/ |   
#   |_____/ \___|_|    \_/ \___|_|   
#                                    
#                                    


#Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #Disable some optons by default
  shinyjs::disable(id="Cat1")
  shinyjs::disable(id="Cat2")
  shinyjs::disable(id="Met1")
  shinyjs::disable(id="RemoveRow")
  
  #Load user input data
  ColCatMin=reactive({input$Cat1})
  ColCatMax=reactive({input$Cat2})
  ColMeta=reactive({input$Met1})
  RowRemove=reactive({input$RemoveRow})
  
  #Create reactive value to store DF
  DFReact=reactiveValues(DF=NULL)
  
  #Load actual DF
  observeEvent(input$file1, {
    #Read file
    DFFile=input$file1
    
    #Load sheet Names
    WB=openxlsx::getSheetNames(file=DFFile$datapath)
    
    #Output Sheet Names in a dropdown menu
    output$SheetName = renderUI({
      selectInput(choices=WB, selected=WB[1], inputId="SheetNameInput", label="Nombre de Hoja")
    })
    
    #Output Spacer
    output$SPACER=renderUI({
      tags$p(tags$b("Cargar Datos:"))
    })
    
    #Output Button to load
    output$LoadFileUI=renderUI({
      actionButton(inputId="LoadFile", label="Cargar Tabla", icon=icon("upload"), 
                   style="color: #fff;
                          background-color: #47a447; 
                          border-color: #398439;
                          display: inline-block;")
    })
    
  })
  
  #Load Excel based on selected Sheet Name
  observeEvent(input$LoadFile, {
    #Require precense of DF before computing data
    req(input$file1)
    
    #Load user input table
    DFFile=input$file1
    DFReact$DF=openxlsx::read.xlsx(xlsxFile=DFFile$datapath, sheet=input$SheetNameInput)
    
    
    #Output Button to load
    output$CalcularUI=renderUI({
      actionButton(inputId="Compute", label="Calcular", icon=icon("angle-right"),
                   style="color: #fff;
                   border-radius: 12px;
                   background-color: #428bca;
                   border-color: #357ebd;
                   font-size: 20px;
                   padding: 10px 40px;
                   display: inline-block;
                   margin-left: 20%;")
    })
    
    #Reactivate User Input
    shinyjs::enable(id="Cat1")
    shinyjs::enable(id="Cat2")
    shinyjs::enable(id="Met1")
    shinyjs::enable(id="RemoveRow")
    
  })
  
  #Print Loaded Table
  output$contents = DT::renderDataTable({
    DT::datatable(data=DFReact$DF, options=list(lengthMenu=c(5,10)))
  })
  
  #Delete Row of current loaded DF
  observeEvent(input$DeleteRowBtn, {
    #Require DF existente
    req(input$LoadFile)
    
    #Validate rownames to delete.
    #If the user input dont exist in the row names then throws an error
    if(RowRemove() %in% DFReact$DF[,1]){
      DFReact$DF=DFReact$DF[-which(DFReact$DF[,1]==paste0(RowRemove())),]
    }else{
      shinyalert(
        title = "ERROR",
        closeOnEsc = FALSE, #Should be false
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE, #Should be false
        confirmButtonText = "Cerrar",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        text="El valor ingresado no existe!"
      )
    }
    
  })
  
  #Reset Page Buttons
  observeEvent(input$reset_button, {js$reset()}) 
  
  #Setup Shiny Alert
  #Compute Data
  observeEvent(input$Compute, {
    #Require precense of DF before computing data
    req(input$file1)
    
    #Validate numeric part of DataFrame
    NumericTemp=sapply(X=DFReact$DF[,ColMeta():ncol(DFReact$DF)], FUN=class)
    #Which values are either of class numeric or integer?
    #If the result of the negation of any is TRUE, then it means that there are values that 
    #arent either numeric nor integer, ergo, the DF is wrong.
    NumericValidation=any(!(NumericTemp %in% c("numeric","integer")))
    if(NumericValidation){
      shinyalert(
        title = "ERROR",
        closeOnEsc = FALSE, #Should be false
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = FALSE,
        showCancelButton = FALSE, #Should be false
        confirmButtonText = "Cerrar",
        confirmButtonCol = "#AEDEF4",
        timer = 2900,
        imageUrl = "",
        animation = TRUE,
        text="Valores no-numericos entre los metabolitos!"
      )
      #Wait
      Sys.sleep(3)
      #Resetear pagina
      js$reset() 
      
    } else {
      
      #Wait
      Sys.sleep(time=0.5)
      #Alert of Computing, purely aesthetic
      shinyalert(
        inputId="DummyAlert",
        title = "Calculando...",
        closeOnEsc = FALSE, #Should be false
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE, #Should be false
        confirmButtonText = "Siguiente",
        confirmButtonCol = "#428bca",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        text="Presione el boton para continuar"
      )
    }
    
    
  })
  
  #Shiny Alert to start computation
  observeEvent(input$DummyAlert, {
    #Wait
    Sys.sleep(time=0.5)
    #Display
    shinyalert(
      inputId="ComputeAlert",
      title = "Listo!",
      text = "Descarga tus Resultados",
      closeOnEsc = FALSE, #Should be false
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE, #Should be false
      confirmButtonText = "Descargar",
      confirmButtonCol = "#428bca",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
  })
  
  #Hide Tab2 by default
  hideTab(inputId="NavPage1", target="Tab2")
  
  #When Calculate buttons is pressed:
  observeEvent(input$ComputeAlert, {
    #First hide Tab1
    hideTab(inputId="NavPage1", target="Tab1")
    #Then show Tab2 (hidden by default)
    showTab(inputId="NavPage1", target="Tab2")
    #Then go to Tab2
    updateTabsetPanel(session, "NavPage1", selected = "Tab2")
  })
  
  #Suggest Button - It's an internal joke for now. Could it be something in the future.
  observeEvent(input$SugBtn, {
    shinyalert(
      title = "",
      text = "AJAJAJAJAJAJAJAJAJAJAJAJAJAJAJA no.",
      closeOnEsc = FALSE, #Should be false
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = FALSE,
      showCancelButton = FALSE, #Should be false
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  #Plot
  observeEvent(input$PlotG1, {output$G1=renderPlot(PLSPlot()$p1)})
  observeEvent(input$PlotG2, {output$G2=renderPlot(PLSPlot()$p1v2)})
  observeEvent(input$PlotG3, {output$G3=renderPlot(PCAPlot()$PCA_NoLimits)})
  observeEvent(input$PlotG4, {output$G4=renderPlot(PCAPlot()$Scores_PCA_NoHotellings)})
  observeEvent(input$PlotG5, {output$G5=renderPlot(VIPPlot()$VIP_Plot)})
  
  #    _____                             
  #   |  __ \                            
  #   | |__) | __ ___   ___ ___  ___ ___ 
  #   |  ___/ '__/ _ \ / __/ _ \/ __/ __|
  #   | |   | | | (_) | (_|  __/\__ \__ \
  #   |_|   |_|  \___/ \___\___||___/___/
  #                                      
  #
  
  #Preare data
  ComputeData=eventReactive(input$DummyAlert, {
    
    #Take input
    DFData=as.data.frame(DFReact$DF)
    
    #Compute matrix of categories in form of 1 and 0 - This are the Response or Dependent Variable
    #Return also the counterData DF, which is the original data -This are the Predictors or Independt Variables
    DataFrameTransformed=TransformData(DF=DFData, 
                                       ColumnaCategoria=ColCatMin():ColCatMax(), 
                                       ColumnaMetabolitos=ColMeta())
    
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
    
  })
  
  #Read output value to display conditional panel
  output$ConditionalPanel=renderText({
    ComputeData()$Status
  })
  
  
  #     ____        _               _   
  #    / __ \      | |             | |  
  #   | |  | |_   _| |_ _ __  _   _| |_ 
  #   | |  | | | | | __| '_ \| | | | __|
  #   | |__| | |_| | |_| |_) | |_| | |_ 
  #    \____/ \__,_|\__| .__/ \__,_|\__|
  #                    | |              
  #                    |_|              
  
  #Graph PLS
  PLSPlot=eventReactive(input$Compute, { 
    
    #Take values from reactive Event
    plsrData=ComputeData()$plsrData
    LogicalMatrix=ComputeData()$LogicalMatrix
    customSize=5
    
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
    p1=ggplot(data=clplot, aes(x=`Comp 1`, y=`Comp 2`)) + geom_point() +
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
    p1=p1 + geom_text(data=clplot, nudge_y=0.05, size=customSize, aes(label=paste(1:nrow(clplot),substr(rownames(clplot),1,10), sep="-"))) +
      theme_blank
    
    
    
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
    p1v2=p1v2 + geom_text(data=clplot, size=customSize, aes(label=seq(1,nrow(clplot),by=1))) +
      theme_blank
    
    
    #Plotly
    p1Plotly=ggplotly(p1)
    p1v2Plotly=ggplotly(p1v2)
    
    list("p1Plotly"=p1Plotly, "p1v2Plotly"=p1v2Plotly, 
         "p1"=p1, "p1v2"=p1v2,
         "clplot"=clplot, "clplotY"=clplotY, "circ1"=circ1, "circ2"=circ2)
    
    
  })
  
  
  #Graph PCA
  PCAPlot=eventReactive(input$Compute,{
    
    #Take input Data
    pcaData=ComputeData()$pcaData
    Input=DFReact$DF
    labelPercent=ComputeData()$labelPercent
    ellTest=ComputeData()$ellTest
    clPCA=ComputeData()$clPCA
    circ1=PLSPlot()$circ1
    circ2=PLSPlot()$circ2
    plsrData=ComputeData()$plsrData
    
    #Score Plot SIN Hotellings T2-Test
    PCA_NoLimits=ggplot(data=pcaData,aes(x=as.numeric(pcaData$X1), y=as.numeric(pcaData$X2))) + 
      geom_point() + 
      geom_text(label=Input[,1], nudge_y=0.5) +
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
      theme_blank
    
    PCAP2Plotly=ggplotly(Scores_PCA_NoHotellings)
    
    
    
    #Grafico de Score CON Hotelling T2-Test
    Scores_PCA_Hotellings_NoLimits=ggplot(data=pcaData, aes(x=X1, y=X2)) + geom_point() + 
      geom_polygon(data=ellTest,aes(x=V1, y=V2),inherit.aes = F, colour="black", fill=NA, alpha=0.5) +
      geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
      labs(x=paste("PC-1 (",labelPercent[1],")", sep=""), y=paste("PC-2 (",labelPercent[2],")", sep="")) +
      geom_text(label=Input[,1], nudge_y=0.5) +
      theme_blank
    
    #With Limits
    Scores_PCA_Hotellings= Scores_PCA_Hotellings_NoLimits + scale_x_continuous(limits=c(-1*xlimitsRes,xlimitsRes), breaks=seq(-1*xlimitsRes,xlimitsRes, by=1)) +
      scale_y_continuous(limits=c(-1*ylimitsRes,ylimitsRes), breaks=seq(-1*ylimitsRes,ylimitsRes, by=1)) +
      theme_blank
    
    ###
    #Raw Plot
    CorrelationLoading_PCA=ggplot(data=clPCA, aes(x=X1, y=X2)) + geom_point() +
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
    CorrelationLoading_PCA=CorrelationLoading_PCA + geom_text(data=clPCA, nudge_y=0.05, size=3, aes(label=paste(1:nrow(clPCA),substr(rownames(clPCA),1,10), sep="-"))) +
      theme_blank
    
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
    CorrelationLoading_PCA_NoLabel=CorrelationLoading_PCA_NoLabel + geom_text(data=clPCA, size=3, aes(label=seq(1,nrow(clPCA),by=1))) +
      theme_blank
    
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
  })
  
  
  #Graph VIP
  VIPPlot=eventReactive(input$Compute, {
    VIP=ComputeData()$VIP
    
    #Plot VIP Ordenados
    VIP_Plot=ggplot(data=VIP, aes(x=reorder(VIP$Name, -distance), y=distance)) + 
      geom_col(width=0.5) + theme(axis.text.x=element_text(angle=90))
    #Labels
    VIP_Plot= VIP_Plot + labs(y = "Distancia", x = "Compuestos")
    #Limites
    VIP_Plot= VIP_Plot + scale_y_continuous(expand=c(0,0),limits=c(0,max(VIP$distance)),
                                            breaks=seq(0,round(max(VIP$distance), digits=1), by=0.05))
    
    list("VIP_Plot"=VIP_Plot)
    
  })
  
  
  #Download Tables PLS
  output$DownTablePLS=downloadHandler(
    filename = "PLS-Data.xlsx",
    content = function(file) {
      
      WB=createWorkbook(title="PLS-Data")
      addWorksheet(wb=WB, sheetName = "clplot")
      addWorksheet(wb=WB, sheetName = "clplotY")
      addWorksheet(wb=WB, sheetName = "circ1")
      addWorksheet(wb=WB, sheetName = "circ2")
      addWorksheet(wb=WB, sheetName= "vip")
      writeData(wb=WB, x=PLSPlot()$clplot, sheet="clplot")
      writeData(wb=WB, x=PLSPlot()$clplotY, sheet="clplotY")
      writeData(wb=WB, x=PLSPlot()$circ1, sheet="circ1")
      writeData(wb=WB, x=PLSPlot()$circ2, sheet="circ2")
      writeData(wb=WB, x=ComputeData()$VIP, sheet="vip")
      saveWorkbook(wb=WB, file=file)
      
    }
  )
  
  #Donwload Figures PLS
  output$DownFigPLS=downloadHandler(
    filename="PLS-Plots.pdf",
    content=function(file){
      pdf(file, width=12, height=10)
      print(PLSPlot()$p1)
      print(PLSPlot()$p1v2)
      print(VIPPlot()$VIP_Plot)
      dev.off()
    }
  )
  
  #Download Tables PCA
  output$DownTablePCA=downloadHandler(
    filename = "PCA-Data.xlsx",
    content = function(file) {
      
      WB=createWorkbook(title="PCA-DATA")
      addWorksheet(wb=WB, sheetName = "pcaData")
      addWorksheet(wb=WB, sheetName = "ellTest")
      writeData(wb=WB, sheet="pcaData", x=ComputeData()$pcaData)
      writeData(wb=WB, sheet="ellTest", x=ComputeData()$ellTest)
      saveWorkbook(wb=WB, file=file)
      
    }
  )
  
  
  #Donwload Figures PCA
  output$DownFigPCA=downloadHandler(
    filename="PCA-Plots.pdf",
    content=function(file){
      pdf(file, width=10, height=10)
      print(PCAPlot()$PCA_NoLimits)
      print(PCAPlot()$Scores_PCA_NoHotellings)
      print(PCAPlot()$Scores_PCA_Hotellings_NoLimits)
      print(PCAPlot()$Scores_PCA_Hotellings)
      print(PCAPlot()$CorrelationLoading_PCA)
      print(PCAPlot()$CorrelationLoading_PCA_NoLabel)
      dev.off()
    }
  )
  
}) #End of file

