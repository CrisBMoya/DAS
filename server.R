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
"~/GithubProjects/Shiny/"
#Source Functions and Modules
source(file="~/GithubProjects/Shiny/DAS/Scripts/simpleEllipse[FUNCTION].R")
source(file="~/GithubProjects/Shiny/DAS/Scripts/LogicalMatrix[FUNCTION].R")
source(file="~/GithubProjects/Shiny/DAS/Scripts/VIP[FUNCTION].R")
source(file="~/GithubProjects/Shiny/DAS/Scripts/LimitFUN[FUNCTION].R")
source(file="~/GithubProjects/Shiny/DAS/Modules/ServerModules/PLSPlotModule.R")
source(file="~/GithubProjects/Shiny/DAS/Modules/ServerModules/PCAPlotModule.R")
source(file="~/GithubProjects/Shiny/DAS/Modules/ServerModules/ComputeDataModule.R")
source(file="~/GithubProjects/Shiny/DAS/Modules/UIModules/LandingTab.R")
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
  
  #Hide Navigation Pane by default
  hide(id="NavPage1")
  
  #Show landing popup, as a modal. Landing Page.
  LandingTab()
  
  #When one of the analysis is chosen
  observeEvent(input$An1, {
    #Remove Modal
    removeModal()
    
    #Add Title to page for this specific analysis
    output$PageTitle=renderUI({
      titlePanel(title="PLS & PCA")
    })
    
    #Reset Button
    output$ResetBtn=renderUI({
      actionButton(inputId="reset_button", label="Resetear", icon=icon("times"), 
                   style="color: #fff; 
                        border-radius: 10px;
                        background-color: #d2322d;
                        border-color: #ac2925;
                        font-size: 15px;
                        padding: 6px 30px;
                        display: inline-block;
                        margin-top: 20px; 
                        float:right;")
    })
    
    #Hide landing Tab
    hideTab(inputId="NavPage1", target="Tab0")
    #Show first Tab1
    showTab(inputId="NavPage1", target="Tab1")
    #Then go to Tab1
    updateTabsetPanel(session, "NavPage1", selected = "Tab1")
  })
  
  #Disable some optons by default
  shinyjs::disable(id="Cat1")
  shinyjs::disable(id="Cat2")
  shinyjs::disable(id="Met1")
  shinyjs::disable(id="RemoveRow")
  shinyjs::disable(id="An2")
  shinyjs::disable(id="An3")
  
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
  
  #Shape Matching
  DB=read.table(file="~/GithubProjects/Shiny/DAS/DB/MetabolitesDB.txt", header=TRUE, sep="\t")
  
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
  
  #Plot every graph
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
  ComputeData=eventReactive(input$DummyAlert, {
    callModule(module=ComputeDataModule, id="", DFData=as.data.frame(DFReact$DF),
               ColCatMin=ColCatMin(), ColCatMax=ColCatMax(), ColMeta=ColMeta())
    
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
  
  #Point Size
  DotSizeReact=reactive({
    input$DotSize
  })
  
  #Text Size
  TextSizeReact=reactive({
    input$TextSize
  })
  
  #Shape Logic
  ShapeLogicReact=reactive({
    input$ShapeLogic
  })
  
  PLSPlot=reactive({
    callModule(module=PLSPlotModule, id="", plsrData=ComputeData()$plsrData, 
               LogicalMatrix=ComputeData()$LogicalMatrix,
               themeCustom=theme_blank, 
               DotSize=DotSizeReact(),
               TextSize=TextSizeReact(),
               Shape=DB,
               ShapeLogic=ShapeLogicReact())
  })
  
  #Plot PLS
  # PLSPlot=eventReactive(input$Compute, {
  #   callModule(module=PLSPlotModule, id="", plsrData=ComputeData()$plsrData, 
  #              LogicalMatrix=ComputeData()$LogicalMatrix,
  #              themeCustom=theme_blank, DotSize=SizeDot())
  # })
  # 
  #Plot PCA
  # PCAPlot=eventReactive(input$Compute, {
  #   callModule(module=PCAPlotModule, id="", pcaData=ComputeData()$pcaData, 
  #              Input=DFReact$DF, labelPercent=ComputeData()$labelPercent,
  #              ellTest=ComputeData()$ellTest, 
  #              clPCA=ComputeData()$clPCA, 
  #              circ1=PLSPlot()$circ1, circ2=PLSPlot()$circ2, 
  #              plsrData=ComputeData()$plsrData,
  #              themeCustom=theme_blank, DotSize=SizeDot())
  # })
  
  PCAPlot=reactive({
    callModule(module=PCAPlotModule, id="", pcaData=ComputeData()$pcaData, 
               Input=DFReact$DF, labelPercent=ComputeData()$labelPercent,
               ellTest=ComputeData()$ellTest, 
               clPCA=ComputeData()$clPCA, 
               circ1=PLSPlot()$circ1, circ2=PLSPlot()$circ2, 
               plsrData=ComputeData()$plsrData,
               themeCustom=theme_blank, 
               DotSize=DotSizeReact(),
               TextSize=TextSizeReact())
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
  
}) 
#End of file

