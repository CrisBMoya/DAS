TabPlotDescarga=function(id, label="TabPlotDescarga"){  
  
  ns=NS(id)
  
  tabPanel(title="Graficar y Descargar", value="Tab2",
           fluidRow(
             column(2,
                    #Titulo
                    titlePanel("Estetica:"),
                    hr(),
                    
                    #Estetic Pannel
                    #Size of dots and text
                    tags$p("Tamaño de Puntos y Texto:"),
                    splitLayout(
                      
                      numericInput(inputId="DotSize", label="Puntos", value=1, min=0, max=100, step=1),
                      numericInput(inputId="TextSize", label="Textos", value=5, min=1, max=20, step=1)),
                    
                    #Width and height
                    tags$p("Dimensiones de gráfico:"),
                    splitLayout(
                      numericInput(inputId="Width", label="Ancho", value=1, min=1, max=20, step=1),
                      numericInput(inputId="Height", label="Alto", value=1, min=1, max=20, step=1)),
                    #numericInput(inputId="ShapeLogic", label="Shape?", value=0, min=0,max=1, step=0),
                    checkboxInput(inputId="ShapeLogic", label="Graficar con formas?", value=FALSE)
                    
                    
             ),
             
             column(7,
                    
                    
                    #Tabset to Output Plots in an organized manner
                    tabsetPanel(type = "tabs",
                                tabPanel(title="G1", 
                                         actionButton(inputId="PlotG1", label="Plot G1"),
                                         plotOutput(outputId="G1")),
                                tabPanel(title="G2", 
                                         actionButton(inputId="PlotG2", label="Plot G2"),
                                         plotOutput(outputId="G2")),
                                tabPanel(title="G3", 
                                         actionButton(inputId="PlotG3", label="Plot G3"),
                                         plotOutput(outputId="G3")),
                                tabPanel(title="G4", 
                                         actionButton(inputId="PlotG4", label="Plot G4"),
                                         plotOutput(outputId="G4")),
                                tabPanel(title="G5", 
                                         actionButton(inputId="PlotG5", label="Plot G5"),
                                         plotOutput(outputId="G5")))
                    
                    
             ),
             
             column(3,
                    #Title
                    titlePanel("Descargar"),
                    
                    #Conditional panel
                    hr(),
                    conditionalPanel(condition="output.ConditionalPanel=='DONE!'",
                                     splitLayout(
                                       downloadButton(outputId="DownTablePLS", label="Tablas PLS"),
                                       downloadButton(outputId="DownTablePCA", label="Tablas PCA")
                                     ),
                                     splitLayout(
                                       downloadButton(outputId="DownFigPLS", label="Graficos PLS"),
                                       downloadButton(outputId="DownFigPCA", label="Graficos PCA")
                                     )
                    )
                    
             )
           )
  ) #End of Second Panel
  
}