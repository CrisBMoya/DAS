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

#Method to reset shiny page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


shinyUI(fluidPage(
  
  #Allow shiny to use JavaScript in UI
  useShinyjs(),                                           
  extendShinyjs(text = jsResetCode),                      
  
  #Page title on browser
  title="PLS-PCA",
  
  fluidRow(
    column(3,
           #Page title
           titlePanel("PLS & PCA")),
    column(6),
    column(3,
           
           #Button to Reset Shiny
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
    )
  ),
  #Separator line
  hr(),
  
  #Tabs
  navbarPage(title="Analisis: ", id="NavPage1",
             tabPanel(title="Cargar Datos", value="Tab1",
                      
                      fluidRow(
                        
                        #Left Column with options and general setup
                        column(3,
                               #Titulo
                               titlePanel("Setup"),
                               hr(),
                               
                               #Load Selected File
                               fileInput(inputId="file1", label='Subir Excel', accept = c(".xlsx")),
                               
                               #Choose Sheet Name
                               uiOutput(outputId="SheetName"),
                               
                               #Button to Load the DF
                               uiOutput(outputId="SPACER"),
                               uiOutput(outputId="LoadFileUI"),
                               
                               #tags$p(tags$b("Cargar Datos:")),
                               #actionButton(inputId="LoadFile", label="Cargar Tabla"),
                               #Options
                               hr(),
                               tags$p("Rango Categorias:"),
                               
                               splitLayout(
                                 #Range of categorical variables
                                 numericInput(inputId="Cat1", label="Desde", 
                                              value=0, step=1),
                                 numericInput(inputId="Cat2", label="Hasta", 
                                              value=0, step=1)
                               ),
                               #Column which is holding the metabolites
                               numericInput(inputId="Met1", label="Columna metabolitos:", 
                                            value=0, step=1)
                               
                               
                               
                               
                        ),
                        
                        #Right column with dataframe output
                        column(6,
                               #Output Table
                               box(
                                 title = "Data Frame:", width = NULL, status = "primary",
                                 div(style = 'overflow-x: scroll', DT::dataTableOutput('contents'))
                               )
                        ),
                        
                        column(3,
                               #Titulo
                               titlePanel("Editar Tabla"),
                               #Wether remove or not a row
                               hr(),
                               tags$p("¿Remover fila?"),
                               textInput(inputId="RemoveRow", label="Nombre de Fila a remover", value=""),
                               
                               #Button to remove
                               actionButton(inputId="DeleteRowBtn", label="Borrar"),
                               
                               #Titulo
                               #titlePanel("Output"),
                               hr(),
                               #Compute Data Alert Panel
                               useShinyalert(),
                               
                               #Compute
                               #Button to Load the DF
                               uiOutput(outputId="CalcularUI"),
                               
                               #Conditional signal to show
                               div(style="visibility: hidden;", textOutput(outputId="ConditionalPanel"))
                               
                               
                        )
                        
                      )
             ), #End of First Panel
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
                                 numericInput(inputId="DotSize", label="Puntos", value=1, min=1, max=20, step=1),
                                 numericInput(inputId="TextSize", label="Textos", value=1, min=1, max=20, step=1)),
                               
                               #Width and height
                               tags$p("Dimensiones de gráfico:"),
                               splitLayout(
                                 numericInput(inputId="Width", label="Ancho", value=1, min=1, max=20, step=1),
                                 numericInput(inputId="Height", label="Alto", value=1, min=1, max=20, step=1)),
                               
                               #Plot Button
                               hr(),
                               radioButtons(inputId="PlotChoose", label="Graficar:", 
                                            choices=list("G1"=1, "G2"=2,
                                                         "G3"=3, "G4"=4,
                                                         "G5"=5), 
                                            selected=1)
                        ),
                        
                        column(7,
                               
                               #Place to put Plots
                               plotlyOutput(outputId="PlotlyOutput") %>% withSpinner(color="#0dc5c1")
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
  )
)
)

