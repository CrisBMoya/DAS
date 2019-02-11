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

#Load modules
source(file="~/RFiles/Shiny/DAS/Modules/TabPlot&Descarga.R")
source(file="~/RFiles/Shiny/DAS/Modules/CargaDeDatos.R")

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
             TabCargaDeDatos(id="TabCargaDeDatos")
             , #End of First Panel
             TabPlotDescarga(id="TabPlot&Descarga")
  )
)
)

