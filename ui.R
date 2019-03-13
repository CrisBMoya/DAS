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
source(file="~/RFiles/Shiny/DAS/Modules/UIModules/TabPlot&Descarga.R")
source(file="~/RFiles/Shiny/DAS/Modules/UIModules/CargaDeDatos.R")
source(file="~/RFiles/Shiny/DAS/Modules/UIModules/LandingTab.R")

#Method to reset shiny page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


shinyUI(fluidPage(
  
  #Allow shiny to use JavaScript in UI
  useShinyjs(),                                           
  extendShinyjs(text = jsResetCode),                      
  
  #Page title on browser
  title="DES",
  
  fluidRow(
    column(3,
           #Page title
           uiOutput(outputId="PageTitle")),
    column(6),
    column(3,
           
           #Button to Reset Shiny
           uiOutput(outputId="ResetBtn")
    )
  ),
  #Separator line
  hr(),
  
  #Tabs
  navbarPage(title="", id="NavPage1",
             #Landing Tab
             tabPanel(title="", value="Tab0", fluidRow()),
             #First Tab
             TabCargaDeDatos(id="TabCargaDeDatos"),
             #Second Tab
             TabPlotDescarga(id="TabPlot&Descarga")
  )
)


)

