TabCargaDeDatos=function(id, label="TabCargaDatos"){
  
  ns=NS(id)
  
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
                                 value=0, step=1),
                    
                    #Suggestions - Not working for now.
                    hr(),
                    textInput(label="Sugerencias?", inputId="Sug"),
                    actionButton(inputId="SugBtn", label="Enviar Comentarios")
                    
                    
                    
             ),
             
             #Right column with dataframe output
             column(6,
                    #Output Table
                    box(
                      title = "Tabla de Datos:", width = NULL, status = "primary",
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
  )
}