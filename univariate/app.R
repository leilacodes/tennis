# Univariate

# Load Data ---------------------------------------------------------------


source('../functions/shiny_functions.R')
library(shiny)

unidata <- readRDS('../data/model_dset.RDS') %>% 
  select(-tourney_level, -surface, -round)

unicolnames <- varinfo(unidata) %>% select(-ends_with("id")) %>% 
  filter(ndistinct > 1, 
         varclass == "logical" |
           varclass == "numeric" | 
  (varclass == "character" & ndistinct < 20)) %>%
  pull(colname) %>% sort()


# Define UI for application that draws a histogram ------------------------


ui <- fluidPage(
   
   # Application title
   titlePanel("Univariate Information"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "myvar",
                    label = "Select columns:", 
                    choices = unicolnames, 
                    selected = unicolnames[1], 
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL, 
                    size = NULL),
        checkboxGroupInput(inputId = "mytransform",
                           label = "Transform Variable?", 
                           choices = transformations,
                           selected = NULL,
                           inline = TRUE, 
                           width = NULL)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "varplot", 
                   width = "100%",
                   height = "400px")
      )
   )
)


# Define server logic required to draw a histogram ------------------------


server <- function(input, output) {
  output$varplot <- renderPlot({
    
    if(length(input$mytransform) > 1) {
      stop("Max 1 transformation")
    }
    
    if(is.null(input$mytransform)) {
      
      plotdata <- unidata
      
    } else {
      
      plotdata <- unidata %>% 
        mutate_at(.vars = input$myvar, 
                  .funs = eval(parse(text = input$mytransform)))
      
    }
    
    if(length(unique(unidata[,input$myvar])) > 20) {
      
      ggplot(data = plotdata) +
        geom_density(aes_string(x = input$myvar)) + 
        labs(title = glue("Density of Values for {input$myvar}"))
      
    } else {
      
      ggplot(data = plotdata) +
        geom_bar(aes_string(x = input$myvar)) + 
        labs(title = glue("Frequency of Values for {input$myvar}"))
      
    } 
    })
}


# Run the application  ----------------------------------------------------


shinyApp(ui = ui, server = server)

