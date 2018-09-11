# Univariate
source('../functions/shiny_functions.R')
library(shiny)

mydata <- readRDS('../data/model_dset.RDS') %>% 
  select(-tourney_level, -surface, -round)

colinfo <- varinfo(mydata)

colnames <- colinfo %>% select(-ends_with("id")) %>% 
  filter(ndistinct > 1, 
         varclass == "logical" |
           varclass == "numeric" | 
  (varclass == "character" & ndistinct < 20)) %>%
  pull(colname) %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Univariate Information"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "myvar",
                    label = "Select columns:", 
                    choices = colnames, 
                    selected = colnames[1], 
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL, 
                    size = NULL)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "varplot", 
                   width = "100%",
                   height = "400px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$varplot <- renderPlot({
    if(length(unique(mydata[,input$myvar])) > 20) {
      ggplot(data = mydata) +
        geom_density(aes_string(x = input$myvar)) + 
        labs(title = glue("Density of Values for {input$myvar}"))
    } else {
      ggplot(data = mydata) +
        geom_bar(aes_string(x = input$myvar)) + 
        labs(title = glue("Frequency of Values for {input$myvar}"))
    } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

