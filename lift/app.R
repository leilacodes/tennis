source('../functions/shiny_functions.R')
library(shiny)
library(dplyr)
library(ggplot2)
library(glue)

mydata <- readRDS('../data/model_dset.RDS') %>% 
  select(-tourney_level, -surface, -round) %>% 
  mutate_if(.predicate = function(x) class(x) == "logical", 
            .funs = function(x) as.numeric(x))

colnames <- varinfo(mydata) %>% select(-ends_with("id")) %>% 
  filter(colname != "win",
         ndistinct > 1, 
         varclass == "logical" |
           varclass == "numeric" | 
           (varclass == "character" & ndistinct < 20)) %>%
  pull(colname) %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Explore Lift"),
  
  # Sidebar with a dropdown menu for variable
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x",
                  label = "Select X variable:", 
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

# Define server logic required to draw plot
server <- function(input, output) {
  output$varplot <- renderPlot({
    plot_lift_range(dset = mydata, yvar = quo(win), xvar =)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

