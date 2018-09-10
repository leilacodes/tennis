source('../functions/shiny_functions.R')

bidata <- readRDS('../data/model_dset.RDS') %>% 
  select(-tourney_level, -surface, -round) %>% 
  mutate_if(.predicate = function(x) class(x) == "logical", 
            .funs = function(x) as.numeric(x))

bicolnames <- varinfo(bidata) %>% select(-ends_with("id")) %>% 
  filter(ndistinct > 20, varclass == "numeric") %>%
  pull(colname) %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Explore Correlations"),
  
  # Sidebar with a dropdown menu for variable
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x",
                  label = "Select first variable:", 
                  choices = bicolnames, 
                  selected = bicolnames[1], 
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL, 
                  size = NULL),
      selectInput(inputId = "y",
                  label = "Select second variable:", 
                  choices = bicolnames, 
                  selected = bicolnames[2], 
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

      ggplot(data = bidata) +
        geom_point(aes_string(x = input$x, y = input$y)) + 
        labs(title = glue("Scatterplot of {input$y} vs. {input$x}"))
 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

