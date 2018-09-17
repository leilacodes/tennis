source('../functions/shiny_functions.R')

liftdata <- readRDS('../data/model_dset.RDS') %>% 
  select(-tourney_level, -surface, -round) %>% 
  mutate_if(.predicate = function(x) class(x) == "logical", 
            .funs = function(x) as.numeric(x))

liftcolnames <- varinfo(liftdata) %>% select(-ends_with("id")) %>% 
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
                  choices = liftcolnames, 
                  selected = liftcolnames[1], 
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
    
    if(!is.numeric(liftdata[, input$x]) & length(unique(liftdata[, input$x])) > 10) {
      stop("Numeric or categorical only")
    }
    
    if(length(unique(liftdata[, input$x])) > 10) {
      
      plot_lift_range(dset = liftdata, xvar = input$x)
      
    } else {
      
      plot_lift(dset = liftdata, xvar = input$x)
      
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

