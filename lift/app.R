# Lift app
# TODO: Why is the content not showing?
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
      radioButtons(inputId = "liftvar", 
                   label = "Select X variable:",
                   choices = liftcolnames, 
                   selected = liftcolnames[1], 
                   inline = FALSE),
      checkboxGroupInput(inputId = "lifttransform",
                         label = "Transform Variable?", 
                         choices = transformations,
                         selected = NULL,
                         inline = TRUE, 
                         width = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "varplot")
    )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
  output$varplot <- renderPlot({
    
    if(!is.numeric(liftdata[, input$liftvar]) & 
       length(unique(liftdata[, input$liftvar])) > 0) {
      stop("Numeric or categorical only")
    }
    
    
    if(length(input$lifttransform) > 1) {
      stop("Max 1 transformation")
    }
    
    if(is.null(input$lifttransform)) {
      
      plotdata <- liftdata
      
    } else {
      
      plotdata <- liftdata %>% 
        mutate_at(.vars = input$liftvar,
                  .funs = function(x) {
                    x0 <- x
                    x0[which(x==0)] <- .000000001
                    return(x0)
                  }) %>% 
        mutate_at(.vars = input$liftvar, 
                  .funs = eval(parse(text = input$lifttransform)))
      
    }
    
    if(length(unique(liftdata[, input$liftvar])) > 10) {
      
      plot_lift_range(dset = plotdata, xvar = input$liftvar)
      
    } else {
      
      plot_lift(dset = plotdata, xvar = input$liftvar)
      
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

