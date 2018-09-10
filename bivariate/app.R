# Bivariate
source('../functions/shiny_functions.R')

mydata <- readRDS('../data/model_dset.RDS') %>% 
  mutate(win = ifelse(win == TRUE, "win", "loss"))
  # mutate_if(.predicate = function(x) class(x) == "logical", 
  #           .funs = function(x) as.numeric(x))

colnames <- varinfo(mydata) %>% select(-ends_with("id")) %>% 
  filter(varclass == "logical" |
         varclass == "numeric" |
           (ndistinct < 10 & varclass == "character")) %>%
  pull(colname) %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Explore Correlations"),
  
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
                  size = NULL),
      checkboxGroupInput(inputId = "xtransform",
                         label = "Transform X?", 
                         choiceNames = c("log", "sqrt", "sq"),
                         choiceValues = c("log", "sqrt", "function(x) x^2"),
                         selected = NULL,
                         inline = TRUE, 
                         width = NULL),
      selectInput(inputId = "y",
                  label = "Select Y variable:", 
                  choices = colnames, 
                  selected = "win", 
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL, 
                  size = NULL),
      checkboxGroupInput(inputId = "ytransform",
                         label = "Transform Y?", 
                         choices = c("log", "sqrt", "sq"),
                         choiceValues = c("log", "sqrt", "function(x) x^2"),
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

# Define server logic required to draw plot
server <- function(input, output) {
  output$varplot <- renderPlot({

    nx <- length(unique(mydata[,input$x]))
    ny <- length(unique(mydata[,input$y]))
    
    if (is.null(input$xtransform) & is.null(input$ytransform)) {
      
      plotdata <- mydata
      
    } else if (is.null(input$xtransform) &
               !is.null(input$ytransform)) {
      
      plotdata <- mydata %>% 
        mutate_at(.vars = input$y, .funs = eval(parse(text = input$ytransform)))
      
    } else if (!is.null(input$xtransform) &
               is.null(input$ytransform)) {
      
      plotdata <- mydata %>% 
        mutate_at(.vars = input$x, .funs = eval(parse(text = input$xtransform)))
      
    } else if (!is.null(input$xtransform) &
               !is.null(input$ytransform)) {
      
      plotdata <- mydata %>% 
        mutate_at(.vars = input$x, .funs = eval(parse(text = input$xtransform))) %>% 
        mutate_at(.vars = input$y, .funs = eval(parse(text = input$ytransform)))
    }
    
    if (nx > 10 & ny > 10) {
      
      ggplot(data = plotdata) +
        geom_point(aes_string(x = input$x, y = input$y)) + 
        labs(title = glue("Scatterplot of {input$y} vs. {input$x}"))
      
    } else if (nx < 10 & ny > 10) {
      ggplot(data = plotdata) +
        geom_histogram(aes_string(x = input$y, 
                                  group = input$x,
                                  fill = input$x), 
                       position = "identity",
                       alpha = .75) + 
        labs(title = glue("Density of {input$y} by {input$x}"))
    } else if (nx > 10 & ny < 10) {
      ggplot(data = plotdata) +
        geom_histogram(aes_string(x = input$x, 
                                  group = input$y,
                                  fill = input$y), 
                       position = "identity",
                       alpha = .75) + 
        labs(title = glue("Density of {input$x} by {input$y}"))
    } else {
      ggplot(data = plotdata) +
        geom_bar(aes_string(x = input$x, 
                            group = input$y,
                            fill = input$y),
                 position = "dodge") + 
        labs(title = glue("Frequency of {input$x} by {input$y}"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

