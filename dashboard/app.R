library(shiny)
library(shinydashboard)

# Load Data ---------------------------------------------------------------
source('../functions/shiny_functions.R')
rawdata <- readRDS('../data/model_dset.RDS')

# Uni ---------------------------------------------------------------------


unidata <- rawdata %>% 
  select(-tourney_level, -surface, -round)

unicolnames <- varinfo(unidata) %>% select(-ends_with("id")) %>% 
  filter(ndistinct > 1, 
         varclass == "logical" |
           varclass == "numeric" | 
           (varclass == "character" & ndistinct < 20)) %>%
  pull(colname) %>% sort()


# Bi ----------------------------------------------------------------------

bidata <- rawdata %>% 
  select(-tourney_level, -surface, -round) %>% 
  mutate_if(.predicate = function(x) class(x) == "logical", 
            .funs = function(x) as.numeric(x))


bicolnames <- varinfo(bidata) %>% select(-ends_with("id")) %>% 
  filter(ndistinct > 20, varclass == "numeric") %>%
  pull(colname) %>% sort()

# Lift --------------------------------------------------------------------

liftdata <- rawdata %>% 
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


# Define UI ---------------------------------------------------------------


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(id = "uni",
             text = "Univariate",
             icon = icon("signal"),
             tabName = "unitab"),
    menuItem(id = "bi",
             text = "Bivariate",
             icon = icon("signal"),
             tabName = "bitab"),
    menuItem(id = "lift",
             text = "Lift",
             icon = icon("signal"),
             tabName = "signal")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "unitab",
            fluidPage(
              
              # Application title
              titlePanel("Univariate Information"),
              
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "univar",
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
                  plotOutput(outputId = "uniplot", 
                             width = "100%",
                             height = "400px")
                )
              )
            )
            # h1("Explore Univariate Distributions")
    ),
    tabItem(tabName = "bitab",
            box(title = "Select two variables")
            # h1("Explore Variable Relationships")
    ),
    tabItem(tabName = "signal",
            box(title = "Select X variable")
            # h1("Explore Univariate Relationships with Predictor")
    )
  )  
)

ui <- dashboardPage(dashboardHeader(title = "WTA Predictive Model Data"),
                    sidebar,
                    body)


# Define Server -----------------------------------------------------------


server <- function(input, output) {
  output$uniplot <- renderPlot({
    
    if(length(input$mytransform) > 1) {
      stop("Max 1 transformation")
    }
    
    if(is.null(input$mytransform)) {
      
      plotdata <- unidata
      
    } else {
      
      plotdata <- unidata %>% 
        mutate_at(.vars = input$univar, 
                  .funs = eval(parse(text = input$mytransform)))
      
    }
    
    if(length(unique(unidata[,input$univar])) > 20) {
      
      ggplot(data = plotdata) +
        geom_density(aes_string(x = input$univar)) + 
        labs(title = glue("Density of Values for {input$univar}"))
      
    } else {
      
      ggplot(data = plotdata) +
        geom_bar(aes_string(x = input$univar)) + 
        labs(title = glue("Frequency of Values for {input$univar}"))
      
    } 
  })
}

shinyApp(ui, server)