library(shiny)
library(shinydashboard)
source('../functions/shiny_functions.R')


# Load Data ---------------------------------------------------------------

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
            box(title = "Select Variable")
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
}

shinyApp(ui, server)