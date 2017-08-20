# Shiny application for filtering tennis ranking datset


# 0. Load packages --------------------------------------------------------

require(shiny)
require(dplyr)
require(DT)
require(shinydashboard)
require(data.table)
require(RCurl)

# 1. Import data ----------------------------------------------------------

url <- getURL("https://raw.githubusercontent.com/samperochkin/tennis-analytics/master/Data/Raw/DataRanking.csv")
data_ranking <- data.table(read.csv(textConnection(url)))
data_ranking$ranking_date <- as.Date(data_ranking$ranking_date)

# 2. Define filtering function --------------------------------------------

filtering <- function(age_input, ranking_input, stat_input){
  data_filtered <- copy(data_ranking)
  if (stat_input == "entry"){
    data_filtered <- data_filtered[age <= age_input & rank <= ranking_input, .(ranking_date = min(ranking_date, na.rm = TRUE)), id]
  } else {
    data_filtered <- data_filtered[age <= age_input & rank <= ranking_input, .(rank = min(rank, na.rm = TRUE)), .(id)]
    data_filtered <- data.table(left_join(data_filtered, data_ranking, by = c("id", "rank")))
    data_filtered <- data_filtered[, .(ranking_date = min(ranking_date, na.rm = TRUE)), id]
  }
  data_filtered <- data.table(left_join(data_filtered, data_ranking, by = c("id", "ranking_date")))
}

# 3. UI  ------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Tennis Ranking"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ranking", tabName = "ranking", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ranking",
              box(
                numericInput("age", label = "Age", value = 20, min = 15, max = 75, step = 1),
                numericInput("ranking", label = "Ranking", value = 30, min = 1, max = 500, step = 1),
                selectInput("stat", label = "Stat", choices = c("Première entrée" = "entry", "Maximum atteint" = "max"), multiple = FALSE, selected = "entry")
              ),
              dataTableOutput("table")
      )
    )
  )
)


# 4. Server ---------------------------------------------------------------

server <- function(input, output, session) {
  
  output$table <- DT::renderDataTable({
    filtering(input$age, input$ranking, input$stat)
  })
}


# 5. Run App --------------------------------------------------------------

shinyApp(ui, server)
