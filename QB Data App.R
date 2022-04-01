#Libraries
library(pacman)
p_load(rvest,tidyverse,RSelenium,dplyr,formattable,shiny,shinythemes)

#Prerequisite File
####create link for Tom Brady career stats
link <- "https://en.wikipedia.org/wiki/List_of_starting_quarterbacks_in_the_National_Football_League"
####Pull stats 
Stats <- read_html(link) %>% 
  html_nodes(".mw-parser-output") %>% 
  html_table()
####Clean data frame
QBdf <- Stats[[1]]
QBdf <- QBdf[-c(33:43),-c(3:14)]
QBdf$Team <- gsub("list", "", as.character(QBdf$Team))
QBdf$Team <- gsub("[()]", "", as.character(QBdf$Team))
QBdf <- QBdf %>% mutate(QBlink = str_replace(QBdf$Quarterback, " ","-"))
QBdf <- QBdf[order(QBdf$Quarterback),]

#Shiny App
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(
    "Starting Quaterback Data"),
  
  selectInput(inputId = "qb", label = "Quarterback", choices = QBdf$QBlink),
  downloadButton("QB_file","Download"),
  tableOutput("table")
)

server <- function(input, output, session) {
  QBtable <- reactive({
    link <- paste("https://www.nfl.com/players/",input$qb,"/stats/career", sep = "")
    career <- read_html(link) %>% 
      html_nodes(".nfl-o-roster") %>% 
      html_table() %>%
      pluck(1)
  })
  output$table <- renderTable({
    QBtable()
  })
  output$QB_file <- downloadHandler(
    filename = function(){
      paste(input$qb,".csv",sep = "")
    },
    content = function(file){
      write.csv(QBtable(), file)
    }
  )
}

shinyApp(ui, server)