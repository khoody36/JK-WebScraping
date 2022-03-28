#Libraries
library(pacman)
p_load(rvest,tidyverse,RSelenium,dplyr,formattable,shiny)

#Prerequisite File
####create link for Tom Brady career stats
link <- "https://www.profootballnetwork.com/nfl-starting-quarterbacks-2021/"
####Pull stats 
Stats <- read_html(link) %>% 
  html_nodes(".tdb-block-inner li") %>% 
  html_text(trim = T) %>%
  str_squish()
####Clean data frame
QBdf <- as.data.frame(Stats) %>% separate(Stats, sep = ": ", into = c("Team","QB"))
QBdf[QBdf == "Kyle Trask"] <- "Tom Brady"
QBdf[QBdf == "Feleipe Franks"] <- "Marcus Mariota"
QBdf[QBdf == "Drew Lock/Geno Smith"] <- "Drew Lock"
QBdf[QBdf == "Jimmy Garoppolo/Trey Lance"] <- "Jimmy Garoppolo"
QBdf[QBdf == "Mason Rudolph/Dwayne Haskins"] <- "Mason Rudolph"
QBdf <- QBdf[order(QBdf$QB),]
QBdf <- QBdf %>% mutate(QBlink = str_replace(QBdf$QB, " ","-"))

#Shiny App
ui <- fluidPage(
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