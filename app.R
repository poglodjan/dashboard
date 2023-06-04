library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(bslib)
library(plotly)
library(stringi)
library(stringr)

data <- read.csv("ViewingActivity.csv")
exploring <- data %>% mutate(Profile.Name = ifelse(Profile.Name=="Magda Papierz",
                                                   "Magda", Profile.Name)) %>%
  mutate(Profile.Name = ifelse(Profile.Name=="Pasożyt", "Janek", Profile.Name)) %>%
  mutate(Duration = str_sub(Duration,end=-4)) %>%
  mutate(hours = as.numeric(substr(Duration,1,2)),
         minutes = as.numeric(substr(Duration,4,5))) %>%
  mutate(NewDuration = hours * 60 + minutes) %>%
  mutate(datetime = str_sub(Start.Time,end=-13)) %>%
  mutate(Year = substr(datetime,1,4),
         Month = substr(datetime,4,4)) %>%
  mutate(licznik_filmow=ifelse(!grepl(":",Title),1,0)) %>%
  mutate(licznik_serialow=ifelse(grepl(":",Title),1,0)) %>%
  mutate(Title=gsub(":.*", "", Title)) 

country <- map_data("world")

ui <- fluidPage(
    theme = bs_theme(
      bg = "#101010", 
      fg = "#FDF7F7", 
      primary = "red", 
      base_font = font_google("Prompt"),
      code_font = font_google("JetBrains Mono")
    ),
    
    tags$head(
      tags$style("
      .title {
        font-family: 'Netflix Sans', Arial, sans-serif;
        text-align: center;
        font-size: 24px;
      }
      .input1 {
        font-family: 'Netflix Sans', Arial, sans-serif;
        width: 200px;
        text-align:center;
        margin: 0 auto;
      }
      .input2 {
        width: 200px;
        text-align:center;
        margin: 0 auto;
      }
      .distPlot{
        width: 700px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      .tabelka{
        width: 200px;
        margin: 0 auto;
        border-radius: 8px;
      }
      .slupkowy{
        color: black;
        width:400px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      
      .mapka{
        color: black;
        width: 500px;
        height: 600px;
        margin: 0 auto;
        margin-top: 40px;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
        text-align: center;
      }
      .okno1{
      margin: 0 auto;
      width:800px;
      margin-top: 20px;
      margin-bottom: 20px;
      height:520px;
      border: 4px solid white; 
      }
      
      .okno2{
      margin: 0 auto;
      width:800px;
      margin-top: 20px;
      margin-bottom: 20px;
      height: 480px;
      border: 4px solid white; 
      }
      
      .okno3{
      margin: 0 auto;
      width:800px;
      margin-top: 20px;
      margin-bottom: 20px;
      height: 700px;
      border: 4px solid white; 
      }
      
    ")
    ),
    
    div(
      class = "title",
      "Analiza trendów u znajomych na Netflixie"
    ),
    
    
    fluidRow(
      column(12, 
             
             div(
               class = "input1",
               selectInput("Osoba","Wybierz osobę:",unique(exploring$Profile.Name))
             ),
             
             div(
               class = "okno1",
             div(
               class = "distPlot",
               plotlyOutput("distPlot"), 
             ),

             
             div(
               class = "input2",
               sliderInput("Zakres","Wybierz rok:",c(2019,2023),
                             min = 2019,
                             max = 2023,
                             step=1)
             )
             )
    ),
    
    fluidRow(
        div(
          class="okno2",
    fluidRow(
        
      column(6, 
             div(
               class = "tabelka",
               textOutput("opis1"),
               HTML("<br>"),
               tableOutput("tabelka")
             )
             
      ),
      column(6, 
             
             div(
               class = "slupkowy",
               textOutput("opis2"),
               HTML("<br>"),
               plotlyOutput("slupkowy")
             )
      )
            
    ),
    
    fluidRow(
      column(12,
             div(
               class="okno3",
             div(
             class="mapka",
             HTML("<br><br>"),
             "Kraje w których były oglądane filmy:",
             plotOutput("mapka")
             )
             )
  ))
  
  ))
  ))

    
server <- function(input, output) {
    output$distPlot <- renderPlotly({
      
    plot_ly(data = exploring %>% filter(Profile.Name == input$Osoba 
                                        ,exploring$Year >= input$Zakres[1],
                                        exploring$Year <= input$Zakres[2]), 
            x = ~datetime, y=~NewDuration,
              type = "bar") %>%
        layout(yaxis=list(range=c(0,8000), title = "Czas oglądania w minutach"),
               xaxis = list(title = "Oś czasu"),
               title = paste0("Ile czasu ", input$Osoba, " spędzał/a na netflixie w latach ",
                              input$Zakres[1], "-", input$Zakres[2]))
    })
    
    output$tabelka <- renderTable({
          t <- exploring %>% filter(
                          exploring$Year >= input$Zakres[1],
                          exploring$Year <= input$Zakres[2]) %>%
          group_by(Profile.Name) %>% 
          count(Title) %>%
          filter(Profile.Name == input$Osoba) %>%
          arrange(desc(n)) %>%
          head(6) %>% select(Title)
          t
    })
    
    output$slupkowy <- renderPlotly({
      res <- exploring %>% filter(
        exploring$Year >= input$Zakres[1],
        exploring$Year <= input$Zakres[2],
        Profile.Name == input$Osoba)
      suma_filmow <- sum(res$licznik_filmow)
      suma_seriali <- sum(res$licznik_serialow)
      plot_ly(labels = c("Film", "Serial"), values = c(suma_filmow, suma_seriali), 
              type = "pie")
    })
    
    output$opis1 <- renderText({
    paste0("Sześć najpopularniejszych filmów i seriali dla użytkownika ",
           input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2]," to:")
    })
    
    output$opis2 <- renderText({
      paste0("Stosunek oglądania filmów do seriali dla ",
             input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2],":")
    })
    
    output$mapka <- renderPlot({
      country %>% 
        ggplot(aes(x = long, y = lat, group = group)) + 
        geom_polygon() +
        coord_fixed(1.3) +
        theme_bw()
    })
    
}

shinyApp(ui = ui, server = server)
