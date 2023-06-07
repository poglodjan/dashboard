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
  mutate(Country = ifelse(Country=="KR (Korea, Republic of)",
                               "KR (Korea)", Country)) %>%
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
        font-size:16px;
        width: 700px;
        margin-top:20px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      .tabelka{
        font-size:16px;
        width: 300px;
        margin: 0 auto;
        border-radius: 8px;
      }
      .tabelka2{
        font-size:16px;
        width: 200px;
        margin: 0 auto;
        border-radius: 8px;
      }
      .slupkowy{
        font-size:16px;
        color: black;
        width:400px;
        padding-right:20px;
        height: 490px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      
      .mapka{
        padding-top:50px;
        width: 640px;
        height: 450px;
        margin: 0 auto;
        margin-top:50px;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      
      .okno1{
      font-size:16px;
      margin: 0 auto;
      width:800px;
      padding-top: 20px;
      margin-bottom: 20px;
      height:540px;
      border: 8px solid white; 
      }
      
      .okno2{
      font-size:16px;
      margin: 0 auto;
      width: 1200px;
      margin-top: 20px;
      margin-bottom: 40px;
      height: 500px;
      border: 8px solid white; 
      }
      
      .okno3{
      font-size:16px;
      margin: 0 auto;
      width: 1200px;
      margin-top: 60px;
      margin-bottom: 20px;
      height: 600px;
      border: 8px solid white; 
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
               HTML("<br>"),
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
      div(class="okno3",
        fluidRow(
          
      column(4,
                    div(
                      class="tabelka2",
                      HTML("<br><br>"),
                      "Kraje w których były oglądane filmy:",
                      HTML("<br><br>"),
                      tableOutput("tabelka2")
                    )
             ),
      column(8,
             div(
               class="mapka",
               plotlyOutput("mapka")
             )
          )
      ),
      )
      )
  
  ))
  ))

    
server <- function(input, output) {
    output$distPlot <- renderPlotly({
      
    plot_ly(data = exploring %>% filter(Profile.Name == input$Osoba 
                                        ,exploring$Year >= input$Zakres[1],
                                        exploring$Year <= input$Zakres[2]), 
            x = ~datetime, y=~NewDuration,
              type = "bar",marker=list(color="red")) %>%
        layout(yaxis=list(range=c(0,8000), title = "Czas oglądania w minutach"),
               xaxis = list(title = "Oś czasu"),
               title = list(text = 
                              paste0("Ile czasu ", input$Osoba, " spędzał/a na netflixie w latach ",
                                          input$Zakres[1], "-", input$Zakres[2]), 
                            font = list(color = "white")),
               plot_bgcolor = "black",
               paper_bgcolor = "black")
    })
    
    output$tabelka <- renderTable({
          t <- exploring %>% filter(
                          exploring$Year >= input$Zakres[1],
                          exploring$Year <= input$Zakres[2]) %>%
          group_by(Profile.Name) %>% 
          count(Title) %>%
          filter(Profile.Name == input$Osoba) %>%
          arrange(desc(n)) %>%
          head(8) %>% select(Title)
          t
    })
    
    output$slupkowy <- renderPlotly({
      res <- exploring %>% filter(
        exploring$Year >= input$Zakres[1],
        exploring$Year <= input$Zakres[2],
        Profile.Name == input$Osoba)
      suma_filmow <- sum(res$licznik_filmow)/(sum(res$licznik_filmow)+sum(res$licznik_serialow))
      suma_seriali <- sum(res$licznik_serialow)/(sum(res$licznik_filmow)+sum(res$licznik_serialow))
      df <- data.frame(Film = (suma_filmow*100), Serial = (suma_seriali*100))
      
      pl <- plot_ly(data=df, x=~names(df), y=~unlist(df))
      pl %>% add_trace(type="bar",marker=list(color="red")) %>%
        layout(plot_bgcolor = "black", paper_bgcolor = "black", xaxis=list(title="Rodzaj"),
               yaxis=list(title="Liczba w procentach"))
    })
    
    output$opis1 <- renderText({
    paste0("Osiem najpopularniejszych filmów i seriali dla użytkownika ",
           input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2]," to:")
    })
    
    output$opis2 <- renderText({
      paste0("Stosunek oglądania filmów do seriali dla ",
             input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2],":")
    })
    
    output$mapka <- renderPlotly({
      kraje <- unique(exploring %>% filter(
         exploring$Year >= input$Zakres[1],
         exploring$Year <= input$Zakres[2],
         Profile.Name == input$Osoba) %>% 
         select(Country))
      kraje_fixed <- eval(parse(text = (gsub("[A-Z]{2} \\((.*?)\\)", "\\1", kraje))))

      country %>% 
        ggplot(aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill=ifelse(country$region %in% kraje_fixed, "red","black"), 
                     color="white", size="0.1") +
        coord_fixed(1.3) +
        theme_void() 
    })
    
    output$tabelka2 <- renderTable({
      kraje <- unique(exploring %>% filter(
        exploring$Year >= input$Zakres[1],
        exploring$Year <= input$Zakres[2],
        Profile.Name == input$Osoba) %>% 
          select(Country))
      kraje_fixed <- eval(parse(text = (gsub("[A-Z]{2} \\((.*?)\\)", "\\1", kraje))))
      kraje_fixed
    })
}

shinyApp(ui = ui, server = server)


