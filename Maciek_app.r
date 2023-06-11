library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(stringi)
library(stringr)
library(ggrepel)
library(shinycssloaders)
library(data.table)

# Pobieranie ramek danych
data_disney_actors <- read.csv("credits.csv")
data_disney <- read.csv("disney_plus_titles.csv")
data_amazon_actors <- read.csv("credits1.csv")
data_amazon <- read.csv("amazon_prime_titles.csv")
data_HBO_actors <- read.csv("credits2.csv")
data_HBO <- read.csv("hbo_max_titles.csv")
data_Netflix_actors <- read.csv("credits3.csv")
data_Netflix <- read.csv("netflix_titles.csv")

data_VOD <- rbind(data_amazon, data_disney, data_HBO, data_Netflix)
# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_net <- subset(data_Netflix, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_net <- documentary_net %>% mutate(z = "Dokument")
drama_net <- subset(data_Netflix, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_net <- drama_net %>% mutate(z = "Dramat")
sport_net <- subset(data_Netflix, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_net <- sport_net %>% mutate(z = "Sport")
romance_net <- subset(data_Netflix, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_net <- romance_net %>% mutate(z = "Romans")
comedy_net <- subset(data_Netflix, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_net <- comedy_net %>% mutate(z = "Komedia")
crime_net <- subset(data_Netflix, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_net <- crime_net %>% mutate(z = "Kryminał")
music_net <- subset(data_Netflix, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_net <- music_net %>% mutate(z = "Muzyczny")
fantasy_net <- subset(data_Netflix, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_net <- fantasy_net %>% mutate(z = "Fantasy")
thriller_net <- subset(data_Netflix, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_net <- thriller_net %>% mutate(z = "Thriller")
action_net <- subset(data_Netflix, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_net <- action_net %>% mutate(z = "Akcja")
history_net <- subset(data_Netflix, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_net <- history_net %>% mutate(z = "Historyczny")
family_net <- subset(data_Netflix, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_net <- family_net %>% mutate(z = "Familijny")
war_net <- subset(data_Netflix, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_net <- war_net %>% mutate(z = "Wojenny")
european_net <- subset(data_Netflix, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_net <- european_net %>% mutate(z = "Europejski")
animation_net <- subset(data_Netflix, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_net <- animation_net %>% mutate(z = "Animowany")
scifi_net <- subset(data_Netflix, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_net <- scifi_net %>% mutate(z = "Sci-fi")
reality_net <- subset(data_Netflix, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_net <- reality_net %>% mutate(z = "Reality show")
western_net <- subset(data_Netflix, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_net <- western_net %>% mutate(z = "Western")
horror_net <- subset(data_Netflix, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_net <- horror_net %>% mutate(z = "Horror")

movie_genres_net <- rbind(documentary_net, drama_net, sport_net, romance_net, 
                          comedy_net, crime_net, music_net,fantasy_net, thriller_net,
                          action_net, history_net, family_net, war_net, european_net, 
                          animation_net, scifi_net, reality_net, western_net, horror_net)

movie_genres_net <- movie_genres_net %>% mutate(VOD = "Netflix")


# Tworzenie ramki z gatunkami filmowymi dla HBO
documentary_HBO <- subset(data_HBO, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_HBO <- documentary_HBO %>% mutate(z = "Dokument")
drama_HBO <- subset(data_HBO, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_HBO <- drama_HBO %>% mutate(z = "Dramat")
sport_HBO <- subset(data_HBO, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_HBO <- sport_HBO %>% mutate(z = "Sport")
romance_HBO <- subset(data_HBO, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_HBO <- romance_HBO %>% mutate(z = "Romans")
comedy_HBO <- subset(data_HBO, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_HBO <- comedy_HBO %>% mutate(z = "Komedia")
crime_HBO <- subset(data_HBO, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_HBO <- crime_HBO %>% mutate(z = "Kryminał")
music_HBO <- subset(data_HBO, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_HBO <- music_HBO %>% mutate(z = "Muzyczny")
fantasy_HBO <- subset(data_HBO, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_HBO <- fantasy_HBO %>% mutate(z = "Fantasy")
thriller_HBO <- subset(data_HBO, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_HBO <- thriller_HBO %>% mutate(z = "Thriller")
action_HBO <- subset(data_HBO, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_HBO <- action_HBO %>% mutate(z = "Akcja")
history_HBO <- subset(data_HBO, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_HBO <- history_HBO %>% mutate(z = "Historyczny")
family_HBO <- subset(data_HBO, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_HBO <- family_HBO %>% mutate(z = "Familijny")
war_HBO <- subset(data_HBO, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_HBO <- war_HBO %>% mutate(z = "Wojenny")
european_HBO <- subset(data_HBO, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_HBO <- european_HBO %>% mutate(z = "Europejski")
animation_HBO <- subset(data_HBO, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_HBO <- animation_HBO %>% mutate(z = "Animowany")
scifi_net <- subset(data_HBO, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_HBO <- scifi_net %>% mutate(z = "Sci-fi")
reality_HBO <- subset(data_HBO, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_HBO <- reality_HBO %>% mutate(z = "Reality show")
western_HBO <- subset(data_HBO, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_HBO <- western_HBO %>% mutate(z = "Western")
horror_HBO <- subset(data_HBO, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_HBO <- horror_HBO %>% mutate(z = "Horror")

movie_genres_HBO <- rbind(documentary_HBO, drama_HBO, sport_HBO, romance_HBO, 
                          comedy_HBO, crime_HBO, music_HBO,fantasy_HBO, thriller_HBO,
                          action_HBO, history_HBO, family_HBO, war_HBO, european_HBO, 
                          animation_HBO, scifi_HBO, reality_HBO, western_HBO, horror_HBO)

movie_genres_HBO <- movie_genres_HBO %>% mutate(VOD = "HBO MAX")

# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_dis <- subset(data_disney, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_dis <- documentary_dis %>% mutate(z = "Dokument")
drama_dis <- subset(data_disney, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_dis <- drama_dis %>% mutate(z = "Dramat")
sport_dis <- subset(data_disney, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_dis <- sport_dis %>% mutate(z = "Sport")
romance_dis <- subset(data_disney, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_dis <- romance_dis %>% mutate(z = "Romans")
comedy_dis <- subset(data_disney, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_dis <- comedy_dis %>% mutate(z = "Komedia")
crime_dis <- subset(data_disney, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_dis <- crime_dis %>% mutate(z = "Kryminał")
music_dis <- subset(data_disney, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_dis <- music_dis %>% mutate(z = "Muzyczny")
fantasy_dis <- subset(data_disney, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_dis <- fantasy_dis %>% mutate(z = "Fantasy")
thriller_dis <- subset(data_disney, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_dis <- thriller_dis %>% mutate(z = "Thriller")
action_dis <- subset(data_disney, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_dis <- action_dis %>% mutate(z = "Akcja")
history_dis <- subset(data_disney, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_dis <- history_dis %>% mutate(z = "Historyczny")
family_dis <- subset(data_disney, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_dis <- family_dis %>% mutate(z = "Familijny")
war_dis <- subset(data_disney, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_dis <- war_dis %>% mutate(z = "Wojenny")
european_dis <- subset(data_disney, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_dis <- european_dis %>% mutate(z = "Europejski")
animation_dis <- subset(data_disney, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_dis <- animation_dis %>% mutate(z = "Animowany")
scifi_dis <- subset(data_disney, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_dis <- scifi_dis %>% mutate(z = "Sci-fi")
reality_dis <- subset(data_disney, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_dis <- reality_dis %>% mutate(z = "Reality show")
western_dis <- subset(data_disney, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_dis <- western_dis %>% mutate(z = "Western")
horror_dis <- subset(data_disney, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_dis <- horror_dis %>% mutate(z = "Horror")

movie_genres_dis <- rbind(documentary_dis, drama_dis, sport_dis, romance_dis, 
                          comedy_dis, crime_dis, music_dis,fantasy_dis, thriller_dis,
                          action_dis, history_dis, family_dis, war_dis, european_dis, 
                          animation_dis, scifi_dis, reality_dis, western_dis, horror_dis)

movie_genres_dis <- movie_genres_dis %>% mutate(VOD = "Disney+")

# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_ama <- subset(data_amazon, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_ama <- documentary_ama %>% mutate(z = "Dokument")
drama_ama <- subset(data_amazon, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_ama <- drama_ama %>% mutate(z = "Dramat")
sport_ama <- subset(data_amazon, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_ama <- sport_ama %>% mutate(z = "Sport")
romance_ama <- subset(data_amazon, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_ama <- romance_ama %>% mutate(z = "Romans")
comedy_ama <- subset(data_amazon, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_ama <- comedy_ama %>% mutate(z = "Komedia")
crime_ama <- subset(data_amazon, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_ama <- crime_ama %>% mutate(z = "Kryminał")
music_ama <- subset(data_amazon, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_ama <- music_ama %>% mutate(z = "Muzyczny")
fantasy_ama <- subset(data_amazon, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_ama <- fantasy_ama %>% mutate(z = "Fantasy")
thriller_ama <- subset(data_amazon, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_ama <- thriller_ama %>% mutate(z = "Thriller")
action_ama <- subset(data_amazon, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_ama <- action_ama %>% mutate(z = "Akcja")
history_ama <- subset(data_amazon, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_ama <- history_ama %>% mutate(z = "Historyczny")
family_ama <- subset(data_amazon, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_ama <- family_ama %>% mutate(z = "Familijny")
war_ama <- subset(data_amazon, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_ama <- war_ama %>% mutate(z = "Wojenny")
european_ama <- subset(data_amazon, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_ama <- european_ama %>% mutate(z = "Europejski")
animation_ama <- subset(data_amazon, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_ama <- animation_ama %>% mutate(z = "Animowany")
scifi_ama <- subset(data_amazon, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_ama <- scifi_ama %>% mutate(z = "Sci-fi")
reality_ama <- subset(data_amazon, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_ama <- reality_ama %>% mutate(z = "Reality show")
western_ama <- subset(data_amazon, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_ama <- western_ama %>% mutate(z = "Western")
horror_ama <- subset(data_amazon, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_ama <- horror_ama %>% mutate(z = "Horror")

movie_genres_ama <- rbind(documentary_ama, drama_ama, sport_ama, romance_ama, 
                          comedy_ama, crime_ama, music_ama,fantasy_ama, thriller_ama,
                          action_ama, history_ama, family_ama, war_ama, european_ama, 
                          animation_ama, scifi_ama, reality_ama, western_ama, horror_ama)

movie_genres_ama <- movie_genres_ama %>% mutate(VOD = "Amazon Prime Video")

# Tworzymy całą ramkę danych

movie_genres <- rbind(movie_genres_ama, movie_genres_net, movie_genres_HBO, movie_genres_dis)
movie_data_actors <- rbind(data_Netflix_actors, data_amazon_actors, data_disney_actors, data_HBO_actors)
movie_data_actors <- filter(movie_data_actors, role == "ACTOR")
dt <- inner_join(movie_data_actors, movie_genres, by = "id", multiple = "all")
  
# Tworzymy wykres
#dt <- dt %>%  distinct() %>% group_by(z) %>% summarise(Liczba_filmow = n()) 
 
#  ggplot(dt,  aes(x = z, y = Liczba_filmow, fill = z)) +
#  geom_col(width = 0.5, color = "black", alpha = 0.8) +
#  labs(x = "Gatunek", y = "Liczba filmów", title = "Wykres kolumnowy") +
#  theme_minimal() +
#  theme(
#    legend.position = "none",
#    plot.title = element_text(size = 14, face = "bold"),
#    axis.text = element_text(size = 12, angle = 90, hjust = 1),
#   axis.title = element_text(size = 12, face = "bold"))



## Tabelka aktorów w filmach z najwyższymi ocenami

best_actors <- inner_join(movie_data_actors, data_VOD,by = "id", multiple = "all")
best_actors <- best_actors %>%  
  distinct() %>% 
  filter(imdb_votes > 1000) %>%
  mutate(score = (imdb_score + tmdb_score)/2) 
best <-best_actors %>%  group_by(name) %>% 
  summarise(Filmy = n()) %>% filter( Filmy >= 20)
aktorzy <- inner_join(best, dt,by = "name", multiple = "all") %>% select(name, z)

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
      .tytul {
        margin-top: 20px;
        font-family: 'Netflix Sans', Arial, sans-serif;
        text-align: center;
        font-size: 24px;
        height: 100px;
      }")),
  
  fluidRow(
    div(
      class = "tytul",
    column(12,
           "Analiza występowania aktorów w filmach określonych gatunków z podziałem na portale streamingowe"
           )
    )
  ),
  
  fluidRow(
    column(9,
           plotlyOutput("barPlot")

    ),
    column(
      width = 3,
      selectInput("ktore_VOD",
                  "Platforma streamingowa:",
                  choices = c("Netflix", "HBO MAX", "Amazon Prime Video", "Disney+"))

    )
  ),
  
  fluidRow(
    column(3,
           tableOutput("tabela_aktorow")
           )
    ,

    column(
      width = 3,
      selectInput(inputId = "input_text1", label = "Wprowadź imię i nazwisko aktora:",
                  choices=unique(aktorzy$name)),
      textOutput("ocenaAktora"))

    ,
    column(6,
           plotlyOutput("barplotaktor")
    )

  )
)



server <- function(input, output) {
  output$barPlot <- renderPlotly({
    gatunki <- dt
    plot_ly(gatunki %>%
              filter(gatunki$VOD == input$ktore_VOD) %>%
              distinct() %>%
              group_by(z) %>%
              summarise(Liczba_filmow = n()),
            x = ~z,
            y = ~Liczba_filmow) %>%
      add_trace(type="bar",marker=list(color="red")) %>%
      layout(title = "Popularność gatunków ze względu na platformę", font=list(color="white"),
             xaxis = list(title = 'Gatunek', tickangle = 45),
             yaxis = list(title = 'Liczba filmów'),
                          plot_bgcolor = "black",
                          paper_bgcolor = "black")



  })
  
  
  
  output$tabela_aktorow <- renderTable({
    
    
    
    
    s1 <-best_actors %>%  group_by(name) %>% 
      summarise(Filmy = n())
    
    s2 <- best_actors %>%  group_by(name) %>% 
      summarise(Ocena = mean(score))
    
    score <- inner_join(s1, s2, by = "name")
    score <- filter(score, Filmy >= 5) %>% arrange(desc(Ocena))
    colnames(score)[colnames(score) == "name"] <- "Aktor" 
    
    head(score, 10)
  })

  output$barplotaktor <- renderPlotly({
    gatunki_aktor <- aktorzy
    plot_ly(gatunki_aktor %>%
              filter(gatunki_aktor$name == input$input_text1) %>%
              group_by(z) %>%
              summarise(Liczba_filmow = n()),
            x = ~z,
            y = ~Liczba_filmow) %>%
      add_trace(type = "scatter", mode = "lines+markers", marker = list(color = "red"), line = list(color = "red")) %>%
      layout(title = "Liczba wystąpień aktora w filmie ze względu na gatunek",
             font = list(color = "white"),
             xaxis = list(title = 'Gatunek', tickangle = 45),
             yaxis = list(title = 'Liczba filmów'),
             plot_bgcolor = "black",
             paper_bgcolor = "black")

    })

  output$ocenaAktora <- renderText({

    paste("Średnia ocena filmów w których","\n", "wystepował ", input$input_text1, "to ")
  })

  
}


shinyApp(ui, server)




