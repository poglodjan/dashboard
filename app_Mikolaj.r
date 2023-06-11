library(shiny)
library(ggplot2)
library(plotly)

# Funkcja sortująca kraje
calculate_country_frequency <- function(data) {
  country_counts <- table(unlist(strsplit(as.character(data$countries), ", ")))
  country_counts <- country_counts[order(-country_counts)]
  country_counts
}


# UI
ui <- fluidPage(
  
  # style
  
  tags$style(HTML("
    .bottom-space {
      padding-bottom: 50px; /* Adjust the value to increase or decrease the space */
    }
  ")),
  
  titlePanel("Porównywarka serwisów streamingowych"),
  
  # Side panel z filtrowaniem
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Filtr typu (film/serial)
      
      selectInput(
        "type_filter",
        "Typ:",
        choices = c("Wszystko", "Film", "Serial"),
        selected = "Wszystko"
      ),
      
      # Filtr gatunków
      
      selectInput(
        "genres_filter",
        "Gatunki:",
        choices = c("", unique(unlist(strsplit(as.character(df$genres), ", ")))),
        multiple = TRUE
      ),
      
      # Filtr kraju produkcji
      
      selectInput(
        "countries_filter",
        "Kraj produkcji:",
        choices = c("", names(calculate_country_frequency(df))),
        multiple = TRUE
      ),
      
      # Filtr ocen

      numericInput(
        "rating_filter",
        "Minimalna ocena:",
        value = "",
        step = 0.1
      ),
      
      # Filtr roku wydania
      
      sliderInput(
        "year_filter",
        "Rok wydania:",
        min = min(df$year, na.rm = TRUE),
        max = max(df$year, na.rm = TRUE),
        value = c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)),
        step = 1,
        sep = "",
        ticks = FALSE
      ),
    
      # Filtr liczby sezonów
      conditionalPanel(
        condition = "input.type_filter == 'Serial'",
        sliderInput(
          "seasons_filter",
          "Liczba sezonów:",
          min = 1,
          max = max(df$seasons, na.rm = TRUE),
          value = c(min(df$seasons, na.rm = TRUE), max(df$seasons, na.rm = TRUE)),
          step = 1,
          ticks = FALSE
        )
      )

    ),
   
    # Główny panel z wskaźnikami i histogramem
    mainPanel(
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Netflix"),
            "Ocena:",
            p(textOutput("netflix_average_rating")),
            "Liczba tytułów:",
            p(textOutput("netflix_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("netflix_ratio"))
          )
        ),
        column(
          width = 3,
          wellPanel(
            h4("Disney+"),
            "Ocena:",
            p(textOutput("disney_average_rating")),
            "Liczba tytułów:",
            p(textOutput("disney_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("disney_ratio"))
          )
        ),
        column(
          width = 3,
          wellPanel(
            h4("HBO Max"),
            ("Ocena:"),
            p(textOutput("hbo_average_rating")),
            "Liczba tytułów:",
            p(textOutput("hbo_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("hbo_ratio"))
          )
        ),
        column(
          width = 3,
          wellPanel(
            h4("Amazon Prime"),
            "Ocena:",
            p(textOutput("prime_average_rating")),
            "Liczba tytułów:",
            p(textOutput("prime_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("prime_ratio"))
          )
        ),
        
        # tu jest histogram
        
        column(
          width = 9,
          plotOutput("histogram")
        ),
        column(
          width = 3,
          selectInput(
            "vod_filter",
            "VOD:",
            choices = c("Wszystkie", "Netflix", "Disney+", "HBO Max", "Amazon Prime"),
            selected = "Wszystkie"
          ),
          selectInput(
            "hist_variable",
            "Zmienna histogramu:",
            choices = c("Ocena", "Rok wydania"),
            selected = "Ocena"
          ),
        ),
        
        # tu jest tabela
        
        column(
          class = "bottom-space",
          width = 12,
          DT::dataTableOutput("filtered_table")
        )
      )
    )
  )
)


# Definicja logiki serwera
server <- function(input, output, session) {
  
  # Wskaźniki dla poszczególnych serwisów streamingowych
  
  # netflix
  output$netflix_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Netflix"], na.rm = TRUE), 1))
  })
  
  output$netflix_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Netflix", ])
  })
  
  output$netflix_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Netflix", ])
    netflix_titles <- nrow(filtered[filtered$vod == "Netflix", ])
    paste(sprintf("%.1f", round(netflix_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # disney
  output$disney_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Disney+"], na.rm = TRUE), 1))
  })
  
  output$disney_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Disney+", ])
  })
  
  output$disney_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Disney+", ])
    disney_titles <- nrow(filtered[filtered$vod == "Disney+", ])
    paste(sprintf("%.1f", round(disney_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # hbo
  output$hbo_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "HBO Max"], na.rm = TRUE), 1))
  })
  
  output$hbo_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "HBO Max", ])
  })
  
  output$hbo_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "HBO Max", ])
    hbo_titles <- nrow(filtered[filtered$vod == "HBO Max", ])
    paste(sprintf("%.1f", round(hbo_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # prime
  output$prime_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Amazon Prime"], na.rm = TRUE), 1))
  })
  
  output$prime_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Amazon Prime", ])
  })
  
  output$prime_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Amazon Prime", ])
    prime_titles <- nrow(filtered[filtered$vod == "Amazon Prime", ])
    paste(sprintf("%.1f", round(prime_titles/total_titles, 3) * 100), "%", sep = "")
  })

  # pojawiamy opcję Sezon przy wyborze Serial
  
  observeEvent(input$type_filter, {
    if (input$type_filter == "Serial") {
      updateSelectInput(
        session,
        "hist_variable",
        choices = c("Ocena", "Rok wydania", "Liczba sezonów"),
        selected = "Ocena"
      )
    }
  })
   
  # Histogram
  output$histogram <- renderPlot({
    filtered <- filter_histogram_data()
    filtered <- filtered[!is.na(filtered$rating) & is.finite(filtered$rating), ]
    
    hist_variable <- switch(
      input$hist_variable,
      "Ocena" = filtered$rating,
      "Rok wydania" = filtered$year,
      "Liczba sezonów" = filtered$seasons
    )
    
    p <- ggplot(filtered, aes(x = hist_variable)) +
      geom_histogram(bins = 30)
    p
  })
  
  # Tabela
  
  output$filtered_table <- DT::renderDataTable(DT::datatable({
    filter_data() %>% 
      select('Tytuł oryginalny' = title, Typ = type, 'Rok wydania' = year, 'Kraje produkcji' = countries,
             Sezony = seasons, Gatunki = genres, Ocena = rating, Serwis = vod) %>% 
      mutate(Ocena = ifelse(Ocena=="","",sprintf("%.2f", round(Ocena, 2))))
  }))
  
  # Filtruj dane
  filter_data <- reactive({
    filtered <- df
    
    if (!is.null(input$genres_filter) && length(input$genres_filter) > 0) {
      filtered <- filtered[sapply(filtered$genres, function(x) any(input$genres_filter %in% strsplit(as.character(x), ", ")[[1]])), ]
    }
    
    if (!is.null(input$type_filter) && input$type_filter != "" && input$type_filter != "Wszystko") {
      filtered <- filtered[filtered$type == input$type_filter, ]
    }
    
    if (!is.null(input$seasons_filter)) {
      if (!is.na(input$seasons_filter[1]) && !is.na(input$seasons_filter[2]) && input$seasons_filter[1] <= input$seasons_filter[2]) {
        filtered <- filtered[(!is.na(filtered$seasons) & filtered$seasons >= input$seasons_filter[1] & filtered$seasons <= input$seasons_filter[2]) | is.na(filtered$seasons), ]
      }
    }
    
    
    if (!is.null(input$year_filter) && input$year_filter[1] <= input$year_filter[2]) {
      filtered <- filtered[filtered$year >= input$year_filter[1] & filtered$year <= input$year_filter[2], ]
    }
    
    if (!is.null(input$countries_filter) && length(input$countries_filter) > 0) {
      filtered <- filtered[sapply(filtered$countries, function(x) any(input$countries_filter %in% strsplit(as.character(x), ", ")[[1]])), ]
    }
    
    if (!is.null(input$rating_filter) && input$rating_filter != "" && !is.na(as.numeric(input$rating_filter))) {
      filtered <- filtered[!is.na(filtered$rating) & filtered$rating >= as.numeric(input$rating_filter), ]
    }
    
    filtered
  })

  
  # Filtruj dane dla histogramu
  filter_histogram_data <- reactive({
    filtered <- filter_data()
    
    if (!is.null(input$vod_filter) && input$vod_filter != "" && input$vod_filter != "Wszystkie") {
      filtered <- filtered[filtered$vod == input$vod_filter, ]
    }
    
    filtered
  })
  
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)
