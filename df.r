library(dplyr)
library(ggplot2)

df_netflix <- read.csv('netflix_titles.csv')
df_amazon <- read.csv('amazon_prime_titles.csv')
df_disney <- read.csv('disney_plus_titles.csv')
df_hbo <- read.csv('hbo_max_titles.csv')

df_netflix$vod <- 'Netflix'
df_amazon$vod <- 'Amazon Prime'
df_disney$vod <- 'Disney+'
df_hbo$vod <- 'HBO Max'

df <- bind_rows(df_netflix, df_amazon, df_disney, df_hbo)

X <- gsub("'", "", df[,8])
Y <- gsub("\\[|\\]", "", X)
df$genres <- Y

x <- gsub("'", "", df[,9])
y <- gsub("\\[|\\]", "", x)
df$production_countries <- y

# dorzucamy ocenę
df <- bind_rows(df_netflix, df_amazon, df_disney, df_hbo)
df$genres <- Y
df$production_countries <- y
df <- df %>% 
  select(title, type, year = release_year, countries = production_countries, seasons, genres,
         imdb_score, imdb_votes, tmdb_score, tmdb_popularity, vod) %>% 
  arrange(-imdb_votes)

df <- df %>% mutate(imdb_pom = case_when(imdb_votes > 200 ~ imdb_score,
                                         TRUE ~ NA),
                    tmdb_pom = case_when(tmdb_popularity > 2.5 ~ tmdb_score,
                                         TRUE ~ NA),
                    dif = imdb_score - tmdb_score,
                    dif1 = imdb_pom - tmdb_pom,
                    rating = case_when(is.na(imdb_pom) & is.na(tmdb_pom) ~ NA,
                                       is.na(imdb_pom) & tmdb_popularity < 12 & tmdb_pom > 9.4 ~ NA,
                                       is.na(imdb_pom) & tmdb_pom > 9.4 ~ tmdb_pom + dif * 1/2,
                                       is.na(imdb_pom) ~ tmdb_pom,
                                       is.na(tmdb_pom) ~ imdb_pom,
                                       TRUE ~ (tmdb_pom + imdb_pom)/2)) %>% 
  select(title, type, year, countries, seasons, genres, rating, vod)
#sum(is.na(df$rating))

df <- df %>% 
  mutate(type = case_when(type == "SHOW" ~ "Serial",
                          TRUE ~ "Film",))

genre_mapping <- c(
  "documentation" = "Dokument",
  "drama" = "Dramat",
  "sport" = "Sport",
  "romance" = "Romans",
  "comedy" = "Komedia",
  "crime" = "Kryminał",
  "music" = "Muzyczny",
  "fantasy" = "Fantasy",
  "european" = "Europejski",
  "thriller" = "Thriller",
  "action" = "Akcja",
  "history" = "Historyczny",     
  "family" = "Familijny",
  "war" = "Wojenny",
  "animation" = "Animowany",
  "scifi" = "Science fiction",
  "reality" = "Reality",
  "western" = "Western",
  "horror" = "Horror"
)

country_mapping <- c(
  "US" = "Stany Zjednoczone", "GB" = "Wielka Brytania", "EG" = "Egipt", "IN" = "Indie", "DE" = "Niemcy",
  "SU" = "Związek Radziecki", "DZ" = "Algieria", "CA" = "Kanada", "FR" = "Francja",
  "LB" = "Liban",
  "JP" = "Japonia",
  "AR" = "Argentyna",
  "IE" = "Irlandia",
  "AU" = "Australia",
  "ET" = "Etiopia",
  "GH" = "Ghana",
  "BF" = "Burkina Faso",
  "HK" = "Hongkong",
  "MX" = "Meksyk",
  "CN" = "Chiny",
  "ES" = "Hiszpania",
  "PS" = "Palestyna",
  "CO" = "Kolumbia",
  "BE" = "Belgia",
  "NO" = "Norwegia",
  "IT" = "Włochy",
  "TR" = "Turcja",
  "NZ" = "Nowa Zelandia",
  "DK" = "Dania",
  "HU" = "Węgry",
  "CZ" = "Czechy",
  "TW" = "Tajwan",
  "KR" = "Korea Południowa",
  "RU" = "Rosja",
  "NG" = "Nigeria",
  "CH" = "Szwajcaria",
  "MY" = "Malezja",
  "KW" = "Kuwejt",
  "PH" = "Filipiny",
  "ZA" = "Republika Południowej Afryki",
  "RW" = "Rwanda",
  "MA" = "Maroko",
  "AT" = "Austria",
  "SE" = "Szwecja",
  "NL" = "Holandia",
  "SG" = "Singapur",
  "KE" = "Kenia",
  "CL" = "Chile",
  "SA" = "Arabia Saudyjska",
  "BR" = "Brazylia",
  "ID" = "Indonezja",
  "IS" = "Islandia",
  "AE" = "Zjednoczone Emiraty Arabskie",
  "IL" = "Izrael",
  "PL" = "Polska",
  "FI" = "Finlandia",
  "CD" = "Demokratyczna Republika Konga",
  "RO" = "Rumunia",
  "UA" = "Ukraina",
  "BG" = "Bułgaria",
  "QA" = "Katar",
  "IR" = "Iran",
  "JO" = "Jordania",
  "SY" = "Syria",
  "GL" = "Grenlandia",
  "VE" = "Wenezuela",
  "BY" = "Białoruś",
  "VN" = "Wietnam",
  "TN" = "Tunezja",
  "TH" = "Tajlandia",
  "GE" = "Gruzja",
  "IQ" = "Irak",
  "KH" = "Kambodża",
  "AL" = "Albania",
  "CU" = "Kuba",
  "PR" = "Portoryko",
  "RS" = "Serbia",
  "UY" = "Urugwaj",
  "PE" = "Peru",
  "LU" = "Luksemburg",
  "PY" = "Paragwaj",
  "PK" = "Pakistan",
  "VA" = "Watykan",
  "GR" = "Grecja",
  "NP" = "Nepal",
  "BD" = "Bangladesz",
  "TZ" = "Tanzania",
  "CM" = "Kamerun",
  "KG" = "Kirgistan",
  "MC" = "Monako",
  "SN" = "Senegal",
  "BT" = "Bhutan",
  "LK" = "Sri Lanka",
  "CY" = "Cypr",
  "PT" = "Portugalia",
  "AO" = "Angola",
  "ZW" = "Zimbabwe",
  "MW" = "Malawi",
  "GT" = "Gwatemala",
  "MU" = "Mauritius",
  "IO" = "Brytyjskie Terytorium Oceanu Indyjskiego",
  "AF" = "Afganistan",
  "KN" = "Saint Kitts i Nevis",
  "EE" = "Estonia",
  "DO" = "Dominikana",
  "PA" = "Panama",
  "FO" = "Wyspy Owcze",
  "YU" = "Jugosławia",
  "XC" = "Czechosłowacja",
  "LI" = "Liechtenstein",
  "CI" = "Wybrzeże Kości Słoniowej",
  "AN" = "Antyle Holenderskie",
  "SK" = "Słowacja",
  "SZ" = "Eswatini",
  "JM" = "Jamajka",
  "BO" = "Boliwia",
  "LT" = "Litwa",
  "EC" = "Ekwador",
  "KZ" = "Kazachstan",
  "MT" = "Malta",
  "CF" = "Republika Środkowoafrykańska",
  "GD" = "Grenada",
  "SO" = "Somalia",
  "TT" = "Trynidad i Tobago",
  "XK" = "Kosowo",
  "CR" = "Kostaryka",
  "LV" = "Łotwa",
  "SV" = "Salwador",
  "TC" = "Turks i Caicos",
  "MN" = "Mongolia",
  "NI" = "Nikaragua",
  "SB" = "Wyspy Salomona",
  "VU" = "Vanuatu",
  "AQ" = "Antarktyda",
  "FM" = "Mikronezja",
  "UZ" = "Uzbekistan",
  "NA" = "Namibia",
  "AZ" = "Azerbejdżan",
  "BM" = "Bermudy",
  "SI" = "Słowenia",
  "BA" = "Bośnia i Hercegowina",
  "HR" = "Chorwacja",
  "PF" = "Polinezja Francuska",
  "FJ" = "Fidżi",
  "HN" = "Honduras",
  "NC" = "Nowa Kaledonia",
  "OM" = "Oman",
  "MK" = "Macedonia Północna",
  "UG" = "Uganda",
  "KI" = "Kiribati",
  "BW" = "Botswana",
  "BS" = "Bahamy"
)

df <- df %>%
  mutate(
    genres_split = strsplit(genres, ",\\s*"),
    countries_split = strsplit(countries, ",\\s*")) %>%
  mutate(
    genres_pl = sapply(genres_split, function(genres) {
      translated_genres <- sapply(genres, function(genre) {
        if (genre %in% names(genre_mapping))
          genre_mapping[[genre]]
        else
          genre
      })
      paste(translated_genres, collapse = ", ")
    }),
    countries_pl = sapply(countries_split, function(countries) {
      translated_countries <- sapply(countries, function(country) {
        if (country %in% names(country_mapping))
          country_mapping[[country]]
        else
          country
      })
      paste(translated_countries, collapse = ", ")
    })
  )

df <- df %>% 
  mutate(genres = genres_pl, countries = countries_pl) %>% 
  select(title, type, year, countries, seasons, genres, rating, vod)


############### koniec kodu do projektu ###############

