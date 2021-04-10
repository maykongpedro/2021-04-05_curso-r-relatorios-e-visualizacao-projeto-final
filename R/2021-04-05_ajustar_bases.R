
# carregar pipe
'%>%' <- magrittr::`%>%`

# criar diretório básico de projeto
# fs::dir_create("./dados")
# fs::dir_create("./R")
# fs::dir_create("./outputs")
# fs::dir_create("./docs")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either "yyyy-mm-dd" date or year/week works!
#tuesdata <- tidytuesdayR::tt_load('2021-03-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

raw_bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies

# criando base de legenda para a classificação
legend_rating <- 
  tibble::tribble(
    ~ rating,
    ~ description,
    ~ descricao,
    0L,
    "unscored",
    "Não testado",
    1L,
    "It has to have at least two [named] women in it",
    "Tem pelo menos duas mulheres nomeadas",
    2L,
    "Who talk to each other",
    "As mulheres falam entre si",
    3L,
    "About something besides a man",
    "As mulheres falam entre si sobre algo que não seja um homem"
  )

# explorar a base de filmes e de teste
dplyr::glimpse(movies)
dplyr::glimpse(raw_bechdel)
dplyr::glimpse(legend_rating)


movies %>% 
  View()

# juntar legenda de classificação
raw_bechdel_com_legenda <-
  raw_bechdel %>% 
  dplyr::left_join(legend_rating, by = "rating")

# selecionar colunas para join
raw_bechdel_select <-
  raw_bechdel_com_legenda %>% 
  dplyr::select(-c("year", "id", "title"))

# juntar bases
movies_rating <- 
  movies %>% 
  dplyr::left_join(raw_bechdel_select, by = "imdb_id") %>% 
  dplyr::relocate(c("rating", "description", "descricao"), .after = "binary")


# verficando resultado final
dplyr::glimpse(movies_rating)

# exportar bases em formato .rds
readr::write_rds(movies, "./dados/movies.rds")
readr::write_rds(raw_bechdel, "./dados/raw_bechdel.rds")
readr::write_rds(legend_rating, "./dados/legend_rating.rds")
readr::write_rds(movies_rating, "./R/2021-04-09_movies_rating_bechdel_test.rds")

