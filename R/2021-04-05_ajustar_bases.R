
# carregar pipe
'%>%' <- magrittr::`%>%`

# criar diretório básico de projeto
# fs::dir_create("./dados")
# fs::dir_create("./R")
# fs::dir_create("./outputs")
# fs::dir_create("./docs")


# Obter os dados ----------------------------------------------------------

# Get the Data
# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either "yyyy-mm-dd" date or year/week works!
#tuesdata <- tidytuesdayR::tt_load('2021-03-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

raw_bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies


# Transformação -----------------------------------------------------------

# criando base de legenda para a classificação
legend_rating <- 
  tibble::tribble(
    ~ rating,
    ~ description,
    ~ descricao,
    0L,
    "unscored",
    "Sem pontuação",
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


# movies %>% 
#   View()

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


# Validação ---------------------------------------------------------------

# verficando resultado final
dplyr::glimpse(movies_rating)

# verificar itens
unique(movies_rating$rating)

# tem uma linha que não apresenta rank númerico
# porém ela apresenta um resultado de "pass"no teste
movies_rating %>% 
  dplyr::filter(is.na(rating)) %>% 
  View()

# # corrigir essa linha
# movies_rating$rating <-
#   movies_rating$rating %>%
#   tidyr::replace_na(replace = 3)
# 
# movies_rating$description <-
#   movies_rating$description %>%
#   tidyr::replace_na(replace = "About something besides a man")
# 
# movies_rating$descricao <-
#   movies_rating$descricao %>%
#   tidyr::replace_na(replace = "As mulheres falam entre si sobre algo que não seja um homem")

# verificar testes
movies_rating %>% 
  dplyr::group_by(rating, binary, clean_test) %>% 
  dplyr::summarise(n = dplyr::n())

# é preciso corrigir a coluna "rating", pois somente o que foi rankeado com 3 é que passa no teste
# contudo, considerando que a base "movies" possui mais colunas referentes à classificação, vou optar
# por considerar ela como a correta e mudar a pontuação de acordo com o que é colocado na base "movies"

# ```` para o que é "dubios" irei considerar como "PASS ``````


# aqui também já preencho a linha que estava vazia

# corrindo rating
movies_rating <-
  movies_rating %>%
  # corrigindo colunas relativas ao "rating" de acordo com a base "movies"
  dplyr::mutate(
    rating = dplyr::case_when(
      clean_test == "nowomen" ~ 0,
      clean_test == "notalk" ~ 1,
      clean_test == "men" ~ 2,
      clean_test == "dubious" ~ 3,
      clean_test == "ok" ~ 3,
      TRUE ~ rating),
    
    description = dplyr::case_when(
      clean_test == "nowomen" ~ "unscored",
      clean_test == "notalk" ~ "It has to have at least two [named] women in it",
      clean_test == "men" ~ "Who talk to each other",
      clean_test == "dubious" ~ "About something besides a man",
      clean_test == "ok" ~ "About something besides a man",
      TRUE ~ description
    ),
    
    descricao = dplyr::case_when(
      clean_test == "nowomen" ~ "Sem pontuação",
      clean_test == "notalk" ~ "Tem pelo menos duas mulheres nomeadas",
      clean_test == "men" ~ "As mulheres falam entre si",
      clean_test == "dubious" ~ "As mulheres falam entre si sobre algo que não seja um homem",
      clean_test == "ok" ~ "As mulheres falam entre si sobre algo que não seja um homem",
      TRUE ~ description
    )
  ) %>%
  
  # considerando dubious como PASS
  dplyr::mutate(binary = dplyr::case_when(clean_test == "dubious" ~ "PASS",
                                          TRUE ~ binary))

# validando
movies_rating %>% 
  dplyr::group_by(rating, binary, clean_test) %>% 
  dplyr::summarise(n = dplyr::n())


# Exportar bases ----------------------------------------------------------

# exportar bases em formato .rds
readr::write_rds(movies, "./dados/movies.rds")
readr::write_rds(raw_bechdel, "./dados/raw_bechdel.rds")
readr::write_rds(legend_rating, "./dados/legend_rating.rds")
readr::write_rds(movies_rating, "./R/2021-04-09_movies_rating_bechdel_test.rds")


# writexl::write_xlsx(movies, "./dados/movies.xlsx")
# writexl::write_xlsx(raw_bechdel,"./dados/raw_bechdel.xlsx")
# writexl::write_xlsx(movies_rating,"./dados/movies_rating.xlsx")
