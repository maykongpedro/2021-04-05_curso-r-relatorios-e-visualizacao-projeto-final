# carregar pipe
'%>%' <- magrittr::`%>%`

# objetivo
  # Gerar um resumo por gênero do teste de bechdel

# importar base
movies_rating <- readr::read_rds("./R/2021-04-09_movies_rating_bechdel_test.rds")

# explorar base
dplyr::glimpse(movies_rating)


# quantidade de filmes por gênero
generos <-
  movies_rating %>% 
  dplyr::mutate(genre = stringr::str_replace_all(genre, pattern = "-", 
                                                 replacement = "")) %>% 
  tidyr::separate(col = genre, into = c("genero_1", "genero_2", "genero_3"), sep = "\\,")  %>% 
  dplyr::select("genero_1", "genero_2", "genero_3")

genero_1 <-
  generos %>% 
  dplyr::select("genero_1") %>% 
  dplyr::rename("genero" = "genero_1")

genero_2 <- 
  generos %>% 
  dplyr::select("genero_2") %>% 
  dplyr::rename("genero" = "genero_2")

genero_3 <- 
  generos %>% 
  dplyr::select("genero_3") %>% 
  dplyr::rename("genero" = "genero_3")


# gerar uma tabela com os gêneros únicos
generos_unicos <- tibble::tibble()

generos_unicos <- dplyr::bind_rows(generos_unicos, genero_1)
generos_unicos <- dplyr::bind_rows(generos_unicos, genero_2)
generos_unicos <- dplyr::bind_rows(generos_unicos, genero_3)

generos_unicos <- 
  unique(generos_unicos) %>% 
  dplyr::filter(!is.na(genero)) %>% 
  dplyr::mutate(genero = stringr::str_trim(genero))


# gráficos
movies_rating %>% 
  dplyr::filter(stringr::str_detect(genre, "Horror")) %>% 
  View()


# teste
genero <- "Horror"
base_genero_filtrado <- 
  movies_rating %>% 
  dplyr::filter(stringr::str_detect(genre, genero)) %>% 
  dplyr::group_by(genre, binary, rating ) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(genero_filtrado = genero)


# teste de loop
for (genero in generos_unicos){
  
  print(genero)
  
}


# ele falha nesse loop
base_genero_consolidado <- tibble::tibble()

for (genero in generos_unicos){
  
  base_genero_filtrado <- 
    movies_rating %>% 
    dplyr::filter(stringr::str_detect(genre, genero)) %>% 
    dplyr::group_by(genre, binary, rating ) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::mutate(genero_filtrado = genero)
  
  base_genero_consolidado <- dplyr::bind_rows(base_genero_consolidado, base_genero_filtrado)
  
}
