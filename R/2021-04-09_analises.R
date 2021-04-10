
# carregar pipe
'%>%' <- magrittr::`%>%`

# objetivo
# explorar a base de filmes e responder alguns questionamentos sobre o teste de bechdel, como:

  # 1. Como a base histórica se saiu no teste?
  # 2. 
  # 3.


# importar base
movies_rating <- readr::read_rds("./R/2021-04-09_movies_rating_bechdel_test.rds")

# explorar base
dplyr::glimpse(movies_rating)

# verificar itens
unique(movies_rating$rating)

# tem uma linha que não apresenta rank númerico
movies_rating %>% 
  dplyr::filter(is.na(rating)) %>% 
  View()

  # porém ela apresenta um resultado de "pass"no teste

# corrigir essa linha
movies_rating$rating <-
  movies_rating$rating %>%
  tidyr::replace_na(replace = 3)

movies_rating$description <-
  movies_rating$description %>%
  tidyr::replace_na(replace = "About something besides a man")

movies_rating$descricao <-
  movies_rating$descricao %>%
  tidyr::replace_na(replace = "As mulheres falam entre si sobre algo que não seja um homem")


# 1. resultado do teste na base histórica

# quantidade total de filmes
nrow(movies_rating)

# quantidade de filmes por ano
movies_rating %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n = dplyr::n())

# quantidade de filmes por ano e por resultado do teste
movies_rating %>% 
  dplyr::group_by(year, binary) %>% 
  dplyr::arrange(year) %>% 
  dplyr::summarise(n = dplyr::n()) %>%
  
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = binary)) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, 130)) +
  ggplot2::labs(x = "Ano de lançamento", y = "Quantidade de filmes") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 1),
    axis.text.x = ggplot2::element_text(angle = 90)
  )

movies_rating %>% 
  dplyr::group_by(year, binary) %>% 
  dplyr::arrange(year) %>% 
  dplyr::summarise(n = dplyr::n()) %>%
  
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = n, y = as.character(year), fill = binary)) +
  ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0, 130)) +
  ggplot2::labs(y = "Ano de lançamento", x = "Quantidade de filmes", fill = "Teste de Bechdel") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_line(size = 1)
  )



# quantidade de filmes por ano e por resultado da classificação
movies_rating %>% 
  dplyr::group_by(year, binary, rating) %>% 
  dplyr::arrange(year) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 

  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = rating)) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, 130)) +
  ggplot2::scale_fill_viridis_c(direction = -1 ,option = "viridis") +
  ggplot2::labs(x = "Ano de lançamento", y = "Quantidade de filmes") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 1),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
