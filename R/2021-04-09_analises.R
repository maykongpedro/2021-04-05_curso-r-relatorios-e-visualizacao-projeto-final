
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
total_de_filmes <- nrow(movies_rating)
total_de_filmes

# quantidade de filmes por ano
movies_rating %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n = dplyr::n())



# Visualizar dados --------------------------------------------------------

# definir fonte de texto para os gráficos
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
font_family <- "Palatino"

# definir cores
# pegando o código das cores da paletta viridis
scales::show_col(viridis::viridis_pal(option = "viridis")(7)) 

# verde escuro
#paleta_viridis_modificada <- c("#440154", "#443183", "#31688E", "#35B779")

# verde claro
paleta_viridis_modificada <- c( "#443183", "#31688E", "#21908C","#8FD744")

# verde claro
paleta_viridis_modificada <- c("#440154", "#443183", "#31688E", "#8FD744")


# definir nota de rodapé
nota_rodape <- "FONTE: Dados Bechdel Test - TidyTuesday: Week 11 (2021)"


# quantidade de filmes por ano e por resultado do teste
movies_rating %>%
  dplyr::group_by(year, binary) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = binary)) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 130)) +
  #ggplot2::scale_fill_viridis_d() +
  ggplot2::scale_fill_manual(values = c(paleta_viridis_modificada[1], paleta_viridis_modificada[4])) +
  ggplot2::labs(
    x = "Ano de lançamento",
    y = "Quantidade de filmes",
    fill = "Teste de Bechdel",
    caption = nota_rodape
  ) +
  ggplot2::ggtitle("Quantidade de Filmes por Ano - Teste de Bechdel") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = c(.30, .78)
  ) +
  theme_bechdel_cols(font_family) 
 

# quantidade de filmes por ano e por resultado da classificação
#plot <-
movies_rating %>%
  dplyr::mutate(rating = as.character(rating)) %>%
  tidyr::unite("rating", c("rating", "descricao"), sep = " - ") %>%
  dplyr::group_by(year, binary, rating) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = rating)) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 150),
    breaks = seq(0, 150, by = 25)
  ) +
  ggplot2::scale_fill_manual(
    values = paleta_viridis_modificada,
    guide = ggplot2::guide_legend(
      keyheight = grid::unit(6, units = "mm"),
      keywidth = grid::unit(9, units = "mm")
    )
  ) +
  ggplot2::labs(
    x = "Ano de lançamento", 
    y = "Quantidade de filmes", 
    fill = "Pontuação - Teste de Bechdel",
    caption = nota_rodape) +
  ggplot2::ggtitle("Quantidade de Filmes - Pontuação pelo Teste de Bechdel") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = c(.35, .78),
    panel.grid.major.y = ggplot2::element_line(size = .6,color = "#dddddd",linetype = "dashed")
  ) +
  theme_bechdel_cols(font_family) +
  
  # anotação de "PASS"
  ggplot2::geom_label(ggplot2::aes(x = 2005, y = 25, label = "PASS"),
                    hjust = 0.4,
                    vjust = 0.5,
                    colour = "#555555",
                    fill = NA,
                    label.size = NA,
                    family= font_family,
                    size = 10) +
  
  # anotação explicativa
  ggplot2::geom_label(
    ggplot2::aes(x = 1986, y = 63, label = "Apenas os filmes com \npontuação 3 passam no teste."),
    hjust = 0.4,
    vjust = 0.5,
    colour = "#555555",
    fill = NA,
    label.size = NA,
    family = font_family,
    size = 4
  ) +

  # flecha
  ggplot2::geom_curve(ggplot2::aes(x = 1992, y = 60, xend = 2003, yend = 28), 
           colour = "#555555", 
           size=0.5, 
           curvature = -0.2,
           arrow = ggplot2::arrow(length = grid::unit(0.03, "npc")))
           

# quantidade de filmes por ano e por resultado (%)
#plot <-
  movies_rating %>% 
  dplyr::mutate(rating = as.character(rating)) %>% 
  tidyr::unite("rating", c("rating", "descricao"), sep = " - ") %>% 
  
  dplyr::group_by(year, binary, rating) %>% 
  dplyr::arrange(year) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = rating), position = "fill") +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  #ggplot2::scale_fill_viridis_d(direction = 1 , option = "viridis") +
  ggplot2::scale_fill_manual(values = paleta_viridis_modificada) +
  ggplot2::labs(x = "Ano de lançamento", y = "", caption = nota_rodape) +
  ggplot2::ggtitle("Percentual de filmes por pontuação do teste de Bechdel") +
  ggplot2::theme_minimal(base_family = font_family) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
  ) +
  theme_bechdel_cols(font_family) +
  ggplot2::geom_label(ggplot2::aes(x = 1990, y = 0.20, label = "PASS"),
           hjust = 0.4,
           vjust = 0.5,
           colour = "#555555",
           #fill = "white",
           label.size = NA,
           family = font_family,
           size = 10) +
  ggplot2::geom_label(ggplot2::aes(x = 1990, y = 0.83, label = "FAIL"),
                      #hjust = 0.4,
                      #vjust = 0.0,
                      colour = "#555555",
                      fill = "white",
                      label.size = NA,
                      family = font_family,
                      size = 10)


#ggplot2::unit("top",right","botton", "left")

plot %>% 
ggplot2::ggsave(filename = "./outputs/plot_quant_movies.png",
                width = 12,
                height = 8,
                dpi = 300)



# quantidade de filmes por gênero
movies_rating



# quantidade de filmes por país



# filmes com boas notas no imdb tendem a passar no teste de bechdel?


