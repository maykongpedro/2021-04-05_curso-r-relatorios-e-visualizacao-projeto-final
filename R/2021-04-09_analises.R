
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
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(x = "Ano de lançamento", y = "Quantidade de filmes") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 1),
    axis.text.x = ggplot2::element_text(angle = 90)
  )


# quantidade de filmes por ano e por resultado da classificação
font_grafico <- "Helvetica"
windowsFonts(Times=windowsFont("Times New Roman"))

movies_rating %>% 
  dplyr::mutate(rating = as.character(rating)) %>% 
  tidyr::unite("rating", c("rating", "descricao"), sep = " - ") %>% 

  dplyr::group_by(year, binary, rating) %>% 
  dplyr::arrange(year) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  # dplyr::mutate(
  #   rating = factor(
  #     rating,
  #     levels = c(0, 1, 2, 3),
  #     ordered = TRUE
  #   )
  #) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = rating)) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  ggplot2::scale_fill_viridis_d(
    direction = 1 ,
    option = "viridis",
    guide = ggplot2::guide_legend(
      keyheight = grid::unit(6, units = "mm"),
      keywidth = grid::unit(9, units = "mm")
    )
  ) +
  #ggplot2::annotate(geom = "text", x = 2005, y = 25, label = "PASS", colour = "black", size = 10) +
  # ggplot2::annotate(geom = "text", x = 2005, y = 69, label = "FAIL", colour = "black", size = 9) +
  ggplot2::labs(x = "Ano de lançamento", y = "Quantidade de filmes", fill = "Pontuação - Teste de Bechdel") +
  ggplot2::ggtitle("Quantidade de Filmes - Pontuação pelo Teste de Bechdel") +
  #ggplot2::theme_minimal(13) +
  ggthemes::theme_clean(base_family = font_grafico)+
  ggplot2::theme(
    text = ggplot2::element_text(color = "#22211d", family = font_grafico),
    plot.title = ggplot2::element_text(hjust = 0.5),
    
    plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    
    axis.line.x = ggplot2::element_line(size = 1),
    axis.text.x = ggplot2::element_text(angle = 90),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    
    legend.position = c(.35, .78),
    legend.text = ggplot2::element_text(family = font_grafico),
    legend.title = ggplot2::element_text(face = "bold", family = font_grafico),
    
    panel.grid.major.y = ggplot2::element_line(size = .75, color = "#dddddd", linetype = "dashed"),
    #panel.grid = ggplot2::element_blank()
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
  ) 
  # ggtext::geom_textbox(
  #   aes(x = 2005, y = 25, label = "*PASS*"),
  #   height = grid::unit(8, "mm"),
  #   width = grid::unit(15, "mm"),
  #   # 73% of plot panel width
  #   hjust = 0,
  #   vjust = 1,
  # )
  # ggtext::geom_richtext(
  #   ggplot2::aes(
  #     x = 2005,
  #     y = 25,
  #     label = "*PASS*",
  #     fill = after_scale(alpha("#70af85", .5))
  #   ),
  #   text.colour = "black",
  #   hjust = 1, vjust = 1,
  #   #size = 10
  # )


# quantidade de filmes por ano e por resultado (%)
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
font_family <- "Palatino"

plot <-
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
  ggplot2::labs(x = "Ano de lançamento", y = "") +
  ggplot2::ggtitle("Percentual de filmes por pontuação do teste de Bechdel") +
  ggplot2::theme_minimal(base_family = font_family) +
  ggplot2::theme(
    
    text = ggplot2::element_text(color = "#22211d"),
    plot.title = ggplot2::element_text(hjust = 0.5, family = font_family),
    
    plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    
    axis.text.x = ggplot2::element_text(angle = 90),
    
    legend.text = ggplot2::element_text(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm") 
  ) +
  ggplot2::geom_label(ggplot2::aes(x = 1990, y = 0.20, label = "PASS"),
           hjust = 0.4,
           vjust = 0.5,
           colour = "#555555",
           #fill = "white",
           label.size = NA,
           family="Palatino",
           size = 10) +
  ggplot2::geom_label(ggplot2::aes(x = 1990, y = 0.83, label = "FAIL"),
                      #hjust = 0.4,
                      #vjust = 0.0,
                      colour = "#555555",
                      fill = "white",
                      label.size = NA,
                      family="Palatino",
                      size = 10)


plot

#ggplot2::unit("top",right","botton", "left")

plot %>% 
ggplot2::ggsave(filename = "./outputs/plot_percent_movies.png",
                width = 12,
                height = 8,
                dpi = 300)


# pegando o código das cores da paletta viridis
paleta_viridis_modificada <- c("#440154", "#443183", "#31688E", "#8FD744")

scales::show_col(viridis::viridis_pal(option = "viridis")(7)) 


# quantidade de filmes por gênero


# filmes com um lucro maior tendem a passar no teste de bechdel?

# filmes com boas notas no imdb tendem a passar no teste de bechdel?


