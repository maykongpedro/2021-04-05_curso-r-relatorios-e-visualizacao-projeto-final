
# carregar pipe
'%>%' <- magrittr::`%>%`

# objetivo
# explorar a base de filmes e responder alguns questionamentos sobre o teste de bechdel



# Importar base -----------------------------------------------------------
movies_rating <- readr::read_rds("./R/2021-04-09_movies_rating_bechdel_test.rds")
raw_bechdel <- readr::read_rds("./R/2021-04-11_raw_bechdel_with_legends.rds")



# Explorar dados ----------------------------------------------------------
dplyr::glimpse(movies_rating)


# 1. resultado do teste na base histórica

# quantidade total de filmes
total_de_filmes <- nrow(movies_rating)
total_de_filmes

# quantidade de filmes por ano
movies_rating %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(n = dplyr::n())


# Visualizar dados --------------------------------------------------------

# carregar função de tema
source("./R/2021-04-11_funcoes_graficos.R")

# definir fonte de texto para os gráficos
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
font_family <- "Palatino"

# definir cores
# pegando o código das cores da paletta viridis
scales::show_col(viridis::viridis_pal(option = "viridis")(7)) 

# verde escuro
#paleta_viridis_modificada <- c("#440154", "#443183", "#31688E", "#35B779")

# azul mais escuro + verde claro
#paleta_viridis_modificada <- c("#440154", "#443183", "#31688E", "#8FD744")

# azul mais claro + verde claro
paleta_viridis_modificada <- c( "#443183", "#31688E", "#21908C","#8FD744")


# definir notas
subtitulo <- paste0("Base de dados com ", total_de_filmes, " filmes")
nota_rodape <- "FONTE: Dados Bechdel Test - TidyTuesday: Week 11 (2021)"


# quantidade de filmes por ano e por resultado do teste
plot_historico_binary <-
  movies_rating %>%
  dplyr::group_by(year, binary) %>%
  dplyr::arrange(year) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = n, fill = binary)) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01), breaks = movies_rating$year) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 130)) +
  ggplot2::scale_fill_manual(values = c(paleta_viridis_modificada[1], paleta_viridis_modificada[4])) +
  ggplot2::labs(
    x = "Ano de lançamento",
    y = "Quantidade de filmes",
    fill = "Teste de Bechdel",
    caption = nota_rodape
  ) +
  ggplot2::ggtitle(label = "Quantidade de Filmes por Ano - Teste de Bechdel",
                   subtitle = subtitulo) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = c(.30, .78),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  theme_bechdel_cols(font_family) 

plot_historico_binary



# quantidade de filmes por ano e por resultado da classificação
plot_historico_rating <-
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
  ggplot2::ggtitle(label = "Quantidade de Filmes - Pontuação pelo Teste de Bechdel",
                   subtitle = subtitulo) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = c(.35, .78),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
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
    ggplot2::aes(x = 1986, y = 63, label = "Apenas os filmes que \npontuaram nos 3 itens \npassam no teste."),
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

plot_historico_rating


# quantidade de filmes por ano e por resultado (%)
plot_historico_percent_rating <-
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
  ggplot2::scale_fill_manual(values = paleta_viridis_modificada) +
  ggplot2::labs(x = "Ano de lançamento", y = "", caption = nota_rodape, fill = "") +
  ggplot2::ggtitle(label = "Percentual de filmes por pontuação do teste de Bechdel",
                   subtitle = subtitulo) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
  ) +
  theme_bechdel_cols(font_family) +
  ggplot2::geom_label(ggplot2::aes(x = 1990, y = 0.15, label = "PASS"),
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

plot_historico_percent_rating

# para nao me perder
#ggplot2::unit("top",right","botton", "left")


# acabei não usando isso no final
# transformando em fator a descrição
# dplyr::mutate(rating = factor(
#   rating,
#   levels = c("0 - Sem pontuação",
#              "1 - Tem pelo menos duas mulheres nomeadas",
#              "2 - As mulheres falam entre si",
#              "3 - As mulheres falam entre si sobre algo que não seja um homem"),
#   ordered = TRUE
# )) 


# média de orçamento por teste rating
orcamento <- movies_rating %>% 
  dplyr::mutate(rating = as.character(rating)) %>% 
  tidyr::unite("rating", c("rating", "descricao"), sep = " - ") %>% 
  dplyr::group_by(rating, binary) %>% 
  dplyr::summarise(media_orcamento = mean(budget_2013, na.rm = TRUE)) %>% 
  dplyr::mutate(media_orcamento = media_orcamento / 10 ^ 6) %>%
  dplyr::mutate(media_orcamento_label = sprintf("$ %1.2f MM", media_orcamento))
  
# gráfico de orçamento
plot_orcamento <-
  orcamento %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(
    ggplot2::aes(x = media_orcamento, y = rating),
    fill = ifelse(
      orcamento$rating == "3 - As mulheres falam entre si sobre algo que não seja um homem",
      "#35B779",
      "#bdc7c9"
    )
  ) +
  ggplot2::geom_label(ggplot2::aes(
    x = media_orcamento,
    y = rating ,
    label = media_orcamento_label,
    hjust = 1.2,
    family = font_family
  )) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(), limits = c(0, 70)) +
  ggplot2::labs(x = "Média de orçamento normalizado para 2013 (Milhões de dólares)",
                caption = nota_rodape) +
  ggplot2::ggtitle(label = "Pontuação do Teste de Bechdel por Média de Orçamento",
                   subtitle = "Valores normalizados para 2013") +
  ggplot2::theme_minimal() +
  theme_bechdel_bars(font_family)+
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = -1.7),
    plot.title = ggplot2::element_text(hjust = -3),
    plot.subtitle = ggplot2::element_text(hjust = .0)
  ) 
  # a tentativa com gghighlightt não foi bem-sucedida por que ele esconde as outras labels
  # gghighlight::gghighlight(rating == "3 - As mulheres falam entre si sobre algo que não seja um homem",
  #                          label_key = media_orcamento_label)

plot_orcamento


# filmes com boas notas no imdb tendem a passar no teste de bechdel?
notas_imdb <-
  movies_rating %>% 
  dplyr::mutate(rating = as.character(rating)) %>% 
  tidyr::unite("rating", c("rating", "descricao"), sep = " - ") %>% 
  dplyr::group_by(rating) %>% 
  dplyr::summarise(media_imdb = mean(imdb_rating, na.rm = TRUE)) %>% 
  dplyr::arrange(dplyr::desc(media_imdb)) %>% 
  dplyr::mutate(media_imdb = round(media_imdb, 2))
  
plot_notas <-
  notas_imdb %>% 
  ggplot2::ggplot() +
  ggplot2::geom_col(
    ggplot2::aes(x = media_imdb, y = rating),
    fill = ifelse(
      notas_imdb$rating == "3 - As mulheres falam entre si sobre algo que não seja um homem",
      "#35B779",
      "#bdc7c9"
    )
  ) +
  ggplot2::geom_label(
    ggplot2::aes(
      x = media_imdb,
      y = rating ,
      label = media_imdb,
      hjust = 1.2,
      family = font_family
    )
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(), limits = c(0, 10)) +
  ggplot2::labs(x = "Média de notas das avaliações no IMDB",
                caption = nota_rodape) +
  ggplot2::ggtitle(label = "Pontuação do Teste de Bechdel por Média de Avaliação no IMDB",
                   subtitle = subtitulo) +
  ggplot2::theme_minimal() +
  theme_bechdel_bars(font_family) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = -1.7),
    plot.title = ggplot2::element_text(hjust = 1.9),
    plot.subtitle = ggplot2::element_text(hjust = .0)
  ) 

plot_notas

# Salvar gráficos ---------------------------------------------------------

graficos_finais <- list(
  historico_1 = plot_historico_binary,
  historico_2 = plot_historico_rating,
  orcamento = plot_orcamento,
  notas = plot_notas
)


purrr::iwalk(graficos_finais, ~ggplot2::ggsave(paste0(.y, ".png"), .x, 
                                              path = "./outputs",
                                              height = grid::unit(5.5, units = "cm"),
                                              width = grid::unit(9.5, units = "cm")))



# gráfico com tamanho diferente
ggplot2::ggsave(plot = plot_historico_percent_rating,
                path = "./outputs",
                filename = "historico_3.png",
                height = grid::unit(7, units = "cm"),
                width = grid::unit(11, units = "cm"))



