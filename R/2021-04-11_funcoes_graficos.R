
# tema geral para os gráficos
theme_bechdel_cols <- function(font_family){
  ggplot2::theme(
    
    # troca os elementos textuais do gráfico
    text = ggplot2::element_text(color = "#22211d", family = font_family),
    
    # deixa o título em negrito
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),

    # deixa os títulos do eixos em negrito
    axis.title = ggplot2::element_text(face = "bold"),
    
    # coloca uma linha no eixo x
    axis.line.x = ggplot2::element_line(size = 1),
    
    # pinta a parte de plot
    plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    
    # pinta o painel
    panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
    
    # retira o fundo da caixa de legenda
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    
    # define elementos textuais do título da legenda
    legend.title = ggplot2::element_text(face = "bold", family = font_family),
    
    # define elementos textuais da legenda
    legend.text = ggplot2::element_text(family = font_family),
    
    # posiciona a legenda no canto esquerdo
    plot.caption = ggplot2::element_text(hjust = 0),
    
    # ajusta as margens do gráfico
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm") 
    
  )
  
} 

  