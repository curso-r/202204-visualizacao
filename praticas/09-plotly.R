library(dados)
library(tidyverse)
library(plotly)

p <- dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura, colour = genero) +
  geom_point() +
  geom_smooth(aes(group = 1)) +
  theme(
    legend.position = "bottom"
  )

p

ggplotly(p) |>
  layout(
    legend = list(orientation = 'h')
  )

dados_starwars |>
  filter(massa < 1000) |>
  plot_ly(x = ~massa, y = ~altura)

