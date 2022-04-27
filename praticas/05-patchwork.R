
# gridExtra::grid.arrange()

# library(patchwork)

library(tidyverse)
library(dados)

p1 <- dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point()

p2 <- dados_starwars |>
  ggplot() +
  aes(massa) +
  geom_histogram()

p3 <- dados_starwars |>
  ggplot() +
  aes(altura) +
  geom_histogram()

p4 <- dados_starwars |>
  ggplot() +
  aes(genero) +
  geom_bar()

# gridExtra::grid.arrange(p1, p2)

library(patchwork)

p1 + p2
p1 + p2 + p3
p1 + p2 + p3 + p4
p1 | p2 | p3 | p4

p1 / p2

p1 + p2 + plot_layout(ncol = 1)

(p1 + p2) / p3
(p1 | p2) / p3

## avançado

(p1 + plot_spacer() + p2) / p3

((p2 / p3) | p1) + plot_layout(widths = c(1, 3))

p22 <- p2 +
  theme(plot.background = element_blank())

# adicionando grafico no grafico
p1 +
  inset_element(
    p22,
    left = 0.6, bottom = 0.6, right = 1, top = 1
  ) +
  inset_element(p22, left = 0.8, bottom = 0.8, right = 1, top = 1)
