
library(tidyverse)
library(dados)

jabba <- dados_starwars |>
  filter(nome == "Jabba Desilijic Tiure")


# anotacoes simples -------------------------------------------------------



dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  annotate(
    "label",
    x = jabba$massa - 100,
    y = jabba$altura + 10,
    label = jabba$massa,
    # hjust = 1.1,
    # vjust = -1,
    # nudge_x = -1000,
    # nudge_y = 100
  )

# setas -------------------------------------------------------------------

dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  geom_curve(
    aes(
      x = massa - 500,
      y = altura - 50,
      xend = massa - 5,
      yend = altura - 3
    ),
    arrow = arrow(
      type = "closed", length = unit(.4, "cm")
    ),
    color = "red",
    curvature = .4,
    data = jabba
  ) +
  annotate(
    "label",
    x = jabba$massa - 500,
    y = jabba$altura - 50,
    label = "Jabba",
    # hjust = 1.1,
    # vjust = -1,
    # nudge_x = -1000,
    # nudge_y = 100
  )


# anotacao com uma imagem -------------------------------------------------

u_img <- "https://www.pikpng.com/pngl/b/277-2778885_jabba-the-hut-6-star-wars-black-series.png"
img <- httr::content(httr::GET(u_img))

dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  annotation_raster(
    img,
    xmin = jabba$massa - 500,
    xmax = jabba$massa - 5,
    ymin = jabba$altura - 50,
    ymax = jabba$altura - 2
  )


# NOVO: gghighlight -------------------------------------------------------

library(gghighlight)

dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point(color = "blue") +
  gghighlight(
    massa > 1000,
    label_key = nome,
    unhighlighted_colour = "green"
  )

dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  gghighlight(
    altura < 150 | massa > 1000,
    label_key = nome
  )


# ggrepel -----------------------------------------------------------------


library(ggrepel)

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  geom_label_repel(aes(label = nome))


# ggalt -------------------------------------------------------------------

library(ggalt)

dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  geom_encircle(
    data = jabba,
    color = "red",
    s_shape = 0,
    expand = 0,
    spread = .02,
    size = 2
  )


dados_starwars |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  geom_encircle(
    data = filter(dados_starwars, altura < 150),
    color = "red",
    s_shape = 0,
    expand = 0.05,
    spread = 1,
    size = 2
  ) +
  geom_encircle(
    data = filter(dados_starwars, altura > 200),
    color = "blue",
    s_shape = 0,
    expand = 0.05,
    spread = 1,
    size = 2
  )
