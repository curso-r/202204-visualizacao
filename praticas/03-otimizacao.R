
library(tidyverse)
library(dados)

imagem <- "https://wallpaperaccess.com/full/11836.jpg"

vader_annotate <- dados_starwars |>
  filter(nome == "Darth Vader")

vader <- dados_starwars |>
  mutate(vader = nome == "Darth Vader")

p <- dados_starwars |>
  # filter(massa < 1000) |>
  ggplot() +
  aes(massa, altura) +
  geom_point(
    color = "yellow",
    shape = 23,
    fill = "yellow",
    alpha = .8,
    size = 2
  ) +
  geom_point(
    data = vader_annotate,
    fill = "red",
    color = "red",
    shape = 23,
    size = 2,
    alpha = 1
  ) +
  geom_label(
    data = vader_annotate,
    label = "vader",
    fill = "red",
    family = "Star Jedi",
    nudge_x = -5,
    nudge_y = 12
  ) +
  coord_cartesian(xlim = c(0, 200)) +
  labs(
    x = "Massa",
    y = "Altura",
    title = "Star Wars",
    subtitle = "May the force be with you",
    caption = "Fonte: pacote {dados}"
  ) +
  # annotate(
  #   "point",
  #   x = vader_annotate$massa,
  #   y = vader_annotate$altura,
  #   color = "red",
  #   fill = "red",
  #   shape = 23,
  #   size = 2
  # ) +
  # annotate(
  #   "label",
  #   x = vader_annotate$massa,
  #   y = vader_annotate$altura,
  #   label = "vader",
  #   color = "black",
  #   fill = "red",
  #   vjust = -.5,
  #   family = "Star Jedi"
  # ) +
  theme(
    plot.title = element_text(
      color = "yellow",
      size = 30,
      hjust = .5,
      family = "Star Jedi"
    ),
    plot.subtitle = element_text(
      color = "yellow",
      size = 12,
      hjust = .5,
      family = "Star Jedi"
    ),
    plot.background = element_rect(
      fill = "black"
    ),
    axis.title = element_text(
      color = "yellow",
      size = 10,
      family = "Star Jedi"
    ),
    axis.text = element_text(
      color = "yellow",
      size = 8,
      family = "Star Jedi"
    ),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#555555")
  )

# element_line()
# element_rect()
# element_text()

## install.packages("ggimage")

ggimage::ggbackground(
  p, imagem
)

# ggimage::geom_image()

# Import -----------------------------------------------------------------------

url0 <- "https://raw.githubusercontent.com/adamribaudo/storytelling-with-data-ggplot/master/data/FIG0921-26.csv"
da_raw <- read_csv(url0)

# Tidy -------------------------------------------------------------------------

# queremos chegar em uma tabela que tem as seguintes colunas
# categoria, ano, percentual

da_tidy <- da_raw %>%
  pivot_longer(
    -Category,
    names_to = "ano",
    values_to = "perc_funders") %>%
  janitor::clean_names() %>%
  mutate(
    ano = as.integer(ano),
    perc_funders = parse_number(perc_funders)/100
  )

funcao_para_iterar <- function(tipo_painel, dados = da_tidy) {
  da_tidy %>%
    mutate(
      realce = if_else(category == tipo_painel, 1, 0),
      painel = tipo_painel
    )
}

# Visualize --------------------------------------------------------------------

da_visualizar <- map_dfr(
  unique(da_tidy$category),
  funcao_para_iterar
)

tab <- da_visualizar %>%
  mutate(
    realce = factor(realce),
    numero = ifelse(
      (ano == min(ano) | ano == max(ano)) & realce == 1, 1, 0
    ),
    hjust = case_when(
      ano == min(ano) ~ 1.2,
      ano == max(ano) ~ -0.3,
      TRUE ~ NA_real_
    ),
    painel = str_wrap(painel, 5)
  )

tab %>%
  filter(realce == 0) |>
  ggplot(aes(x = ano, y = perc_funders)) +
  geom_line(size = 1.3, color = "grey", aes(group = category)) +
  geom_line(
    data = filter(tab, realce == 1),
    size = 1.3,
    color = "royal blue"
  ) +
  geom_point(
    data = filter(tab, numero == 1),
    aes(x = ano, y = perc_funders),
    color = "black"
  ) +
  geom_text(
    data = filter(tab, numero == 1),
    aes(
      x = ano,
      y = perc_funders,
      label = scales::percent(perc_funders, accuracy = 1),
      hjust = hjust
    ),
    color = "black"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  ) +
  # sugestão do BRUNO MIOTO
  scale_x_continuous(position = 'top') +
  #scale_x_continuous(guide = guide_axis(position = 'top')) +
  labs(x = "", y = "") +
  facet_wrap(vars(painel), nrow = 5,  strip.position = "left") +
  theme(
    # panel.background = element_rect(fill = 'black'),
    strip.text.y.left = element_text(angle = 0, hjust = 0),
    plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.line.x = element_line()
  ) +
  scale_color_viridis_d(begin = .5) +
  coord_cartesian(clip = "off")
