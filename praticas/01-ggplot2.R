
# Revisão do ggplot2 ------------------------------------------------------

library(tidyverse)
library(dados)

mtcarros
diamante
dados_starwars


# o lego do ggplot2 -------------------------------------------------------

## Passo 1: base de dados

ggplot()

ggplot(dados_starwars)

dados_starwars |>
  ggplot()

## Passo 2: mapeamentos estéticos

ggplot(dados_starwars, aes(x = massa))

ggplot(dados_starwars, aes(y = altura))

ggplot(dados_starwars, aes(x = massa, y = altura))

ggplot(dados_starwars) +
  aes(x = massa, y = altura)

dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura)

## Passo 3: Formas geométricas

# temos muitas geom_* diferentes. 2 tipos principais:
# geoms individuais (cada geometria representa UMA LINHA da base de dados)
# geoms agrupados (cada geometria representa UM CONJUNTO de linhas)

# JABBA THE HUTT

dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point()

dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura, colour = genero) +
  geom_point()

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura, colour = genero) +
  geom_point()


dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  # aqui o aes() se aplica a todos os geoms que vierem
  aes(x = massa, y = altura) +
  geom_point(colour = "royalblue", shape = 2)

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  geom_point(
    # AES só se aplica a esse geom_point()
    aes(x = massa, y = altura),
    colour = "royalblue", shape = 2
  )

# aes() dentro da geom vs aes() global

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point(
    # AES só se aplica a esse geom_point()
    aes(colour = genero),
    size = 4
  ) +
  geom_point()

## outras geometrias...

### geom_bar() e geom_col()

dados_starwars |>
  ggplot() +
  aes(genero) +
  geom_bar()

dados_starwars |>
  count(genero) |>
  ggplot() +
  aes(genero, n) +
  geom_col()

dados_starwars |>
  count(genero) |>
  ggplot() +
  aes(n, genero) +
  geom_col()

### geom_line()

voos |>
  count(mes) |>
  ggplot(aes(mes, n)) +
  geom_col()

voos |>
  count(mes) |>
  ggplot(aes(mes, n)) +
  geom_line()

voos |>
  count(mes, companhia_aerea) |>
  ggplot(aes(mes, n, colour = companhia_aerea)) +
  geom_line()

voos |>
  count(mes, companhia_aerea) |>
  ggplot(aes(mes, n)) +
  geom_line() +
  facet_wrap(~companhia_aerea)

voos |>
  count(companhia_aerea, mes) |>
  pivot_wider(
    names_from = companhia_aerea,
    values_from = n
  ) |>
  # ruim!
  ggplot() +
  geom_line(aes(x = mes, y = AA)) +
  geom_line(aes(x = mes, y = B6))

voos |>
  count(companhia_aerea, mes) |>
  pivot_wider(
    names_from = companhia_aerea,
    values_from = n
  ) |>
  # melhor!
  pivot_longer(-mes) |>
  ggplot() +
  geom_line(aes(x = mes, y = value, colour = name))

### distribuições! geom_histogram vs geom_density

dados_starwars |>
  ggplot() +
  aes(x = altura) +
  geom_histogram(bins = 20) +
  ## ESCALAS
  scale_x_continuous(
    breaks = seq(0, 300, 30),
    labels = \(x) paste0(x, "cm"),
    limits = c(0, 300)
  )

dados_starwars |>
  ggplot() +
  aes(x = altura) +
  geom_density(adjust = 1)

dados_starwars |>
  drop_na(genero) |>
  ggplot() +
  aes(x = altura, colour = genero) +
  geom_density(adjust = 1)

dados_starwars |>
  drop_na(genero) |>
  ggplot() +
  aes(x = altura, fill = genero) +
  geom_density(adjust = 1, alpha = .4)

dados_starwars |>
  drop_na(massa, altura) |>
  esquisse::esquisser()

drop_na(dados_starwars, massa, altura) %>%
  filter(massa >= 15L & massa <= 1054L) %>%
  ggplot() +
  aes(x = massa, y = altura, colour = genero) +
  geom_point(shape = "square", size = 3.05) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  labs(
    title = "Meu primeiro grafico com esquisse",
    subtitle = "Dados do Star Wars"
  ) +
  theme_dark() +
  theme(legend.position = "bottom")

voos |>
  mutate(data = lubridate::ymd(paste(ano, mes, dia, sep = "-"))) |>
  esquisse::esquisser()

## elementos extras

### facets

dados_starwars |>
  drop_na(sexo_biologico) |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  facet_wrap(~sexo_biologico, scales = "free")

dados_starwars |>
  mutate(humano = if_else(
    especie == "Humano", "Humano", "Não-humano"
  )) |>
  drop_na(genero) |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  facet_wrap(~genero+humano, scales = "free")

dados_starwars |>
  mutate(humano = if_else(
    especie == "Humano", "Humano", "Não-humano"
  )) |>
  drop_na(genero) |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  facet_grid(humano~genero, scales = "free")

### Escalas

auxiliar <- dados_starwars |>
  filter(massa > 120, massa < 1000) |>
  summarise(massa = mean(massa), altura = mean(altura))

dados_starwars |>
  mutate(humano = if_else(
    especie == "Humano", "Humano", "Não-humano"
  )) |>
  drop_na(genero) |>
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  geom_smooth() +
  geom_point(size = 4, colour = "red", data = auxiliar) +
  scale_x_continuous(
    # limits = c(0, 200),
    breaks = seq(0, 200, 20),
    labels = \(x) paste0(x, "kg")
  ) +
  scale_y_continuous(
    # limits = c(0, 250),
    breaks = seq(0, 250, 50),
    labels = \(x) paste0(x, "cm")
  ) +
  coord_cartesian(xlim = c(0, 200))

### Transformações

dados_starwars |>
  ggplot() +
  aes(sexo_biologico) +
  geom_bar()

dados_starwars |>
  count(sexo_biologico) |>
  ggplot() +
  aes(sexo_biologico, n) +
  geom_col()

dados_starwars |>
  count(sexo_biologico) |>
  ggplot() +
  aes(sexo_biologico, n) +
  geom_bar(stat = "identity")


voos |>
  ggplot() +
  aes(mes, tempo_voo, group = companhia_aerea) +
  geom_point(stat = "sum")

voos |>
  group_by(mes, companhia_aerea) |>
  summarise(tempo_voo = sum(tempo_voo)) |>
  ggplot() +
  aes(mes, tempo_voo) +
  geom_point()

### coordenadas

# pizza
dados_starwars |>
  ggplot() +
  aes(x = "asdas", fill = genero) |>
  geom_bar() +
  coord_polar(theta = "y")
