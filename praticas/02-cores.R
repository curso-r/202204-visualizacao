
library(dados)
library(tidyverse)


# cores -------------------------------------------------------------------

# theme_set(theme_minimal())

p1 <- dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(massa, altura, colour = sexo_biologico) +
  geom_point(size = 3) +
  theme_minimal()

p2 <- dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(massa, altura, colour = log10(ano_nascimento)) +
  geom_point(size = 3) +
  theme_minimal()

p1
p2

# discretas
p1 +
  scale_colour_brewer(palette = "Set2")

p1 +
  scale_colour_brewer(palette = "Set2", na.value = "#000000")

p1 +
  scale_colour_viridis_d(
    option = "D",
    begin = .2,
    end = .8, na.value = "#999999"
  )

rainbow(3)
hcl.colors(3)

p1 +
  scale_colour_manual(
    values = hcl.colors(3),
    na.value = "#999999"
  )


p1 +
  scale_colour_hue(
    h = c(0, 360) + 90,
    c = 100,
    l = 65,
    h.start = 0
  )


# continuas
p2 +
  scale_color_distiller(palette = "Oranges")

p2 +
  scale_color_fermenter(palette = "Oranges")

p2 +
  scale_color_viridis_c(option = "A")

p2 +
  scale_color_viridis_b(option = "A", n.breaks = 5)



# quero manter as mesmas cores para
# determinadas categorias
tb1 <- tibble::tibble(
  a = factor(letters[1:5], levels = letters[1:5]),
  b = 1:5
)

tb2 <- tibble::tibble(
  a = factor(letters[3:5], levels = letters[1:5]),
  b = 4:6
)

p1 <- tb1 |>
  ggplot(aes(x = a, y = b, fill = a)) +
  geom_col()

p2 <- tb2 |>
  ggplot(aes(x = a, y = b, fill = a)) +
  geom_col() +
  scale_fill_discrete(drop = FALSE)


library(patchwork)
patchwork::wrap_plots(p1, p2)
# spoiler!
p1/p2

# temas -------------------------------------------------------------------

# install.packages("ggthemes")

p1 +
  scale_color_brewer(palette = "Set2") +
  ggthemes::theme_excel()

p1 +
  scale_color_brewer(palette = "Set2") +
  ggthemes::theme_wsj()


# tvthemes ----------------------------------------------------------------

# install.packages("tvthemes")
p1 +
  tvthemes::scale_colour_spongeBob() +
  tvthemes::theme_spongeBob(
    title.font = "Some Time Later",
    title.size = 40
  ) +
  labs(title = "Duas horas depois...")


## install.packages("extrafont")
# extrafont::font_import("exemplos_de_aula/fontes", prompt = FALSE)
# extrafont::loadfonts("win")
