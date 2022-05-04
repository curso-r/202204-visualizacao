library(tidyverse)
library(dados)
library(ggtext)

# vamos fazer um bd de pokemons
pokemons <- c("pikachu", "bulbasaur", "squirtle", "charmander")
infos_pokemon <- function(pokemon) {
  u <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon)
  r <- httr::GET(u)
  j <- httr::content(r)
  tibble::tibble(
    pokemon = pokemon,
    attack = j$stats[[1]]$base_stat,
    img = j$sprites$front_default
  )
}

da_pokemon <- purrr::map_dfr(pokemons, infos_pokemon) |>
  mutate(pokemon = fct_reorder(pokemon, attack)) |>
  arrange(pokemon)

gg_base <- da_pokemon |>
  ggplot() +
  geom_segment(
    aes(x = 0, xend = attack, y = pokemon, yend = pokemon)
  ) +
  geom_point(
    aes(x = attack, y = pokemon, colour = pokemon),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("gold", "red", "royalblue", "darkgreen")
  ) +
  theme_minimal() +
  labs(
    x = "Poder de ataque",
    y = "",
    title = "Temos que pegar **<span style='color:blue;'>Pokémon</span>**!"
  )

# download.file(da_pokemon$img[1], destfile = "poke1.png")

gg_base +
  scale_y_discrete(
    name = NULL,
    labels = glue::glue("<img src='{da_pokemon$img}'>")
  ) +
  theme(
    plot.title = element_markdown(family = "Pokemon Hollow", size = 20),
    axis.text.y = element_markdown()
  )

