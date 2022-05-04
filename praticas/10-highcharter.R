library(dados)
library(tidyverse)
library(highcharter)

dados_starwars |>
  filter(massa < 1000) |>
  replace_na(list(genero = "<em>vazio</em>")) |>
  hchart("scatter", hcaes(
    x = massa, y = altura,
    group = genero
  ))

dados_starwars |>
  count(sexo_biologico) |>
  hchart("bar", hcaes(
    x = sexo_biologico, y = n
  ))
