library(dados)
library(tidyverse)
library(gganimate)

anim <- dados_gapminder |>
  ggplot() +
  aes(
    expectativa_de_vida,
    log10(pib_per_capita),
    size = log10(populacao)
  ) +
  geom_point() +
  facet_wrap(~continente) +
  # gganimate começa aqui
  labs(
    x = "Expectativa de vida",
    y = "log10(PIB per capta)",
    title = "Ano: {frame_time}"
  ) +
  transition_time(ano)

anim

# library(gifski)
# library(av)

animate(
  anim,
  nframes = 40,
  duration = 10,
  start_pause = 2,
  end_pause = 2,
  width = 8,
  height = 5,
  units = "in",
  res = 300,
  renderer = gifski_renderer("output/gapminder.gif")
)


animate(
  anim,
  nframes = 40,
  duration = 10,
  start_pause = 2,
  end_pause = 2,
  width = 800,
  height = 400,
  renderer = av_renderer("output/gapminder.mp4")
)

## outra aplicação legal


anim_brasil <- dados_gapminder |>
  filter(continente == "Américas") |>
  ggplot() +
  aes(
    expectativa_de_vida/max(expectativa_de_vida),
    log10(pib_per_capita)/max(log10(pib_per_capita)),
    size = log10(populacao)
  ) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0) +
  gghighlight::gghighlight(
    pais == "Brasil",
    label_key = pais
  ) +
  labs(
    x = "Expectativa de vida {frame_time}",
    y = "log10(PIB per capta) {frame_time}",
    title = "Ano: {frame_time}"
  ) +
  transition_time(ano)

mtcars |>
  ggplot(aes(disp/max(disp), mpg/max(mpg))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

animate(
  anim_brasil,
  nframes = 40,
  duration = 10,
  start_pause = 2,
  end_pause = 2,
  width = 6,
  height = 4,
  units = "in",
  res = 200
)
