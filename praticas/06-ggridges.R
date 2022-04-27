
library(dados)
library(tidyverse)
library(ggridges)

# ruim
diamante |>
  ggplot() +
  aes(preco, fill = corte) +
  geom_histogram(position = "identity")

# ruim
diamante |>
  ggplot() +
  aes(preco, fill = corte) +
  geom_density(alpha = .4)

diamante |>
  ggplot() +
  aes(preco, fill = corte) +
  facet_wrap(~corte) +
  geom_density(alpha = .4)

diamante |>
  ggplot() +
  aes(preco, y = corte, fill = corte) +
  geom_density_ridges(alpha = .4)

# c("a", "c", "b")
# factor(c("a", "c", "b"))
# factor(c("a", "c", "b"), ordered = TRUE)

factor(c("Justo", "Bom", "Muito Bom"),
       levels = c("Justo", "Bom", "Muito Bom"))

# diamante |>
#   ggplot() +
#   aes(preco, y = corte, fill = corte) +
#   ggridges::line(alpha = .4)


# exemplo de aplicação ----------------------------------------------------

estrela <- "★"

labs_estrela <- map_chr(1:5, ~paste(rep(estrela, .x), collapse = ""))

labs_estrela <- purrr::map_chr(1:5, ~shinipsum::random_text(.x*10))


media_geral <- mean(diamante$preco)
diamante |>
  ggplot() +
  aes(preco, y = corte, fill = corte) +
  geom_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    alpha = .4
  ) +
  geom_vline(
    xintercept = media_geral,
    colour = "red",
    linetype = 2
  ) +
  scale_y_discrete(
    labels = str_wrap(labs_estrela, width = 20)
  ) +
  scale_fill_viridis_d(
    option = "E"
  ) +
  scale_x_continuous(labels = scales::dollar) +
  theme_classic(14) +
  labs(x = "Preço", y = "Qualidade") +
  theme(legend.position = "none")
