library(tidyverse)
library(dados)
library(reactable)

dados_summ <- pinguins |>
  group_by(ano, ilha) |>
  summarise(
    bico_medio = mean(comprimento_bico, na.rm = TRUE),
    massa_corporal = mean(massa_corporal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = ano,
    values_from = c(bico_medio, massa_corporal)
  )

fmt_num <- colFormat(
  digits = 2
)

dados_summ |>
  reactable(
    columns = list(
      ilha = colDef(name = "Ilha"),
      bico_medio_2007 = colDef(
        name = "2007", format = fmt_num
      ),
      bico_medio_2008 = colDef(
        name = "2008", format = fmt_num
      ),
      bico_medio_2009 = colDef(
        name = "2009", format = fmt_num
      ),

      massa_corporal_2007 = colDef(
        name = "2007", format = fmt_num
      ),
      massa_corporal_2008 = colDef(
        name = "2008", format = fmt_num
      ),
      massa_corporal_2009 = colDef(
        name = "2009", format = fmt_num
      )

    ),

    columnGroups = list(
      colGroup(
        name = "Bico Médio",
        columns = c(
          "bico_medio_2007",
          "bico_medio_2008",
          "bico_medio_2009"
        )
      ),
      colGroup(
        name = "Massa Corporal",
        columns = c(
          "massa_corporal_2007",
          "massa_corporal_2008",
          "massa_corporal_2009"
        )
      )

    ),
    striped = TRUE,
    highlight = TRUE
  )

DT::datatable(dados_summ)
