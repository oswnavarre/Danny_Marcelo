library(tidyverse)
library(agricolae)
library(readxl)
library(writexl)
library(jtools)
library(extrafont)
loadfonts(device = "win")

datos <- read_excel("datos.xlsx")

datos <- datos |> 
  drop_na() |>
  mutate(
    Medición = as.numeric(factor(Fecha))
  ) |>
  select(Medición, 
         Tratamiento, 
         Vigor,
         Dosis,
         Textura,
         SPAD,
         ALTURA)

datos$Vigor <- factor(datos$Vigor,
                      levels =c("Bajo","Medio","Alto"))

datos$Dosis <- factor(datos$Dosis,
                      levels =c(0,1,2,3))

datos_largos <- datos |>
  pivot_longer(
    5:7,
    names_to = "Variable",
    values_to = "Valor"
  ) 


stats_vars <- datos_largos |>
  group_by(Medición, Vigor, Dosis,  Variable) |>
  summarise(
    Media = mean(Valor),
    Desv = sd(Valor)
  )

stats_vars_vigor <- datos_largos |>
  group_by(Medición, Vigor,  Variable) |>
  summarise(
    Media = mean(Valor),
    Desv = sd(Valor)
  )

stats_vars_dosis <- datos_largos |>
  group_by(Medición, Dosis,  Variable) |>
  summarise(
    Media = mean(Valor),
    Desv = sd(Valor)
  )

variables <- unique(datos_largos$Variable)

for(i in seq_along(variables)){
  df <- stats_vars |>
    filter(Variable == variables[i])
  g2 <- ggplot(df, aes(x = Medición, y = Media, col = Dosis)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = unique(df$Medición)) +
    facet_wrap(. ~ Vigor, nrow = 1) +
    labs(x = "Medición") + 
    theme_apa() +
    theme(
      legend.position = "bottom",
      text = element_text(family = "Arial"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10)
    )
  nombre <- paste0("evolucion_",variables[i],".png")
  ggsave(nombre,g2,width = 8, height = 4)
}
