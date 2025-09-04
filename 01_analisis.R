library(tidyverse)
library(agricolae)
library(readxl)
library(writexl)
library(jtools)
library(extrafont)
loadfonts(device = "win")

datos <- read_excel("datos.xlsx")
dicc_vars <- read_excel("datos.xlsx", sheet = "vars") 

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

valoresp_aov_vigor <- data.frame(
  Variable = variables,
  valorp = 0
)

valoresp_aov_dosis <- data.frame(
  Variable = variables,
  valorp = 0
)

valoresp_aov_vigor_dosis <- data.frame(
  Variable = variables,
  valorp = 0
)

grupos_vigor <- data.frame(
  Variable = NA,
  Vigor = NA,
  Grupos = NA
)

grupos_dosis <- data.frame(
  Variable = NA,
  Dosis = NA,
  Grupos = NA
)

for(i in seq_along(variables)){
  
  datos_grafico <- datos_largos |>
    filter(Variable == variables[i])
  variable_lab = dicc_vars[i,2]
  etiqueta_grafico = paste(variable_lab)
  
  nombre_grafico = paste0("graficos/",variables[i],".png")
  
  gvar <-ggplot(datos_grafico, aes(x = Dosis, y = Valor,  fill = Vigor)) +
    geom_boxplot() + 
    theme_apa() +
    ylab(etiqueta_grafico) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(color = "black", family = "Arial",size=12),
      axis.text.x = element_text(color = "black", family = "Arial",size=12),
      axis.title.y = element_text(color = "black", family = "Arial",size=12),
      legend.text = element_text(color = "black", family = "Arial",size=12),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.line.x.top = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black")
    ) 
  ggsave(nombre_grafico,gvar, width = 4, height = 4 )
  
  aov_var <- aov(Valor ~ Vigor + Dosis + Vigor:Dosis, 
                 data = datos_grafico)
  tbl_aov_var <- anova(aov_var)
  valoresp_aov_vigor[i,2] <- tbl_aov_var[1,5]
  valoresp_aov_dosis[i,2] <- tbl_aov_var[2,5]
  valoresp_aov_vigor_dosis[i,2] <- tbl_aov_var[3,5]
  
  tukey_vigor <- HSD.test(aov_var, "Vigor")
  tukey_dosis <- HSD.test(aov_var, "Dosis")
  
  grps_vigor <- tukey_vigor$groups
  grps_vigor <- grps_vigor |>
    rownames_to_column(var = "Vigor") |>
    select(-2) |>
    mutate(Variable = variables[i]) |>
    rename(
      Grupos = groups
    )
  
  grps_dosis <- tukey_dosis$groups
  grps_dosis <- grps_dosis |>
    rownames_to_column(var = "Dosis") |>
    select(-2) |>
    mutate(Variable = variables[i]) |>
    rename(
      Grupos = groups
    )
  
  grupos_vigor <- rbind(grupos_vigor,grps_vigor)
  grupos_dosis <- rbind(grupos_dosis,grps_dosis)
  
  var2 <- datos_grafico |>
    group_by(Vigor, Dosis) |>
    summarise(grupos = mean(Valor))
  nombre_grafico_interaccion <- paste0("graficos/interaccion_",variables[i],".bmp")
  
  grafico_int <- ggplot(var2, aes(x = Dosis, y = grupos, color = Vigor) ) + 
    geom_line(aes(group = Vigor)) +
    geom_point() +
    ylab(etiqueta_grafico) +
    xlab("Dosis") + theme_apa() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(color = "black", family = "Arial",size=12),
      axis.text.x = element_text(color = "black", family = "Arial",size=12),
      axis.title.y = element_text(color = "black", family = "Arial",size=12),
      legend.text = element_text(color = "black", family = "Arial",size=12),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.line.x.top = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black")
    )
  
  ggsave(nombre_grafico_interaccion,grafico_int,width = 4, height = 4)
  
}







# Estadísticas por Madurez y Componente -------------------------------

stats_vars_componente <- datos_largos |>
  group_by(Componente,  Variable) |>
  summarise(
    Media = mean(Valor), 
    Desv  = sd(Valor)
  )

stats_vars_componente <- left_join(stats_vars_componente, grupos_componente)

stats_vars_componente2 <- stats_vars_componente |>
  mutate(
    Valor = paste0(round(Media,2),"\U00B1",round(Desv,2)," ",Grupos)
  ) |>
  select(-c(3:5)) |>
  pivot_wider(
    names_from = "Variable",
    values_from = "Valor"
  )


stats_vars_madurez <- datos_largos |>
  group_by(Madurez,  Variable) |>
  summarise(
    Media = mean(Valor), 
    Desv  = sd(Valor)
  )

stats_vars_madurez <- left_join(stats_vars_madurez, grupos_madurez)

stats_vars_madurez2 <- stats_vars_madurez |>
  mutate(
    Valor = paste0(round(Media,2),"\U00B1",round(Desv,2)," ",Grupos)
  ) |>
  select(-c(3:5)) |>
  pivot_wider(
    names_from = "Variable",
    values_from = "Valor"
  )


write_xlsx(x = list("Componente" = stats_vars_componente2, 
                    "Madurez" = stats_vars_madurez2,
                    "Mad_Comp" = stats_vars,
                    "valsp_mad" = valoresp_aov_mad,
                    "valsp_comp" = valoresp_aov_comp,
                    "valsp_mad_comp" = valoresp_aov_mad_comp),
           "resultados/estadisticas.xlsx")




