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
  etiqueta <- paste0(dicc_vars[i,2])
  g2 <- ggplot(df, aes(x = Medición, y = Media, col = Dosis)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = unique(df$Medición)) +
    facet_wrap(. ~ Vigor, nrow = 1) +
    labs(x = "Medición", y = etiqueta) + 
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
  Variable = NA,
  valorp = NA,
  Medición = NA
)

valoresp_aov_dosis <- data.frame(
  Variable = NA,
  valorp = NA,
  Medición = NA
)

valoresp_aov_vigor_dosis <- data.frame(
  Variable = NA,
  valorp = NA,
  Medición = NA
)

grupos_vigor <- data.frame(
  Medición = NA,
  Variable = NA,
  Vigor = NA,
  Grupos = NA
)

grupos_dosis <- data.frame(
  Medición = NA,
  Variable = NA,
  Dosis = NA,
  Grupos = NA
)
mediciones <- unique(datos$Medición)
for(j in seq_along(mediciones)){
  for(i in seq_along(variables)){
    
    datos_grafico <- datos_largos |>
      filter(Variable == variables[i] & Medición == mediciones[j])
    variable_lab = dicc_vars[i,2]
    etiqueta_grafico = paste(variable_lab)
    
    nombre_grafico = paste0("graficos/",mediciones[j],"_",variables[i],".png")
    
    gvar <- ggplot(datos_grafico, aes(x = Dosis, y = Valor,  fill = Vigor)) +
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
    valsp_aov_vigor <- data.frame(
      Medición = mediciones[j],
      Variable = variables[i],
      valorp =  tbl_aov_var[1,5]
    )
    valsp_aov_dosis <- data.frame(
      Medición = mediciones[j],
      Variable = variables[i],
      valorp =  tbl_aov_var[2,5]
    )
    valsp_aov_vigor_dosis <- data.frame(
      Medición = mediciones[j],
      Variable = variables[i],
      valorp =  tbl_aov_var[3,5]
    )
    
    valoresp_aov_dosis <- bind_rows(valoresp_aov_dosis, valsp_aov_dosis)
    valoresp_aov_vigor <- bind_rows(valoresp_aov_vigor, valsp_aov_vigor)
    valoresp_aov_vigor_dosis <- bind_rows(valoresp_aov_vigor_dosis, valsp_aov_vigor_dosis)
    
    tukey_vigor <- HSD.test(aov_var, "Vigor")
    tukey_dosis <- HSD.test(aov_var, "Dosis")
    
    grps_vigor <- tukey_vigor$groups
    grps_vigor <- grps_vigor |>
      rownames_to_column(var = "Vigor") |>
      select(-2) |>
      mutate(
        Medición = mediciones[j],
        Variable = variables[i]) |>
      rename(
        Grupos = groups
      )
    
    grps_dosis <- tukey_dosis$groups
    grps_dosis <- grps_dosis |>
      rownames_to_column(var = "Dosis") |>
      select(-2) |>
      mutate(
        Medición = mediciones[j],
        Variable = variables[i]) |>
      rename(
        Grupos = groups
      )
    
    grupos_vigor <- rbind(grupos_vigor,grps_vigor)
    grupos_dosis <- rbind(grupos_dosis,grps_dosis)
    
    var2 <- datos_grafico |>
      group_by(Vigor, Dosis) |>
      summarise(grupos = mean(Valor))
    nombre_grafico_interaccion <- paste0("graficos/interaccion_",mediciones[j],"_",variables[i],".bmp")
    
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
}






# Estadísticas por Vigor y Dosis -------------------------------


stats_vars_dosis <- left_join(stats_vars_dosis, grupos_dosis)

stats_vars_dosis2 <- stats_vars_dosis |>
  mutate(
    Valor = paste0(round(Media,2),"\U00B1",round(Desv,2)," ",Grupos)
  ) |>
  select(-c(4:6)) |>
  pivot_wider(
    names_from = "Dosis",
    values_from = "Valor"
  )



stats_vars_vigor <- left_join(stats_vars_vigor, grupos_vigor)

stats_vars_vigor2 <- stats_vars_vigor |>
  mutate(
    Valor = paste0(round(Media,2),"\U00B1",round(Desv,2)," ",Grupos)
  ) |>
  select(-c(4:6)) |>
  pivot_wider(
    names_from = "Vigor",
    values_from = "Valor"
  )


write_xlsx(x = list("Dosis" = stats_vars_dosis2, 
                    "Vigor" = stats_vars_vigor2,
                    "Dosis_Vigor" = stats_vars,
                    "valsp_Dosis" = valoresp_aov_dosis,
                    "valsp_Vigor" = valoresp_aov_vigor,
                    "valsp_Vigor_Dosis" = valoresp_aov_vigor_dosis),
           "resultados/estadisticas.xlsx")




