# **************************************************************************
# Copyright(c) 2026 UFV - Universidade Federal de Viçosa. All rights reserved.
# Project from: Vinícius Gabriel, Luiz Lopes, Davi Atayde, Pedro Balduino.
# For license information, please see the LICENSE file in the root directory.
# **************************************************************************/


library(here)
library(plotly)
library(tidyverse)

source(here("R/aircraft_data.R"))


LINE_WIDTH = 0.5

POINT_SIZE = 1.5
POINT_ALPHA = 0.7

HIGHLIGHT_POINT_SIZE = 3.5
HIGHLIGHT_POINT_STROKE = 1.5
HIGHLIGHT_POINT_SHAPE = 21


csv_airfoil_data_to_dataframe <- function(df, airfoil_name) {
  df %>% mutate(Perfil = airfoil_name) %>%
    rename_with(~str_trim(.)) %>% # Remove espaços em branco dos nomes das colunas
    mutate(cl_cd = CL / CD)       # Calculando a eficiência
}


df_asa <- csv_airfoil_data_to_dataframe(aircraft_data$airfoil_wing, "Asa")
df_eh  <- csv_airfoil_data_to_dataframe(aircraft_data$airfoil_horizontal_stabilizer, "Emp. Horizontal")
df_ev  <- csv_airfoil_data_to_dataframe(aircraft_data$airfoil_vertical_stabilizer, "Emp. Vertical")
df_aviao <- csv_airfoil_data_to_dataframe(aircraft_data$aircraft, "Aeronave")
# Juntando tudo num único Dataframe
df_2d <- bind_rows(df_asa, df_eh, df_ev)
df_3d <- df_aviao

destacar_origem <- list(
  geom_hline(yintercept = 0, color = "#111111", linewidth = 0.5, alpha = 0.5),
  geom_vline(xintercept = 0, color = "#111111", linewidth = 0.5, alpha = 0.5)
)

escala_alpha <- scale_x_continuous(breaks = seq(-20, 20, by = 2))
ponto_cl_max <- df_2d %>% group_by(Perfil) %>% filter(CL == max(CL)) %>% slice(1) %>% ungroup()

custom_colors <- c(
  "Asa" = "#E41A1C",
  "Emp. Horizontal" = "#377EB8",
  "Emp. Vertical" = "#4DAF4A"
)

airfoil_theme <- function(plot_object) {
  list(
    theme_minimal(),
    theme(panel.grid.minor = element_blank())
  )
}

# 2D: Coef. de Sustentação Cl x α
plot_airfoil_cl_alpha <- function(df) {
  ggplot(df, aes(x = alpha, y = CL, color = Perfil)) +
    destacar_origem +
    escala_alpha +
    scale_y_continuous(n.breaks = 15) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_point(
      aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCl: %.3f", Perfil, alpha, CL)), 
      size = POINT_SIZE, alpha = POINT_ALPHA, show.legend = FALSE
    ) +
    geom_point(
      data = ponto_cl_max, 
      aes(color = Perfil, text = sprintf("MÁXIMO Cl (%s)\nAlpha: %.1fº\nCl: %.3f", Perfil, alpha, CL)), 
      size = HIGHLIGHT_POINT_SIZE, shape = HIGHLIGHT_POINT_SHAPE, stroke = HIGHLIGHT_POINT_STROKE, show.legend = FALSE,
      fill = "gold", color = "#111111"
    ) +
    labs(
      title = "2D: Coef. de Sustentação (Cl x α)",
      x = "Alpha (º)", y = "Cl"
    ) +
    scale_color_manual(values = custom_colors) +
    airfoil_theme()
}
# p_cl_alpha <- ggplotly(g1, tooltip = "text", width = 700, height = 800)


# Gráfico 2: Cd x Alpha 
ponto_cd_min <- df_2d %>% group_by(Perfil) %>% filter(CD == min(CD)) %>% slice(1) %>% ungroup()
# 2D: Coef. de Arrasto Cl x α
plot_airfoil_cd_alpha <- function(df) {
  ggplot(df, aes(x = alpha, y = CD, color = Perfil)) +
    destacar_origem +
    escala_alpha +
    scale_y_continuous(n.breaks = 15) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_point(
      aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCd: %.4f", Perfil, alpha, CD)), 
      size = POINT_SIZE, alpha = POINT_ALPHA, show.legend = FALSE
    ) +
    geom_point(
      data = ponto_cd_min,
      aes(text = sprintf("MÍNIMO Cd (%s)\nAlpha: %.1fº\nCd: %.4f", Perfil, alpha, CD)), 
      size = HIGHLIGHT_POINT_SIZE, shape = HIGHLIGHT_POINT_SHAPE, stroke = HIGHLIGHT_POINT_STROKE, show.legend = FALSE,
      fill = "cyan", color = "#111111"
    ) +
    labs(
      title = "2D: Coef. de Arrasto (Cd x α)",
      x = "Alpha (º)", y = "Cd"
    ) +
    scale_color_manual(values = custom_colors) +
    airfoil_theme()
}
# p_cd_alpha <- ggplotly(g2, tooltip = "text", width = 700, height = 800)


# Gráfico 3: Cl/Cd x Alpha
ponto_efic_max <- df_2d %>% group_by(Perfil) %>% filter(cl_cd == max(cl_cd))%>% slice(1) %>% ungroup()
# 2D: Eficiência Cl/Cd x α
plot_airfoil_clcd_alpha <- function(df) {
  ggplot(df, aes(x = alpha, y = cl_cd, color = Perfil)) +
    destacar_origem +
    escala_alpha +
    scale_y_continuous(n.breaks = 15) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_point(
      aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCl/Cd: %.1f", Perfil, alpha, cl_cd)),
      size = POINT_SIZE, alpha = POINT_ALPHA, show.legend = FALSE
    ) +
    # Destacando máxima eficiência (Verde com borda preta)
    geom_point(
      data = ponto_efic_max,
      aes(text = sprintf("MÁXIMA EFICIÊNCIA (%s)\nAlpha: %.1fº\nCl/Cd: %.1f", Perfil, alpha, cl_cd)), 
      size = HIGHLIGHT_POINT_SIZE, shape = HIGHLIGHT_POINT_SHAPE, stroke = HIGHLIGHT_POINT_STROKE, show.legend = FALSE,
      fill = "springgreen", color = "#111111"
    ) +
    labs(
      title = "2D: Eficiência (Cl/Cd x α)",
      x = "Alpha (º)", y = "Cl/Cd"
    ) +
    scale_color_manual(values = custom_colors) +
    airfoil_theme()
}
# p_clcd_alpha <- ggplotly(g3, tooltip = "text", width = 700, height = 800)


# GRÁFICOS 3D (Aeronave Completa)
# Gráfico 4: 3D Cl x Alpha
# 3D: Sustentação CL x α
plot_aircraft_cl_alpha <- function(df) {
  ggplot(df, aes(x = alpha, y = CL)) +
    destacar_origem +
    escala_alpha +
    scale_y_continuous(n.breaks = 15) +
    geom_line(linewidth = LINE_WIDTH, color = "purple") +
    geom_point(
      aes(text = sprintf("Alpha: %.1fº\nCL 3D: %.3f", alpha, CL)), 
      size = POINT_SIZE, show.legend = FALSE,
      color = "purple"
    ) +
    labs(
      title = "3D: Sustentação (CL x α)", 
      subtitle = "Limitações computacionais (X5): Curva tendendo a linear",
      x = "Alpha (º)", y = "CL 3D"
    ) +
    airfoil_theme()
}
# p_3d_cl_alpha <- ggplotly(g4, tooltip = "text", width = 700, height = 800)


# Gráfico 5: Polar de Arrasto 3D (CD x CL)
ponto_polar_melhor <- df_3d %>% filter(cl_cd == max(cl_cd)) %>% slice(1)
# 3D: Polar de Arrasto CL x CD
plot_aircraft_cl_cd <- function(df) {
  ggplot(df, aes(x = CD, y = CL)) +
    destacar_origem +
    scale_x_continuous(n.breaks = 12) + # No Polar o Eixo X é CD, então quebramos por intervalos dinâmicos
    scale_y_continuous(n.breaks = 15) +
    geom_path(linewidth = LINE_WIDTH, color = "firebrick") + 
    geom_point(
      aes(text = sprintf("CD: %.4f\nCL: %.3f\nCl/Cd: %.1f", CD, CL, cl_cd)), 
      size = POINT_SIZE,
      color = "firebrick"
    ) +
    # Destacando ponto ótimo 3D
    geom_point(
      data = ponto_polar_melhor,
      aes(
        text = sprintf("MELHOR RAZÃO (CL/CD)\nCD: %.4f\nCL: %.3f\nCL/CD: %.4f\n", CD, CL, CL/CD)), 
        size = HIGHLIGHT_POINT_SIZE, stroke = HIGHLIGHT_POINT_STROKE, shape = HIGHLIGHT_POINT_SHAPE,
        fill = "gold", color = "#111111"
    ) +
    labs(
      title = "3D: Polar de Arrasto (CD x CL)",
      x = "CD (Arrasto)", y = "CL (Sustentação)"
    ) +
    airfoil_theme()
}
