
library(tidyverse)
library(plotly)

df_asa <- read_csv("asa_certinho_mesmo.txt", show_col_types = FALSE) %>% 
  mutate(Perfil = "Asa")
df_eh  <- read_csv("emp_horizontal.txt", show_col_types = FALSE) %>% mutate(Perfil = "Emp. Horizontal")
df_ev  <- read_csv("emp_vert.txt", show_col_types = FALSE) %>% mutate(Perfil = "Emp. Vertical")

# Juntando tudo num único Dataframe 2D
df_2d <- bind_rows(df_asa, df_eh, df_ev) %>%
  rename_with(~str_trim(.)) %>% # Remove espaços em branco dos nomes das colunas
  mutate(cl_cd = CL / CD)       # Calculando a eficiência

df_3d <- Resultados_Aviao_3d %>%
  rename_with(~str_trim(.)) %>% 
  mutate(cl_cd = CL / CD)

destacar_origem <- list(
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, alpha = 0.5),
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5, alpha = 0.5)
)

escala_alpha <- scale_x_continuous(breaks = seq(-20, 20, by = 2))

ponto_cl_max <- df_2d %>% group_by(Perfil) %>% filter(CL == max(CL)) %>% slice(1) %>% ungroup()

g1 <- ggplot(df_2d, aes(x = alpha, y = CL, color = Perfil)) +
  destacar_origem +
  escala_alpha +
  scale_y_continuous(n.breaks = 15) +
  geom_line(linewidth = 0.3) +
  geom_point(aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCl: %.3f", Perfil, alpha, CL)), 
             size = 1, alpha = 0.7, show.legend = FALSE) +
  geom_point(data = ponto_cl_max, 
             aes(color = Perfil, text = sprintf("MÁXIMO Cl (%s)\nAlpha: %.1fº\nCl: %.3f", Perfil, alpha, CL)), 
             size = 3.5, shape = 21, fill = "gold", stroke = 1.5, show.legend = FALSE) +
  labs(title = "2D: Coef. de Sustentação (Cl x α)", x = "Alpha (º)", y = "Cl") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set1")

p_cl_alpha <- ggplotly(g1, tooltip = "text", width = 700, height = 800)


# Gráfico 2: Cd x Alpha 
ponto_cd_min <- df_2d %>% group_by(Perfil) %>% filter(CD == min(CD)) %>% slice(1) %>% ungroup()

g2 <- ggplot(df_2d, aes(x = alpha, y = CD, color = Perfil)) +
  destacar_origem +
  escala_alpha +
  scale_y_continuous(n.breaks = 15) +
  geom_line(linewidth = 0.5) +
  geom_point(aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCd: %.4f", Perfil, alpha, CD)), 
             size = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_point(data = ponto_cd_min, aes(text = sprintf("MÍNIMO Cd (%s)\nAlpha: %.1fº\nCd: %.4f",
              Perfil, alpha, CD)), 
                 size = 3.5, shape = 21, fill = "cyan", color = "black", stroke = 1, show.legend = FALSE) +
  labs(title = "2D: Coef. de Arrasto (Cd x α)", x = "Alpha (º)", y = "Cd") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set1")

p_cd_alpha <- ggplotly(g2, tooltip = "text", width = 700, height = 800)


# Gráfico 3: Cl/Cd x Alpha
ponto_efic_max <- df_2d %>% group_by(Perfil) %>% filter(cl_cd == max(cl_cd))%>%
  slice(1) %>% ungroup()

g3 <- ggplot(df_2d, aes(x = alpha, y = cl_cd, color = Perfil)) +
  destacar_origem +
  escala_alpha +
  scale_y_continuous(n.breaks = 15) +
  geom_line(linewidth = 0.5) +
  geom_point(aes(text = sprintf("Perfil: %s\nAlpha: %.1fº\nCl/Cd: %.1f", Perfil, alpha, cl_cd)), size = 1.5, alpha = 0.7) +
  # Destacando máxima eficiência (Verde com borda preta)
  geom_point(data = ponto_efic_max, aes(text = sprintf("MÁXIMA EFICIÊNCIA (%s)\nAlpha: %.1fº\nCl/Cd: %.1f", 
                                                       Perfil, alpha, cl_cd)), 
             size = 3.5, shape = 21, fill = "springgreen", color = "black", 
             stroke = 1, show.legend = FALSE) +
  labs(title = "2D: Eficiência (Cl/Cd x α)", x = "Alpha (º)", y = "Cl/Cd") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set1")

p_clcd_alpha <- ggplotly(g3, tooltip = "text", width = 700, height = 800)


# GRÁFICOS 3D (Aeronave Completa)

# Gráfico 4: 3D Cl x Alpha
g4 <- ggplot(df_3d, aes(x = alpha, y = CL)) +
  destacar_origem +
  escala_alpha +
  scale_y_continuous(n.breaks = 15) +
  geom_line(color = "purple", linewidth = 0.3) +
  geom_point(aes(text = sprintf("Alpha: %.1fº\nCL 3D: %.3f", alpha, CL)), 
             color = "purple", size = 1, , show.legend = FALSE) +
  labs(
    title = "3D: Sustentação (CL x α)", 
    subtitle = "Limitações computacionais (X5): Curva tendendo a linear",
    x = "Alpha (º)", 
    y = "CL 3D"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p_3d_cl_alpha <- ggplotly(g4, tooltip = "text", width = 700, height = 800)


# Gráfico 5: Polar de Arrasto 3D (CD x CL)
ponto_polar_melhor <- df_3d %>% filter(cl_cd == max(cl_cd)) %>% slice(1)

g5 <- ggplot(df_3d, aes(x = CD, y = CL)) +
  destacar_origem +
  scale_x_continuous(n.breaks = 12) + # No Polar o Eixo X é CD, então quebramos por intervalos dinâmicos
  scale_y_continuous(n.breaks = 15) +
  geom_path(color = "firebrick", linewidth = 0.5) + 
  geom_point(
    aes(text = sprintf("CD: %.4f\nCL: %.3f\nCl/Cd: %.1f", CD, CL, cl_cd)), 
             color = "firebrick", size = 1.5) +
  # Destacando ponto ótimo 3D
  geom_point(data = ponto_polar_melhor, aes(text = sprintf("MELHOR RAZÃO (CL/CD)\nCD: %.4f\nCL: %.3f\nCL/CD: %.4f\n", CD, CL, CL/CD)), 
             size = 4, shape = 21, fill = "gold", color = "black", stroke = 1) +
  labs(title = "3D: Polar de Arrasto (CD x CL)", x = "CD (Arrasto)", y = "CL (Sustentação)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p_polar_3d <- ggplotly(g5, tooltip = "text", width = 700, height = 800)

# Pra plotar os graficos só escrever o respectivo nome dele, tá definido em baixo do código dos gráficos, no console
