library(tidyverse)

asa_certo_tabela <- read_csv("Asa certo.txt")
emp_horizontal <- read_csv("emp_horizontal.txt")
emp_vertical <- read_csv("emp_vert.txt")
aviao_3d <- read_csv("Resultados Aviao 3d.txt")

aviao_3d %>%
  ggplot(aes(x = CD, y = CL)) +
  geom_point() +
  geom_path() +
  theme_classic() +
  labs(
    title = "CL x CD",
    x = "CD",
    y = "CL"
  )

asa_certo_tabela <- asa_certo_tabela %>%
  mutate(CLmax = CL[alpha == 15])

asa_certo_tabela %>%
  ggplot(aes(x = alpha, y = CL)) +
  geom_point() +
  geom_path() +
  theme_classic() +
  labs(
    title = "CL x Alpha",
    x = "Alpha",
    y = "CL"
  )
