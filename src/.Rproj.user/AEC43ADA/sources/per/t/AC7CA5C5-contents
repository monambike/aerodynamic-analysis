library(here)

# Carrega o arquivo de texto
asa_pf <- here("airfoil-plots/data/Asacoord.csv")
NACA_0012 <- here("airfoil-plots/data/coordempvert.csv")
WORTMANN <- here("airfoil-plots/data/emphorizcoord.csv")

# Função para limpar e estruturar as tabelas
extrair_e_limpar <- function(linhas_texto, inicio, fim) {
  bloco <- linhas_texto[inicio:fim]
  df <- read.csv(text = bloco, header = TRUE, stringsAsFactors = FALSE)
  
  colnames(df) <- c("X", "Y")
  df$X <- as.numeric(df$X)
  df$Y <- as.numeric(df$Y)
  
  return(na.omit(df))
}

airfoil_coordinates <- function(df) {
  linhas <- readLines(df)
  
  # Identifica onde começa cada seção dentro do CSV
  idx_surface <- grep("Airfoil surface", linhas)
  idx_camber  <- grep("Camber line", linhas)
  idx_chord   <- grep("Chord line", linhas)
  
  # Cria as tabelas na memória do R
  dados_surface <- extrair_e_limpar(linhas, idx_surface + 1, idx_camber - 1)
  dados_camber  <- extrair_e_limpar(linhas, idx_camber + 1, idx_chord - 1)
  dados_chord   <- extrair_e_limpar(linhas, idx_chord + 1, length(linhas))
  
  # 1. Junta os dados extraídos em uma única tabela
  dados_totais <- rbind(dados_surface, dados_camber, dados_chord)
  
  # 2. Calcula os limites dos eixos
  xlims <- range(dados_totais$X, na.rm = TRUE)
  ylims <- range(dados_totais$Y, na.rm = TRUE)
  
  # 3. Prepara a janela gráfica
  plot(NULL, xlim = xlims, ylim = ylims, asp = 1,
       xlab = "X (mm)", ylab = "Y (mm)", 
       main = "Perfil Geométrico do Aerofólio (Acauã 24)")
  
  # 4. Adiciona as linhas de grade de fundo
  grid() 
  
  # 5. Desenha cada uma das linhas do aerofólio
  lines(dados_surface$X, dados_surface$Y, col = "#1f77b4", lwd = 2, lty = 1)
  lines(dados_camber$X, dados_camber$Y, col = "#ff7f0e", lwd = 2, lty = 2)
  lines(dados_chord$X, dados_chord$Y, col = "#2ca02c", lwd = 2, lty = 3)
  
  # 6. Adiciona a legenda na parte inferior
  legend(
    "bottom",
    legend = c("Superfície", "L. de Arqueamento", "L. de Corda"),
    col = c("#1f77b4", "#ff7f0e", "#2ca02c"),
    lty = 1:3, lwd = 2, horiz = TRUE
  )
}

airfoil_coordinates(asa_pf)
airfoil_coordinates(NACA_0012)
airfoil_coordinates(WORTMANN)
