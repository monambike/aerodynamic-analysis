# ==============================================================================
# SCRIPT UNIVERSAL: Gráfico com R Nativo (Copie e cole tudo junto)
# ==============================================================================

# 1. Defina o arquivo que deseja plotar aqui
NACA_0012 <- here("airfoil-plots/data/coordempvert.csv")
linhas <- readLines(NACA_0012)

# 2. Descobre o nome do perfil e os índices das seções
nome_perfil <- trimws(strsplit(linhas[1], ",")[[1]][2])
idx_surface <- grep("Airfoil surface", linhas)
idx_camber  <- grep("Camber line", linhas)
idx_chord   <- grep("Chord line", whites <- linhas)

# 3. Função para limpar os dados
extrair_e_limpar <- function(linhas_texto, inicio, fim) {
  bloco <- linhas_texto[inicio:fim]
  df <- read.csv(text = bloco, header = TRUE, stringsAsFactors = FALSE)
  colnames(df) <- c("X", "Y")
  df$X <- as.numeric(df$X)
  df$Y <- as.numeric(df$Y)
  return(na.omit(df))
}

# 4. Extração dos blocos de dados
dados_surface <- extrair_e_limpar(linhas, idx_surface + 1, idx_camber - 1)
dados_camber  <- extrair_e_limpar(linhas, idx_camber + 1, idx_chord - 1)
dados_chord   <- extrair_e_limpar(linhas, idx_chord + 1, length(linhas))
dados_totais  <- rbind(dados_surface, dados_camber, dados_chord)

# 5. Calcula os novos limites de eixos baseados neste arquivo específico
xlims <- range(dados_totais$X, na.rm = TRUE)
ylims <- range(dados_totais$Y, na.rm = TRUE)

# 6. Prepara e abre a nova janela gráfica (asp = 1 impede distorções)
plot(NULL, xlim = xlims, ylim = ylims, asp = 1,
     xlab = "X (mm)", ylab = "Y (mm)", 
     main = paste("Perfil Geométrico do Aerofólio: NACA 0012"))
grid() 

# 7. Desenha as curvas correspondentes
lines(dados_surface$X, dados_surface$Y, col = "#1f77b4", lwd = 2, lty = 1)
lines(dados_camber$X, dados_camber$Y, col = "#ff7f0e", lwd = 2, lty = 2)
lines(dados_chord$X, dados_chord$Y, col = "#2ca02c", lwd = 2, lty = 3)

# 8. Cria a legenda correspondente
legend("bottom", legend = c("Superfície", "Camber Line", "Chord Line"),
       col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1:3, lwd = 2, horiz = TRUE)