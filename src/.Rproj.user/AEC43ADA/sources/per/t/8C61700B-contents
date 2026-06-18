# ==============================================================================
# CÓDIGO EM R (Nativo): Perfil WORTMANN FX M2 (Copie e cole tudo junto)
# ==============================================================================

# 1. Carregar as linhas do arquivo
WORTMANN <- here("airfoil-plots/data/emphorizcoord.csv")
linhas <- readLines(WORTMANN)

# 2. Identificar metadados e marcadores das seções
nome_perfil <- trimws(strsplit(linhas[1], ",")[[1]][2])
idx_surface <- grep("Airfoil surface", linhas)[1]
idx_camber  <- grep("Camber line", linhas)[1]
idx_chord   <- grep("Chord line", linhas)[1]

# 3. Função para limpar os blocos de dados
get_df <- function(linhas_texto, inicio, fim) {
  bloco <- linhas_texto[inicio:fim]
  df <- read.csv(
    text = bloco, skip = 1, header = FALSE,
    col.names = c("X", "Y"), stringsAsFactors = FALSE)
  df$X <- as.numeric(df$X)
  df$Y <- as.numeric(df$Y)
  return(na.omit(df))
}

# 4. Extração das coordenadas de interesse
dados_surface <- get_df(linhas, idx_surface, idx_camber - 1)
dados_camber  <- get_df(linhas, idx_camber, idx_chord - 1)
dados_chord   <- get_df(linhas, idx_chord, length(linhas))
dados_totais  <- rbind(dados_surface, dados_camber, dados_chord)

# 5. Calcular dinamicamente os limites exatos do gráfico
xlims <- range(dados_totais$X, na.rm = TRUE)
ylims <- range(dados_totais$Y, na.rm = TRUE)

# 6. Inicializar o plano cartesiano (asp = 1 garante a proporção real 1:1)
plot(NULL, xlim = xlims, ylim = ylims, asp = 1,
     xlab = "X (mm)", ylab = "Y (mm)", 
     main = paste("Perfil Geométrico do Aerofólio: WORTMANN FX M2"))
grid() 

# 7. Desenhar as curvas sobre a tela inicializada
lines(dados_surface$X, dados_surface$Y, col = "#1f77b4", lwd = 2, lty = 1)
lines(dados_camber$X, dados_camber$Y, col = "#ff7f0e", lwd = 2, lty = 2)
lines(dados_chord$X, dados_chord$Y, col = "#2ca02c", lwd = 2, lty = 3)

# 8. Adicionar a legenda explicativa
legend("bottom", legend = c("Superfície", "Camber Line", "Chord Line"),
       col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lty = 1:3, lwd = 2, horiz = TRUE)