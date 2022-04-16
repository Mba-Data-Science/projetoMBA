pacotes <- c("tidyverse", "ggmap", "kableExtra", "sjPlot", "pivottabler", "cabootcrs", "FactoMineR",
             "plotly", "ggrepel")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

########################################################################
# Funções
########################################################################

tabelaContgenciaCNAE <- function(df, cnae01, cnae02) {
  rows <- df[paste0("cnae", cnae01)][, 1]
  cols <- df[paste0("cnae", cnae02)][, 1]
  titulo <- paste0("Tabela de Contigencia ente os CNAE ", cnae01, "x", cnae02)
  sjt.xtab(title = titulo,
           var.labels = c(cnae01, cnae02),
           var.row = rows,
           var.col = cols,
           show.exp = TRUE,
           show.row.prc = TRUE,
           show.col.prc = TRUE)
}

testeQui2 <- function(df, cnae01, cnae02) {
  rows <- df[paste0("cnae", cnae01)][, 1]
  cols <- df[paste0("cnae", cnae02)][, 1]
  table <- table(rows, cols)
  qui2 <- chisq.test(table)
  return(qui2)
}

tabela <- function(df) df %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = TRUE,
                font_size = 12)

########################################################################
# Programa
########################################################################

df.cnaes <- read.csv("cnae.txt", sep = ";")
df.cnaes %>% tabela()

df.estabelacimento.bruto <- read.csv("estabelecimentos.txt", sep = ";", dec = ",",
                                     colClasses = c("CNPJ_BASICO" = "character", "CNPJ_ORDEM" = "character", "CNPJ_DV" = "character",
                                                    "IDENTIFICADOR_MF" = "integer", "NOME_FANTASIA" = "character", "SITUACAO" = "integer",
                                                    "DATA_SITUACAO" = "character", "MOTIVO_SITUACAO" = "integer", "CIDADE_EXT" = "character",
                                                    "PAIS" = "integer", "DATA_ATIVIDADE" = "character", "CNAE_PRINCIPAL" = "integer",
                                                    "CNAE_SECUNDARIO" = "character", "TIPO_LOG" = "character", "LOGRADOURO" = "character",
                                                    "NUMERO" = "character", "COMPLEMENTO" = "character", "BAIRRO" = "character",
                                                    "CEP" = "character", "UF" = "factor", "MUNICIPIO" = "integer",
                                                    "DDD1" = "character", "TELEFONE1" = "character", "DDD2" = "character",
                                                    "TELEFONE2" = "character", "DDD_FAX" = "character", "FAX" = "character",
                                                    "EMAIL" = "character", "SITUACAO_ESPECIAL" = "character", "DATA_SITUACAO_ESPECIAL" = "integer",
                                                    "RAZAO_SOCIAL" = "character", "NATUREZA" = "integer", "QUALICACAO_RESPONSAVEL" = "integer",
                                                    "CAPITAL" = "numeric", "PORTE" = "integer", "ENTE_FED_RESPONSAVEL" = "character"
                                     ))


df.estabelacimento <- df.estabelacimento.bruto %>%
  transmute(
    cnpj = paste0(CNPJ_BASICO, CNPJ_ORDEM, CNPJ_DV),
    nome = NOME_FANTASIA,
    tipo = if_else(.$IDENTIFICADOR_MF == 1, 'MATRIZ',
                   if_else(.$IDENTIFICADOR_MF == 2, 'FILIAL', NULL)),
    situacao = if_else(.$SITUACAO == 1, "NULA",
                       if_else(.$SITUACAO == 2, "ATIVA",
                               if_else(.$SITUACAO == 3, "SUSPENSA",
                                       if_else(.$SITUACAO == 4, "INAPTA",
                                               if_else(.$SITUACAO == 8, "BAIXADA",
                                                       NULL))))),
    dataSituacao = .$DATA_SITUACAO %>% as.Date(format = "%Y%m%d"),
    dataAtividade = .$DATA_ATIVIDADE %>% as.Date(format = "%Y%m%d"),
    cnaePrincipal = .$CNAE_PRINCIPAL,
    cnaeSecundario = .$CNAE_SECUNDARIO %>% str_split(","),
    endereco = paste0(.$TIPO_LOG, " ", .$LOGRADOURO, ", ", .$COMPLEMENTO, " ", .$NUMERO, " - ",
                      .$BAIRRO, ", TERESINA - PI, "),
    cep = CEP,
    razaoSociao = RAZAO_SOCIAL,
    capital = CAPITAL,
    porte = if_else(.$PORTE == 0, "NÂO INFORMADO",
                    if_else(.$PORTE == 1, "MICRO EMPRESA",
                            if_else(.$PORTE == 3, "PEQUENO PORTE",
                                    if_else(.$PORTE == 5, "DEMAIS", NULL))))
  )

df.estabelacimento <- df.estabelacimento %>% add_column(
  cnaes = mapply(list, .$cnaePrincipal, .$cnaeSecundario, SIMPLIFY = F) %>% map(unlist)
)

df.estabelacimento$cnae4751201 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4751201 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4752100 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4752100 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4753900 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4753900 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4754701 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4754701 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4755503 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4755503 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4759899 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4759899 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4763601 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4763601 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4772500 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4772500 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4781400 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4781400 %in% .x, 'S', 'N')) %>%
  unlist()
df.estabelacimento$cnae4782201 <- df.estabelacimento$cnaes %>%
  map(~ifelse(4782201 %in% .x, 'S', 'N')) %>%
  unlist()

df.estabelacimento.cnae <- df.estabelacimento %>% select(cnpj,
                                                         cnae4751201,
                                                         cnae4752100,
                                                         cnae4753900,
                                                         cnae4754701,
                                                         cnae4755503,
                                                         cnae4759899,
                                                         cnae4763601,
                                                         cnae4772500,
                                                         cnae4781400,
                                                         cnae4782201)


tabela(df.estabelacimento.cnae)

tabelaContgenciaCNAE(df.estabelacimento.cnae, 4751201, 4755503)
tabelaContgenciaCNAE(df.estabelacimento.cnae, 4772500, 4772500)

cnaes <- c(4751201, 4752100, 4753900, 4754701, 4755503, 4759899, 4763601, 4772500, 4781400, 4782201)

df.cnaes %>% filter(CODIGO %in% cnaes) %>% tabela()

cnaes01 <- NULL
cnaes02 <- NULL
num01 <- NULL
num02 <- NULL
p.value <- NULL

for (i in seq_along(cnaes)) {
  for (j in 1:i) {
    if (i != j) {
      num01 <- c(num01, i)
      num02 <- c(num02, j)
      cnaes01 <- c(cnaes01, cnaes[i]) %>% as.character()
      cnaes02 <- c(cnaes02, cnaes[j]) %>% as.character()
      qui2 <- testeQui2(df.estabelacimento.cnae, cnaes[i], cnaes[j])
      p.value <- c(p.value, qui2$p.value)
    }
    print(paste(i, j))
  }
}

df.qui2 <- data.frame(num01, num02, cnaes01, cnaes02, p.value)

df.qui2 <- df.qui2[order(df.qui2$cnaes01, df.qui2$cnaes02),]

df.qui2 %>% tabela()

pt <- qpvt(df.qui2, "cnaes01", "cnaes02", "sum(p.value)",
           formats = list("%.4f"), totals = NULL)

pt$renderPivot()

matriz_binaria <- getindicator(Xinput = df.estabelacimento.cnae[, 2:11])
matriz_binaria

CA(matriz_binaria)

options(ggrepel.max.overlaps = Inf)
matriz_burt <- getBurt(Xinput = df.estabelacimento.cnae[, 2:11])
matriz_burt
CA(matriz_burt)

ACM <- MCA(df.estabelacimento.cnae[, 2:11], method = "Indicador")

cord <- ACM$var$coord

round(ACM$var$coord, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

categorias <- apply(df.estabelacimento.cnae[, 2:11],
                    MARGIN = 2,
                    FUN = function(x) nlevels(as.factor(x)))

categorias

It_explicada <- ACM$eig[, 1] / sum(ACM$eig[, 1])

data.frame(Dimensão = paste("Dimensão", seq_along(It_explicada)),
           Inércia_Total = It_explicada) %>%
  ggplot(aes(x = Dimensão,
             y = Inércia_Total,
             label = paste0(round(Inércia_Total, 3) * 100, "%"))) +
  geom_bar(stat = "identity",
           color = "#440154FF",
           fill = "#287C8EFF") +
  geom_label(vjust = 2) +
  labs(title = paste("Inércia Total Explicada de",
                     paste0(sum(It_explicada) * 100), "%")) +
  theme_bw()

############ MAPA ###########################################################

#1º Definir o número de categorias por variável
categorias <- apply(df.estabelacimento.cnae[, 2:11],
                    MARGIN = 2,
                    FUN = function(x) nlevels(as.factor(x)))

#2º transformar o objeto ACM em um data frame, levando-se em consideração quais
#tipos de coordenadas se quer plotar. Neste exemplo, utilizaremos as coordenadas
#dadas pela matriz de binária
ACM_mp <- data.frame(ACM$var$coord, Variável = rep(names(categorias), categorias))

ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("cnae4751201.", "", Categoria),
         Categoria = gsub("cnae4752100.", "", Categoria),
         Categoria = gsub("cnae4753900.", "", Categoria),
         Categoria = gsub("cnae4754701.", "", Categoria),
         Categoria = gsub("cnae4755503.", "", Categoria),
         Categoria = gsub("cnae4759899.", "", Categoria),
         Categoria = gsub("cnae4763601.", "", Categoria),
         Categoria = gsub("cnae4772500.", "", Categoria),
         Categoria = gsub("cnae4781400.", "", Categoria),
         Categoria = gsub("cnae4782201.", "", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)


#Plotando o Mapa Perceptual:
ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1,
             y = Dim.2,
             label = Categoria,
             color = Variável,
             shape = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[1, 2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[2, 2], 2), "%"))) +
  scale_color_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Plotando o Mapa Perceptual, evidenciando-se as coordenadas das observações:

#Criando um mapa perceptual mais elegante, com as posições relativas de cada
#observação

#1º Salvar as posições relativas de cada observação
ACM_observacoes_df <- data.frame(ACM$ind$coord)

#2º Utilizando a estrutura do mapa perceptual já estabelecido, vamos acrescentar
#as novas informações presentes no objeto mca_observacoes_df

ACM_observacoes_df %>%
  ggplot(aes(x = Dim.1, y = Dim.2, label = df.estabelacimento.cnae$cnpj)) +
  geom_point(shape = 17, color = "#E76F5AFF", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray80") +
  geom_label_repel(data = ACM_mp,
                   aes(x = Dim.1, y = Dim.2,
                       label = rownames(ACM_mp),
                       fill = Variável),
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[, 2][1], digits = 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[, 2][2], digits = 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#E como criar um mapa perceptual 3D de uma ACM?
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = ACM_mp[, 1],
                    y = ACM_mp[, 2],
                    z = ACM_mp[, 3],
                    mode = "text",
                    text = rownames(ACM_mp),
                    textfont = list(color = "#440154FF"),
                    showlegend = FALSE)

# Adicionando as labels das dimensões
ACM_3D <- layout(p = ACM_3D,
                 scene = list(xaxis = list(title = colnames(ACM_mp)[1]),
                              yaxis = list(title = colnames(ACM_mp)[2]),
                              zaxis = list(title = colnames(ACM_mp)[3]),
                              aspectmode = "data"))

ACM_3D
# Fim ---------------------------------------------------------------------
