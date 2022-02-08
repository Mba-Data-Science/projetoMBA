######################################################################################################################
# Carrega pacotes
######################################################################################################################

pacotes <- c("tidyverse", "kableExtra", "sjPlot", "pivottabler", "cabootcrs", "FactoMineR",
             "plotly", "ggrepel", "magick", "webshot", "basictabler", "flextable", "rbokeh",
             "factoextra", "gridExtra", "grid", "openxlsx")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#webshot::install_phantomjs(force = TRUE)

options(ggrepel.max.overlaps = Inf)

######################################################################################################################
# Funções
######################################################################################################################

tabelaContigencia <- function(df, var01, var02) {
  rows <- df[var01][, 1]
  cols <- df[var02][, 1]
  titulo <- paste0("Tabela de Contigencia ente as variáveis ", var01, " x ", var02)
  tab <- sjt.xtab(title = titulo,
                  var.labels = c(var01, var02),
                  var.row = rows,
                  var.col = cols,
                  show.exp = TRUE,
                  show.row.prc = TRUE,
                  show.col.prc = TRUE)
  return(tab)
}

testeQui2 <- function(df, variavel01, variavel02) {
  rows <- df[variavel01][, 1]
  cols <- df[variavel02][, 1]
  table <- table(rows, cols)
  qui2 <- chisq.test(table)
  return(qui2)
}

tabela <- function(df, caption = NULL) {

  tabelaPriv <- function(df, caption) df %>%
    kable(caption = caption) %>%
    kable_styling(bootstrap_options = "striped",
                  full_width = TRUE,
                  font_size = 12)

  if (caption %>% is.null()) {
    tabelaPriv(df, caption) %>%
      save_kable(file = paste0("files/", caption, ".png"))
  }

  caption <- if (caption %>% is.null()) "Titulo" else caption
  tabelaPriv(df, caption)
}

strTable <- function(df, caption = "") {
  data.frame(variable = names(df),
             classe = sapply(df, typeof),
             first_values = sapply(df, function(x) paste0(head(x), collapse = ", ")),
             row.names = NULL) %>%
    tabela(caption = caption)
}

######################################################################################################################
## Carrega tabela de CNAE
######################################################################################################################
cnaes.empresa <- c(4751201, 4752100, 4753900, 4754701, 4755503, 4759899, 4763601, 4772500, 4781400, 4782201)
df.cnaes <- read.csv("cnae.txt", sep = ";")
df.cnaes %>% strTable("Dataframe CNAE")

######################################################################################################################
## CNAE's da empresa
######################################################################################################################

df.cnaes %>%
  filter(CODIGO %in% cnaes.empresa) %>%
  tabela("CNAEs Empresa")

######################################################################################################################
## Carrega tabela com os estabelacimentos com a mesma cidade de Teresina e Timon
######################################################################################################################
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
  filter(SITUACAO == 2 | (DATA_SITUACAO >= "20170101" & SITUACAO %in% c(3, 8))) %>%
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
    endereco = paste0(.$TIPO_LOG, " ", .$LOGRADOURO, ", ", .$NUMERO, ", ",
                      .$BAIRRO),
    bairro = .$BAIRRO,
    municipio = if_else(.$MUNICIPIO == 937, "TIMON",
                        if_else(.$MUNICIPIO == 1219, "TERESINA", NULL)),
    uf = .$UF,
    cep = CEP,
    razaoSocial = RAZAO_SOCIAL,
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

df.estabelacimento.acm <- df.estabelacimento %>% select(cnpj,
                                                        porte,
                                                        cnae4751201,
                                                        cnae4752100,
                                                        cnae4753900,
                                                        cnae4754701,
                                                        cnae4755503,
                                                        cnae4759899,
                                                        cnae4763601,
                                                        cnae4772500,
                                                        cnae4781400,
                                                        cnae4782201
                                                        )


df.estabelacimento.acm %>% strTable(caption = "Data frame estabelecimento")

######################################################################################################################
## Salva o data frame df.estabelacimento
######################################################################################################################

df.estabelacimento %>%  saveRDS(file = "df.estabelacimento.rda")

######################################################################################################################
## Teste Qui2 dos pares de variáveis
######################################################################################################################

df.variaveis <- df.estabelacimento.acm[, 2:length(df.estabelacimento.acm)]

variaveis <- df.variaveis %>% colnames()

var01 <- NULL
var02 <- NULL
p.value <- NULL

for (i in seq_along(variaveis)) {
  for (j in seq_along(variaveis)) {
    var01 <- c(var01, variaveis[i])
    var02 <- c(var02, variaveis[j])
    qui2 <- testeQui2(df.estabelacimento.acm, variaveis[i], variaveis[j])
    p.value <- c(p.value, qui2$p.value)
  }
}

df.qui2 <- data.frame(var01, var02, p.value)

df.qui2.h0 <- df.qui2 %>% filter(p.value > 0.05)

pt <- qpvt(df.qui2, "var01", "var02",
           c("p.value" = "round(sum(p.value), 2)", "h1" = "sum(p.value > 0.05)"),
           formats = list(list(nsmall = 2, decimal.mark = ",", scientific = FALSE)))

cells <- pt$findCells(minValue = 0.05)
pt$setStyling(cells = cells, declarations = list("background-color" = "orange"))
pt$renderPivot()
tbl <- pt$asBasicTable()
ft <- tbl$asFlexTable()
save_as_image(x = ft, path = "files/qui2Teste.png")

######################################################################################################################
## ACM
######################################################################################################################


ACM <- MCA(df.variaveis, method = "Indicador")

df.cords <- data.frame(ACM$ind$coord)

######################################################################################################################
## Inercia total explicada
######################################################################################################################

categorias <- apply(df.variaveis,
                    MARGIN = 2,
                    FUN = function(x) nlevels(as.factor(x)))

categorias


It <- (sum(categorias) - length(categorias)) / length(categorias)
It

sum(ACM$eig[,1])

It_explicada <- ACM$eig[,1] / sum(ACM$eig[,1])

It_explicada

data.frame(Dimensão = paste("Dimensão", 1:length(It_explicada)),
           Inércia_Total = It_explicada) %>%
  ggplot(aes(x = Dimensão, 
             y = Inércia_Total, 
             label = paste0(round(Inércia_Total,3)*100,"%"))) +
  geom_bar(stat = "identity",
           color = "#440154FF", 
           fill = "#287C8EFF") +
  geom_label(vjust = 2) +
  labs(title = paste("Inércia Total Explicada de",
                     paste0(sum(It_explicada) * 100),"%")) +
  theme_bw()


######################################################################################################################
## CLUSTERIZAÇAO
######################################################################################################################

fviz_nbclust(df.cords, kmeans, method = "wss")


df.cords.k <- kmeans(df.cords, centers = 5)

v12 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.1", "Dim.2")) + ggtitle("")

v13 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.1", "Dim.3")) + ggtitle("")

v14 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.1", "Dim.4")) + ggtitle("")

v15 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.1", "Dim.5")) + ggtitle("")

v23 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.2", "Dim.3")) + ggtitle("")

v24 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.2", "Dim.4")) + ggtitle("")

v25 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.2", "Dim.5")) + ggtitle("")

v34 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.3", "Dim.4")) + ggtitle("")

v35 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.3", "Dim.5")) + ggtitle("")

v45 <- fviz_cluster(df.cords.k, geom = "point", data = df.cords, 
                    choose.vars = c("Dim.4", "Dim.5")) + ggtitle("")

nc <-  textGrob("")


arrange <- grid.arrange(v12, nc,  nc,  nc,
             v13, v23, nc,  nc,
             v14, v24, v34, nc,
             v15, v25, v35, v45,
             nrow = 4)


ggsave("files/clusterizacao.png", arrange, 
       scale = 5,
       width = 1280,
       height = 800,
       units = "px")


######################################################################################################################
##
######################################################################################################################

df.fit <- data.frame(cluster = df.cords.k$cluster)


df.estabelacimento.cluster <- cbind(df.fit, df.estabelacimento)

df.empresa <- df.estabelacimento.cluster %>% filter(razaoSocial == "PINTOS LTDA") 

df.empresa %>% tabela()

cluster.empresa <- df.empresa$cluster %>% unique()

df.estabelacimento.cluster %>%
  filter(cluster %in% cluster.empresa) %>%
  write.xlsx ("files/clusterEstabelecimentos.xlsx")



