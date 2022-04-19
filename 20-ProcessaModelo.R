################################################################################
# Carrega pacotes
################################################################################
options(digits = 4, scipen = 30)

library(tidyverse)
library(forecast)
library(urca)
library(zoo)
library(FinTS)
library(foreach)
library(future)

################################################################################
# Funções
################################################################################

#' @title transforma um vetor no formato c(ano, mes) em string no formato 
#' "mes/ano"
formatCYM <- function(cym) {
  return(paste0(cym[2], '/', cym[1]))
}

#' @title transforma um vetor no formato "mes-ano" em string no formato
#' c(ano, mes)
formatSYM <- function(sym, pat = "-") {
  return(sym %>%
           str_split(pat) %>%
           unlist() %>%
           as.numeric())
}

################################################################################
# Processamento
################################################################################

# Ler o data frame com as séries temporais de todos os registros de vendas
vendasGrupo <- readRDS("vendas.grupo.rds") %>%
  filter(data >= yearmonth('2009-01') &
           data <= yearmonth('2021-12'))

treinoStart <- c(2009, 1)
treinoEnd <- c(2020, 2)
testeStart <- c(2020, 6) # Variável
testeEnd <- c(2021, 12)
isolamentoStart <- treinoEnd
isolamentoEnd <- testeStart

lojaGrupos <- tibble(loja = vendasGrupo$loja, grupo = vendasGrupo$grupo) %>%
  unique() %>%
  arrange(loja, grupo)

ID <- 0

listaTreino <- foreach(row = seq_len(nrow(lojaGrupos)), .combine = 'rbind') %do% {
  dfRow <- lojaGrupos[row,]
  print(row)
  print(dfRow)
  grupoPar <- dfRow$grupo
  lojaPar <- dfRow$loja

  serieVenda <- vendasGrupo %>%
    filter(grupo == grupoPar &
             loja == lojaPar) %>%
    transmute(data,
              quant = quant.venda,
              valor = valor.venda) %>%
    arrange(data)

  vendasQuantTs <- ts(serieVenda$quant, start = treinoStart, end = testeEnd, frequency = 12)
  vendasQuantTreino <- window(vendasQuantTs, start = treinoStart, end = treinoEnd)
  vendasValorTs <- ts(serieVenda$valor, start = treinoStart, end = testeEnd, frequency = 12)
  vendasValorTreino <- window(vendasValorTs, start = treinoStart, end = treinoEnd)
  Lambda <- BoxCox.lambda(vendasQuantTreino)

  ID <- ID + 1

  list(
    id = ID,
    grupoPar = grupoPar,
    lojaPar = lojaPar,
    vendasQuantTs = vendasQuantTs,
    vendasQuantTreino = vendasQuantTreino,
    vendasValorTs = vendasValorTs,
    vendasValorTreino = vendasValorTreino,
    arima = auto.arima(vendasQuantTreino,
                       lambda = Lambda,
                       allowdrift = TRUE,
                       allowmean = TRUE,
                       approximation = FALSE,
                       stepwise = FALSE)
  )
}

warnings()

saveRDS(listaTreino, "lista.treino.rds")

