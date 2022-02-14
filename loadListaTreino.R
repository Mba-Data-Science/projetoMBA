######################################################################################################################
# Carrega pacotes
######################################################################################################################

pacotes <- c("readr", "readxl", "plotly", "tidyverse", "gridExtra", "forecast", "TTR",
             "smooth", "tsibble", "fable", "tsibbledata", "fpp3", "lubridate",
             "urca", "dygraphs", "quantmod", "BETS", "tseries", "FinTS",
             "gridExtra", "scales", "caret", "xtable", "tsutils", "GetBCBData",
             "quantmod", "dgof", "seasonal", "R6", "coro")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


lista = readRDS("lista.treino.RData")

lista[order(lista$acuracia),] %>% head()


lista[order(lista$start.treino, decreasing=TRUE), ] %>% head()
