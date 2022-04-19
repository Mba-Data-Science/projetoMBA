################################################################################
# Carrega pacotes
################################################################################
options(digits = 4, scipen = 30)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(tsibble)
library(lubridate)
library(numform)
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

#' @title Retorna uma string com a interpletação do teste dickey Fuller
getInterpleracaoDickeyFuller <- function(treino.df) {
  result <- if (is.null(treino.df)) { "" }
  else {
    treino.df.sum <- summary(treino.df)
    tau           <- treino.df.sum@teststat[1, 1]
    cval          <- treino.df.sum@cval[1, "5pct"]
    if (is.na(tau) | is.na(cval)) { "" }
    else
      if (abs(tau) < abs(cval)) { "H0: A série não é Estacionária (95% de confiaça)" }
      else { "H1: A série é Estacionária (95% de confiaça)" }
  }
  return(result)
}

  #' @title Retorna a descrição do modelo de previsão arima
getDescicaoModelo <- function(arima) {
  a <- arima$arma
  return(paste0("ARIMA(", a[1], ",", a[6], ",", a[2], ")(",
                a[3], ",", a[7], ",", a[4], ")[", a[5], "]"))
}

  #' @title Retorna uma string com a interpletação do teste LjungBox
getInterpletacaoLjungBox <- function(testeLjungBox) {
  p.value <- formatC(testeLjungBox$p.value, format = "f", digits = 2)
  return(
    if (p.value > 0.05) {
      paste0("H0: Os resíudos não são correlacionados (", p.value, " > 0.05)")
    } else {
      paste0("H1: Os resíudos são correlacionados (", p.value, " < 0.05)")
    }
  )
}

  #' @title Retorna uma string com a interpletação do teste Kolmogorov Smirnov
getInterpletacaoKolmogorovSmirnov <- function(testeKolmogorovSmirnov) {
  teste   <- testeKolmogorovSmirnov
  p.value <- if (is.null(teste))  NA
  else
    formatC(teste$p.value, format = "f", digits = 2)

  return(
    if (is.na(p.value)) { NA }
    else
      if (p.value > 0.05) {
        paste0("H0: Residuos são normais (", p.value, " > 0.05)")
      }else {
        paste0("H1: Residuos não são normais (", p.value, " < 0.05)")
      }
  )
}

  #' @title Retorna uma string com a interpletação do teste Arch
getInterpletacaoArch <- function(testeArch) {
  p.value <- formatC(testeArch$p.value, format = "f", digits = 2)
  return(
    if (p.value > 0.05) {
      paste0("H0: Garante a não existencia de efeitos Arch (", p.value, " > 0.05)")
    }else {
      paste0("H1: Existe Efeito Arch (", p.value, " < 0.05)")
    }
  )
}

  #' @title Retorna SIM OU NÃO se o modelo foi aceito
getModeloAceito <- function(testeArch, testeKolmogorovSmirnov, testeLjungBox) {
  p.value.Arch <- testeArch$p.value
  p.value.KS   <- testeKolmogorovSmirnov$p.value
  p.value.LB   <- testeLjungBox$p.value
  return(
    if (is.null(p.value.Arch) |
      is.null(p.value.KS) |
      is.null(p.value.LB))
      "NÃO"
    else
      if (p.value.Arch > 0.05 &
        p.value.KS > 0.05 &
        p.value.LB > 0.05)
        "SIM"
      else
        "NÃO"
  )
}

#' @title produz o gráfico com o resultado da previsão
plotPrevisao <- function(previsaoList) {
  venda        <- previsaoList$vendasQuantTs
  loja         <- previsaoList$loja
  nome.grupo   <- previsaoList$grupo
  treino.start <- previsaoList$treinoStart
  treino.end   <- previsaoList$treinoEnd
  teste.start  <- previsaoList$testeStart
  teste.end    <- previsaoList$testeEnd
  previsao     <- previsaoList$previsao
  teste        <- window(venda, start = teste.start, end = teste.end)
  treino       <- window(venda, start = treino.start, end = treino.end)
  isolamento   <- window(venda, start = treino.end, end = teste.start)
  countYear    <- venda %>%
    time() %>%
    as.Date() %>%
    format(format = "%Y") %>%
    unique() %>%
    length()

  return(autoplot(venda, alpha = 0) +
           labs(x       = "Tempo", y = "Quantidade de itens vendidos",
                title   = paste(loja, nome.grupo),
                caption = paste0("Período de treino: ", treino.start %>% formatCYM(), " - ", treino.end %>% formatCYM(), " ",
                                 "Período de teste: ", teste.start %>% formatCYM(), " - ", teste.end %>% formatCYM())
           ) +
           autolayer(previsao$mean, series = "Previsão") +
           autolayer(teste, series = "Teste") +
           autolayer(isolamento, series = "Isolamento") +
           autolayer(treino, series = "Treino") +
           scale_colour_viridis_d() +
           scale_x_yearmon(format = "%Y", n = countYear) +
           scale_y_continuous() +
           theme_light()
  )
}

#' @title retorna as estatíticas de acurácia
getAcuracia <- function(previsao, teste.ts) {
  acuracia.df           <- accuracy(previsao, teste.ts)
  return(acuracia.df)
}
