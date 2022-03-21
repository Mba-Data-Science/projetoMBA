######################################################################################################################
# Carrega pacotes
######################################################################################################################
options(digits = 4, scipen = 30)

pacotes <- c("tidyverse", "forecast", "urca", "zoo", "FinTS", "foreach")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

formatYM <- function(cym) {
  return(paste0(cym[2], '/', cym[1]))
}

## Classe Modelo Previsao
ModeloPrevisao <- setRefClass("ModeloPrevisao",
                              fields = list(
                                id = "numeric",
                                nome.grupo = "character",
                                loja = "character",
                                venda.ts = "ts",
                                treino.start = "numeric",
                                treino.end = "numeric",
                                isolamento.start = "numeric",
                                isolamento.end = "numeric",
                                teste.start = "numeric",
                                teste.end = "numeric",
                                teste.ts = "ts",
                                isolamento.ts = "ts",
                                treino.ts = "ts",
                                arima = "ANY"
                              ),
                              methods = list(
                                init = function(id,
                                                nome.grupo,
                                                loja,
                                                venda.ts,
                                                treino.start = c(2009, 1),
                                                treino.end = c(2020, 2),
                                                isolamento.start = c(2020, 2),
                                                isolamento.end = c(2020, 8),
                                                teste.start = c(2020, 8),
                                                teste.end = c(2021, 12)) {
                                  id <<- id
                                  nome.grupo <<- nome.grupo
                                  loja <<- loja
                                  venda.ts <<- venda.ts
                                  treino.start <<- treino.start
                                  treino.end <<- treino.end
                                  isolamento.start <<- isolamento.start
                                  isolamento.end <<- isolamento.end
                                  teste.start <<- teste.start
                                  teste.end <<- teste.end
                                  ## divisão de janelas

                                  ## definindo a serie
                                  teste.ts <<- window(venda.ts, start = teste.start, end = teste.end)
                                  isolamento.ts <<- window(venda.ts, start = isolamento.start, end = isolamento.end)
                                  treino.ts <<- window(venda.ts, start = treino.start, end = treino.end)
                                },
                                plotTreino = function() autoplot(treino.ts),
                                decomposeTreino = function() decompose(treino.ts),
                                plotDecompose = function() plot(decomposeTreino()),
                                plotCorelacao = function() ggtsdisplay(treino.ts),
                                testeDickeyFuller = function() tryCatch(
                                  expr = {
                                    ur.df(treino.ts,
                                          type = "drift",
                                          lags = 24,
                                          selectlags = "AIC")
                                  },
                                  error = function(e){ 
                                    NULL
                                  },
                                  warning = function(w){
                                    NULL
                                  }
                                ),
                                interpleracaoDickeyFuller = function() {
                                  result <- tryCatch(
                                    expr = {
                                      treino.df <- testeDickeyFuller()
                                      if (is.null(treino.df)) 
                                      {
                                        ""
                                      } else {
                                        treino.df.sum <- summary(treino.df)
                                        tau <- treino.df.sum@teststat[1, 1]
                                        cval <- treino.df.sum@cval[1, "5pct"]
                                        if (is.na(tau) | is.na(cval)) {
                                          ""
                                        }
                                        else
                                        if (abs(tau) < abs(cval)) {
                                          "H0: A série não é Estacionária (95% de confiaça)"
                                        } else {
                                          "H1: A série é Estacionária (95% de confiaça)"
                                        }
                                      }
                                    },
                                    error = function(e) {
                                      ""
                                    },
                                    warning = function(w){
                                      ""
                                    }
                                  )
                                  return(result)
                                },
                                executaModelo = function() {
                                  arima <<- auto.arima(treino.ts)
                                },
                                descicaoModelo = function() {
                                  a <- arima$arma
                                  return(paste0("ARIMA(", a[1], ",", a[6], ",", a[2], ")(", a[3], ",", a[7], ",", a[4], ")[", a[5], "]"))
                                },
                                testeLjungBox = function(plot = FALSE) checkresiduals(arima, plot = plot),
                                interpletacaoLjungBox = function() {
                                  p.value <- formatC(testeLjungBox()$p.value, format = "f", digits = 2)
                                  return(
                                    if (p.value > 0.05) {
                                      paste0("H0: Os resíudos não são correlacionados (", p.value, " > 0.05)")
                                    } else {
                                      paste0("H1: Os resíudos são correlacionados (", p.value, " < 0.05)")
                                    }
                                  )
                                },
                                testeKolmogorovSmirnov = function() tryCatch(
                                  expr = {
                                    ks.test(arima$residuals, "pnorm",
                                            mean(arima$residuals),
                                            sd(arima$residuals))
                                  },
                                  error = function(e){ 
                                    NULL
                                  },
                                  warning = function(w){
                                    NULL
                                  }
                                ),
                                interpletacaoKolmogorovSmirnov = function() {
                                  teste <- testeKolmogorovSmirnov()
                                  p.value <-  if (is.null(teste)) {
                                    NA
                                  }else {
                                    formatC(teste$p.value, format = "f", digits = 2)
                                  }
                                  return(
                                    if (is.na(p.value)) {
                                      ""
                                    }
                                    else
                                    if (p.value > 0.05) {
                                      paste0("H0: Residuos são normais (", p.value, " > 0.05)")
                                    }else {
                                      paste0("H1: Residuos não são normais (", p.value, " < 0.05)")
                                    }
                                  )
                                },
                                testeArch = function() ArchTest(arima$residuals),
                                interpletacaoArch = function() {
                                  p.value <- formatC(testeArch()$p.value, format = "f", digits = 2)
                                  return(
                                    if (p.value > 0.05) {
                                      paste0("H0: Garante a não existencia de efeitos Arch (", p.value, " > 0.05)")
                                    }else {
                                      paste0("H1: Existe Efeito Arch (", p.value, " < 0.05)")
                                    }
                                  )
                                },
                                modeloAceito = function() {
                                  p.value.Arch <- testeArch()$p.value
                                  p.value.KS <- testeKolmogorovSmirnov()$p.value
                                  p.value.LB <- testeLjungBox()$p.value
                                  return(
                                    if(is.null(p.value.Arch) | is.null(p.value.KS) | is.null(p.value.LB))
                                      "NÃO"
                                    else
                                    if (p.value.Arch > 0.05 &
                                      p.value.KS > 0.05 &
                                      p.value.LB > 0.05)
                                      "SIM"
                                    else
                                      "NÃO"
                                  )
                                },
                                executaPrevisao = function() forecast::forecast(arima, h = length(teste.ts) +
                                  length(isolamento.ts) - 2),
                                plot = function() {
                                  countYear <- venda.ts %>%
                                    time() %>%
                                    as.Date() %>%
                                    format(format = "%Y") %>%
                                    unique() %>%
                                    length()
                                  previsao <- executaPrevisao()

                                  ts <- window(venda.ts, start = c(2009, 1), end = treino.start)
                                  return(autoplot(ts) +
                                           labs(x = "Tempo", y = "Quantidade de itens vendidos",
                                                title = paste(loja, nome.grupo),
                                                caption = paste0("Período de treino: ", treino.start %>% formatYM(), " - ", treino.end %>% formatYM(), " ",
                                                                 "Período de teste: ", teste.start %>% formatYM(), " - ", teste.end %>% formatYM())
                                           ) +
                                           autolayer(previsao$mean, series = "Previsão") +
                                           autolayer(teste.ts, series = "Teste") +
                                           autolayer(isolamento.ts, series = "Isolamento") +
                                           autolayer(treino.ts, series = "Treino") +
                                           scale_colour_viridis_d() +
                                           scale_x_yearmon(format = "%Y", n = countYear) +
                                           scale_y_continuous() +
                                           theme_light()
                                  )
                                },
                                acuracia = function() {
                                  previsao <- executaPrevisao()
                                  acuracia.df <- accuracy(previsao, teste.ts)
                                  rownames(acuracia.df) <- c("Treino", "Teste")
                                  return(acuracia.df)
                                }
                              )
)


###########################################################

loja.grupos <- tibble(loja = vendas.grupo$loja, grupo = vendas.grupo$grupo) %>%
  unique() %>%
  arrange(loja, grupo)
treino.start.list <- c("2009-1", "2010-1", "2011-1", "2012-1", "2013-1",
                       "2014-1", "2015-1", "2016-1", "2017-1")
teste.start.list <- c("2020-6", "2020-7", "2020-8", "2020-9", "2020-10",
                      "2020-11", "2020-12", "2021-1")

id <- 0

lista.parametros <- foreach(row = 1:nrow(loja.grupos), .combine = 'rbind') %:%
  foreach(treino.start = treino.start.list, .combine = 'rbind') %:%
  foreach(teste.start = teste.start.list, .combine = 'rbind') %do% {
  df.row <- loja.grupos[row,]
  print(row)
  print(df.row)

  grupoPar <- df.row$grupo
  lojaPar <- df.row$loja

  id <- id + 1
  tibble(id = id,
         grupo = grupoPar,
         loja = lojaPar,
         treino.start = treino.start,
         teste.start = teste.start)
}

chave <- ""

lista.modelo <- foreach(row = 1:nrow(lista.parametros), .combine = 'c') %do% {
  paramentro <- lista.parametros[row,]
  id <- paramentro$id
  grupoPar <- paramentro$grupo
  lojaPar <- paramentro$loja
  treino.start <- paramentro$treino.start %>%
    str_split("-") %>%
    unlist() %>%
    as.numeric()
  teste.start <- paramentro$teste.start %>%
    str_split("-") %>%
    unlist() %>%
    as.numeric()

  if (chave != paste(grupoPar, lojaPar)) {
    print(c("chave", chave))
    serie.venda <- vendas.grupo %>%
      filter(grupo == grupoPar & loja == lojaPar) %>%
      transmute(data,
                vendas = quant.venda) %>%
      arrange(data)
    venda.ts <- ts(serie.venda$vendas, start = c(2009, 1), end = c(2021, 12), frequency = 12)
    chave <- paste(grupoPar, lojaPar)
  }

  modelo <- ModeloPrevisao$new()
  modelo$init(id = id,
              nome.grupo = grupoPar %>% as.character(),
              loja = lojaPar %>% as.character(),
              venda.ts = venda.ts,
              treino.start = treino.start,
              treino.end = c(2020, 2),
              isolamento.start = c(2020, 2),
              isolamento.end = teste.start,
              teste.start = teste.start,
              teste.end = c(2021, 12)
  )
  modelo$executaModelo()
  print(paramentro)
  #print(modelo$acuracia())
  #print(modelo$plot())
  modelo
}

saveRDS(lista.modelo, "lista.modelo.rds")

lista.modelo <- readRDS("lista.modelo.rds")

lista.modelo <- foreach(modelo = lista.modelo, .combine = 'c') %do% {
  modelo$copy()
}

modelos <- foreach(modelo = lista.modelo, .combine = 'rbind') %do% {
  acuracia <- modelo$acuracia()
  testeDickeyFuller <- modelo$interpleracaoDickeyFuller()
  testeLjungBox <- modelo$interpletacaoLjungBox()
  testeKolmogorovSmirnov <- modelo$interpletacaoKolmogorovSmirnov()
  testeArch <- modelo$interpletacaoArch()
  modeloAceito <- modelo$modeloAceito()

  print(modelo$id)

  tibble(
    id = modelo$id,
    grupo = modelo$nome.grupo,
    loja  = modelo$loja,
    treino.start = modelo$treino.start %>% paste(collapse = "-"),
    treino.end = modelo$treino.end %>% paste(collapse = "-"),
    isolamento.start = modelo$isolamento.start %>% paste(collapse = "-"),
    isolamento.end = modelo$isolamento.end %>% paste(collapse = "-"),
    teste.start = modelo$teste.start %>% paste(collapse = "-"),
    teste.end = modelo$teste.end %>% paste(collapse = "-"),
    MAPE.Treino = acuracia["Treino", "MAPE"],
    MAPE.Teste = acuracia["Teste", "MAPE"],
    RMSE.Treino = acuracia["Treino", "RMSE"],
    RMSE.Teste = acuracia["Teste", "RMSE"],
    modelo = modelo$descicaoModelo(),
    testeDickeyFuller = testeDickeyFuller,
    testeLjungBox = testeLjungBox,
    testeKolmogorovSmirnov = testeKolmogorovSmirnov,
    testeArch = testeArch,
    modeloAceito = modeloAceito
  )
}

modelos %>%
  View()

modelo <- lista.modelo[[3311]]
modelo$plot()


modelos %>%
  filter(grupo == "Móveis" & modeloAceito == "SIM") %>%
  View()

lista.modelo[[80]]$plot()
lista.modelo[[307]]$plot()
lista.modelo[[313]]$plot()

sum <- lista.modelo[[141]]$testeDickeyFuller() %>% summary()


arima$arma
class(arima)
lista.modelo[[755]]$plot() %>% ggplotly()
lista.modelo[[707]]$plot() %>% ggplotly()
lista.modelo[[828]]$plot() %>% ggplotly()

l <- foreach(modelo = lista.modelo, .combine = 'rbind') %do% {
  modelo$arima$arma
}

lista.modelo[[785]]$arima
lista.modelo[[785]]$arima$arma


