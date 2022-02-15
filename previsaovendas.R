######################################################################################################################
# Carrega pacotes
######################################################################################################################

pacotes <- c("readr", "readxl", "plotly", "tidyverse", "gridExtra", "forecast", "TTR",
             "smooth", "tsibble", "fable", "tsibbledata", "fpp3", "lubridate",
             "urca", "dygraphs", "quantmod", "BETS", "tseries", "FinTS",
             "gridExtra", "scales", "caret", "xtable", "tsutils", "GetBCBData",
             "quantmod", "dgof", "seasonal", "DBI", "RSQLite", "foreach", "doParallel")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

######################################################################################################################
# Funções
######################################################################################################################

find.ts <- function(df, segmento) {
  datas <- df.vendas %>%
    select(data) %>%
    unique()

  df.group <- df %>%
    filter(grupo == segmento) %>%
    group_by(data) %>%
    summarize(vendas = sum(sumValor / 1000000))

  df.group <- merge(x = datas, y = df.group, by = "data", all.x = TRUE) %>%
    transmute(
      data = data,
      vendas = if_else(is.na(vendas), 0, vendas)
    )

  ts <- ts(df.group$vendas, start = c(2010, 1), end = c(2021, 12), frequency = 12)

  return(ts)
}


graficoTimeSerie <- function(df, segmento) {
  ts <- if (class(df) == "ts") df else find.ts(df, segmento)

  return(plot(ts, main = segmento, xlab = "Meses", ylab = "R$ Milhões"))
}

graficoTsDisplay <- function(df, segmento) {
  ts <- if (class(df) == "ts") df else find.ts(df, segmento)

  return(ggtsdisplay(ts))
}


teste.df <- function(ts) {
  teste <- ur.df(ts)
  summary(teste)

  estatistica <- teste@testreg[["coefficients"]]["z.lag.1", "Pr(>|t|)"]
  return(estatistica)
}

formatNumero <- function(x) formatC(x, decimal.mark = ",")

saveJPEG <- function(plot, prefixo, grupo, width, height) {
  jpeg(filename = paste0("files/", prefixo, "_", grupo, ".jpeg"),
       width = width * 4, height = height * 4, res = 300)
  ret <- if (length(plot) == 1 && class(plot) == "function") {
    plot()
  }else {
    print(plot)
  }
  dev.off()
  return(ret)
}

######################################################################################################################
# Carrega dados
######################################################################################################################

## produtos
df.prd <- read.csv(file = "dados/pintos/bd_prd.csv", sep = ";",
                   colClasses = c("prdno" = "character",
                                  "sku" = "character")) %>%
  mutate(grupo = ifelse(grupo == "ADULTO FEM", "CONFECCAO", grupo)) %>%
  mutate(grupo = as.factor(grupo))
df.prd$grupo %>% unique()


str(df.prd)

## vendas
df.vendas <- read.csv(file = "dados/pintos/bd_vendas.csv", sep = ";",
                      colClasses = c("data" = "character",
                                     "prdno" = "character")) %>%
  mutate(data = data %>%
    as.Date(format = "%Y%m%d") %>%
    as.yearmon(data))

str(df.vendas)

## compras
df.compras <- read.csv(file = "dados/pintos/bd_compras.csv", sep = ";",
                       colClasses = c("data" = "character",
                                      "prdno" = "character")) %>%
  mutate(data = data %>%
    as.Date(format = "%Y%m%d") %>%
    as.yearmon(data))

str(df.compras)

## Teste de previsão

mesToString <- function(mes, dias = 0) {
  return((mes %>%
    paste0("-01") %>%
    as.Date() + dias) %>%
           format("%Y-%m"))
}

mesToVector <- function(mes, dias = 0) {
  return((mes %>%
    paste0("-01") %>%
    as.Date() + dias) %>%
           format("%Y-%m") %>%
           strsplit("-") %>%
           unlist() %>%
           as.integer())
}

df.meses <- function(periodo = c("2008-03", "2021-12")) df.vendas %>%
  select(data) %>%
  unique() %>%
  filter((data >= periodo[1]) & (data <= periodo[2]))


testaPrevisao <- function(grupoPar = "MOVEL",
                          teste = "",
                          plot = TRUE,
                          periodoLockDown = c('2020-01', '2020-12'),
                          periodoTreino = c("2008-03", "2020-12"),
                          periodoTeste = c("2021-01", "2021-12")) {
  mes.inicial.ld <- periodoLockDown[1]
  mes.final.ld <- periodoLockDown[2]

  start.ld <- mesToVector(mes.inicial.ld, -15)
  end.ld <- mesToVector(mes.final.ld, 45)

  start.treino <- mesToVector(periodoTreino[1])
  end.treino <- mesToVector(periodoTreino[2])

  start.teste <- mesToVector(periodoTeste[1])
  end.teste <- mesToVector(periodoTeste[2])

  meses <- df.meses(c(periodoTreino[1], periodoTeste[2]))

  df.vendas.grupo <- df.vendas %>%
    merge(y = df.prd %>% filter(grupo == grupoPar), by = "prdno") %>%
    group_by(data) %>%
    summarise(totalMensal = sum(valor / 1000000)) %>%
    merge(y = meses, by = "data", all.y = TRUE) %>%
    mutate(totalMensal = ifelse(is.na(totalMensal), 0.00, totalMensal)) %>%
    filter((data >= periodoTreino[1]) & (data <= periodoTeste[2]))

  ts.vendas.total <- ts(df.vendas.grupo$totalMensal, start = start.treino, end = end.teste, 12)
  ts.vendas.ld <- window(ts.vendas.total, start.ld, end.ld)

  df.vendas.grupo <- df.vendas.grupo %>%
    mutate(totalMensal = ifelse((data %>% format("%Y-%m") < mes.inicial.ld) |
                                  (data %>% format("%Y-%m") > mes.final.ld),
                                totalMensal, NA))

  ts.vendas <- ts(df.vendas.grupo$totalMensal, start = start.treino, end = end.teste, 12)

  vendas.treino <- window(ts.vendas, start = start.treino, end = end.treino)
  vendas.teste <- window(ts.vendas, start = start.teste, end = end.teste)

  if (plot) {
    plot.ts <- autoplot(ts.vendas,
                        main = paste0("Grupo: ", grupoPar),
                        ylab = "Vendas R$ milhões",
                        xlab = "Meses") +
      autolayer(ts.vendas.ld, series = "Isolamento") +
      autolayer(vendas.treino, series = "Treino") +
      autolayer(vendas.teste, series = "Teste") +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()

    saveJPEG(plot.ts, prefixo = paste0("serieTemporal", teste), grupo = grupoPar, width = 1200, height = 400)
  }

  arima.vendas <- auto.arima(vendas.treino, trace = F)

  fun.lb <- function()  checkresiduals(arima.vendas)

  teste.lb <- if (plot) {
    saveJPEG(fun.lb, prefixo = paste0("residuos", teste), grupo = grupoPar, width = 1200, height = 400)
  }else {
    fun.lb()
  }

  resultado.lb <- if (teste.lb$p.value >= 0.01) {
    paste0("teste de Ljung-Box p-value ", teste.lb$p.value %>% formatNumero(),
           " > 0.01 - Aceitamos H0, residuos não são correlacionados")
  } else {
    paste0("teste de Ljung-Box p-value ", teste.lb$p.value %>% formatNumero(),
           " < 0.01 - Aceitamos H1, residuos são correlacionados")
  }

  if (teste.lb$p.value < 0.01) {
    return(NULL)
  }

  residuals <- arima.vendas$residuals

  teste.ks <- ks.test(residuals, "pnorm", mean(residuals, na.rm = TRUE),
                      sd(residuals, na.rm = TRUE))
  resultado.ks <- if (teste.ks$p.value >= 0.01) {
    paste0("teste de Kolmogorov-Smirnov p-value ", teste.ks$p.value %>% formatNumero(),
           " > 0.01 - Aceitamos H0, residuos normais")
  } else {
    paste0("teste de Kolmogorov-Smirnov p-value ", teste.ks$p.value %>% formatNumero(),
           " < 0.01 - Aceitamos H1, não normalidade dos resíduos")
  }

  if (teste.ks$p.value < 0.01) {
    return(NULL)
  }

  teste.ac <- ArchTest(arima.vendas$residuals)
  resultado.ac <- if (teste.ac$p.value >= 0.01) {
    paste0("teste ARCH p-value ", teste.ac$p.value %>% formatNumero(),
           " > 0.01 - Aceitamos H0, garante não existência de efeitos ARCH")
  } else {
    paste0("teste ARCH p-value ", teste.ac$p.value %>% formatNumero(),
           " < 0.01 - Aceitamos H1, existência de efeitos ARCH")
  }

  if (teste.ac$p.value < 0.01) {
    return(NULL)
  }

  arima.previsao <- forecast(arima.vendas, 12)

  acurracia <- accuracy(arima.previsao$mean, vendas.teste)

  mape <- acurracia["Test set", "MAPE"] %>% round(3)

  resultado.acuracia <- paste0("O percentual de erro MAPE do modelo é ", mape %>% formatNumero(), " %")

  resultado <- paste(paste0("Teste: ", teste), resultado.lb, resultado.ks, resultado.ac, resultado.acuracia, "\n", sep = "\n")
  cat(resultado)

  plotForecast <- if (plot) {
    autoplot(
      vendas.treino,
      main = paste0("Grupo: ", grupoPar),
      ylab = "Vendas R$ milhões",
      xlab = "Meses"
    ) +
      autolayer(vendas.teste, serie = "Valores Reais") +
      autolayer(ts.vendas.ld, series = "Isolamento") +
      autolayer(arima.previsao$mean, serie = "Forecast") +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(from = 2008, to = 2022, by = 1)) +
      labs(subtitle = resultado) +
      theme_bw()

    saveJPEG(plotForecast, prefixo = paste0("forecast", teste), grupo = grupoPar, width = 1200, height = 400)
  }

  return(
    list(
      teste.id = teste,
      teste.lb = list(
        p.value = teste.lb$p.value,
        resultado = resultado.lb
      ),
      teste.ac = list(
        p.value = teste.ac$p.value,
        resultado = resultado.ac
      ),
      teste.ks = list(
        p.value = teste.ks$p.value,
        resultado = resultado.ks
      ),
      mape = mape,
      plots = list(
        plot.forecast = plotForecast,
        plot.ts = plot.ts
      )
    )
  )
}

grupos <- df.prd$grupo %>%
  unique() %>%
  sort()

v.start.treino <- c("2008-06", "2009-01", "2009-06", "2010-01", "2010-06",
                    "2011-01", "2011-06", "2012-01", "2012-06", "2013-01",
                    "2013-06", "2014-01", "2014-06", "2015-01", "2015-06",
                    "2016-01", "2016-06", "2017-01", "2017-06", "2018-01")
v.start.isolamento <- c("2020-01")
v.end.isolamento <- c("2020-12")

lista.treino <- data.frame(
  id = integer(),
  grupo = character(),
  start.treino = character(),
  start.isolamento = character(),
  end.isolamento = character(),
  mape = numeric()
)

id <- 0

for (grupo in grupos) {
  for (start.treino in v.start.treino) {
    print(c(grupo, start.treino))
    for (start.isolamento in v.start.isolamento) {
      for (end.isolamento in v.end.isolamento) {
        id <- id + 1
        lista.treino <- lista.treino %>% add_row(
          id = id,
          grupo = grupo,
          start.treino = start.treino,
          start.isolamento = start.isolamento,
          end.isolamento = end.isolamento,
          mape = NULL
        )
      }
    }
  }
}


numCores <- 4

registerDoParallel(numCores)

foreach(row = seq_len(nrow(lista.treino))) %dopar% {
  df.row <- lista.treino[row,]
  teste <- testaPrevisao(grupoPar = df.row$grupo,
                         teste = df.row$id,
                         plot = FALSE,
                         periodoLockDown = c(df.row$start.isolamento, df.row$end.isolamento),
                         periodoTreino = c(df.row$start.treino, "2020-12"))
  if (length(teste) > 0) {
    lista.treino[row, "mape"] <- teste$mape
  }else {
    lista.treino[row, "mape"] <- NA
  }
  print(df.row$id)
}

stopImplicitCluster()






