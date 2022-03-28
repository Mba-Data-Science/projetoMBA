
##########################################################################
## Definição da série base

grupo.classificacao <- "Cama, mesa e banho"

serie.venda <- vendas.grupo.wider %>% 
  transmute(data, 
            vendas = vendas.grupo.wider[,grupo.classificacao])

class(serie.venda)

## divisão de janelas
# Paramentros para os período de treste, treino e isolamento social

treino.start <- c(2009, 1)
treino.end   <- c(2020, 2)

isolamento.start <- c(2020, 2)
isolamento.end <- c(2020, 8)

teste.start <- c(2020, 8)
teste.end <- c(2021, 12)

isolamento.start.ym <- yearmonth(paste(isolamento.start, collapse = "-"))
isolamento.end.ym <- yearmonth(paste(isolamento.end, collapse = "-"))

serie.start.ym <- yearmonth(paste(treino.start, collapse = "-"))
serie.end.ym <- yearmonth(paste(teste.end, collapse = "-"))

## definindo a serie

## Serie temporal total
serie.venda <- serie.venda %>%
  filter(data >= serie.start.ym & data <= serie.end.ym) %>%
  arrange(data)

venda.ts <- ts(serie.venda$vendas, start = treino.start, end = teste.end, frequency = 12)

class(venda.ts)

## Secsionamento das séries temporaris em treino, teste e isoloamento social
teste.ts <- window(venda.ts, start = teste.start, end = teste.end)
isolamento.ts <- window(venda.ts, start = isolamento.start, end = isolamento.end)
treino.ts <- window(venda.ts, start = treino.start, end = treino.end)

treino.ts <- window(venda.ts, start = treino.start, end = treino.end)
#treino.ts <- ts.union(treino.ts.pre, treino.ts.pos)

autoplot(treino.ts)

## Análise da Série

### decomposição
vendas.decompose <- decompose(treino.ts)

plot(vendas.decompose)

### analise de correlação

ggtsdisplay(treino.ts)

# Teste estacionariedade Diken Fouler

treino.df <- ur.df(treino.ts, type = "drift", lags= 24, selectlags = "AIC")
treino.df.sum <- summary(treino.df)  
tau1 <- treino.df.sum@teststat["statistic",1]
cval <- treino.df.sum@cval[1, "5pct"]
treino.df.sum@model

if(abs(tau1) > abs(cval)) "Estationaria" else "Não estacionaria"
corrgram(treino.df@res, lag.max = 36)

## auto arima

arima <- auto.arima(treino.ts, 
                    max.order = 5,
                    D = 1,
                    d = 1,
                    stepwise = FALSE,
                    approximation = FALSE,
                    parallel = TRUE)

arima


residuas <- checkresiduals(arima, plot = FALSE)

residuas$p.value

teste <- ks.test(arima$residuals, "pnorm", mean(arima$residuals),
        sd(arima$residuals))
teste
teste$p.value

arch <- ArchTest(arima$residuals)
arch$p.value

tsdiag(arima)
Box.test(arima$residuals, type = "Ljung-Box")


previsao <- forecast::forecast(arima, h = length(teste.ts) + length(isolamento.ts)-2)

####################################################
# PLOT

countYear <- venda.ts %>%
  time() %>%
  as.Date() %>%
  format(format = "%Y") %>%
  unique() %>%
  length()

ggplotly(
  autoplot(treino.ts, alpha = 0) +
    labs(x = "Tempo", y = "Quantidade Vendida") +
    autolayer(treino.ts, series = "Treino") +
    autolayer(previsao$mean, series = "Previsão") +
    autolayer(teste.ts, series = "Teste") +
    autolayer(isolamento.ts, series = "Isolamento") +
    scale_x_yearmon(format = "%Y", n = countYear) +
    scale_colour_viridis_d() +
    scale_y_continuous() +
    theme_bw()
)

arima

previsao.teste <- previsao$mean %>% window(start = teste.start, end = teste.end)

accuracy(previsao, teste.ts)
accuracy(previsao.teste, teste.ts)

acuracia <- accuracy(previsao, teste.ts)
acuracia


formatC(0.0264, format="f", digits=2)
