################################################################################
# Avaliação
################################################################################

# Ler ods dados de treino
listaTreino <- readRDS("lista.treino.rds")

testeStartList <- c("2020-6", "2020-7", "2020-8", "2020-9", "2020-10",
                    "2020-11", "2020-12", "2021-1")

row <- 1

length(testeStartList)

ID <- 0

listaAvaliacao <- foreach(row = seq_len(nrow(listaTreino)), .combine = 'rbind') %do% {
  rowTreino         <- listaTreino[row,]
  treinoStart       <- c(2009, 1)
  treinoEnd         <- c(2020, 2)
  idTreino          <- rowTreino$id
  grupo             <- rowTreino$grupoPar
  loja              <- rowTreino$lojaPar
  vendasQuantTs     <- rowTreino$vendasQuantTs
  vendasQuantTreino <- rowTreino$vendasQuantTreino
  vendasValorTs     <- rowTreino$vendasValorTs
  vendasValorTreino <- rowTreino$vendasValorTreino
  arima             <- rowTreino$arima

  decomposeTreino                <- decompose(vendasQuantTreino)
  testeDickeyFuller              <- ur.df(vendasQuantTreino,
                                          type       = "drift",
                                          lags       = 24,
                                          selectlags = "AIC")
  interpleracaoDickeyFuller      <- getInterpleracaoDickeyFuller(testeDickeyFuller)
  descicaoModelo                 <- getDescicaoModelo(arima)
  testeLjungBox                  <- checkresiduals(arima, plot = FALSE)
  interpletacaoLjungBox          <- getInterpletacaoLjungBox(testeLjungBox)
  residuals                      <- arima$residuals
  testeKolmogorovSmirnov         <- ks.test(residuals, "pnorm",
                                            mean(residuals),
                                            sd(residuals))
  interpletacaoKolmogorovSmirnov <- getInterpletacaoKolmogorovSmirnov(testeKolmogorovSmirnov)
  testeArch                      <- ArchTest(arima$residuals)
  interpletacaoArch              <- getInterpletacaoArch(testeArch)
  modeloAceito                   <- getModeloAceito(testeArch              = testeArch,
                                                    testeKolmogorovSmirnov = testeKolmogorovSmirnov,
                                                    testeLjungBox          = testeLjungBox)

  if (modeloAceito == "SIM")
    foreach(rowT = seq_len(length(testeStartList)), .combine = 'rbind') %do% {
      testeStart <- testeStartList[rowT] %>% formatSYM()
      print(c(loja, grupo, testeStart))
      testeEnd         <- c(2021, 12)
      vendasQuantTeste <- window(vendasQuantTs, start = testeStart, end = testeEnd)
      previsao         <- forecast::forecast(arima, h = 24)
      acuracia.df      <- getAcuracia(previsao, vendasQuantTeste)
      ID               <- ID + 1
      list(
        id                             = ID,
        idTreino                       = idTreino,
        treinoStart                    = treinoStart,
        treinoEnd                      = treinoEnd,
        grupo                          = grupo,
        loja                           = loja,
        vendasQuantTs                  = vendasQuantTs,
        vendasQuantTreino              = vendasQuantTreino,
        vendasValorTs                  = vendasValorTs,
        vendasValorTreino              = vendasValorTreino,
        arima                          = arima,
        decomposeTreino                = decomposeTreino,
        testeDickeyFuller              = as.numeric,
        interpleracaoDickeyFuller      = interpleracaoDickeyFuller,
        descicaoModelo                 = descicaoModelo,
        testeLjungBox                  = testeLjungBox,
        interpletacaoLjungBox          = interpletacaoLjungBox,
        residuals                      = residuals,
        testeKolmogorovSmirnov         = ks.test,
        interpletacaoKolmogorovSmirnov = interpletacaoKolmogorovSmirnov,
        testeArch                      = testeArch,
        interpletacaoArch              = interpletacaoArch,
        modeloAceito                   = modeloAceito,
        testeStart                     = testeStart,
        testeEnd                       = testeEnd,
        vendasQuantTeste               = vendasQuantTeste,
        previsao                       = previsao,
        acuracia.df                    = acuracia.df
      )
    }
  else {
    list()
  }
}


listModelos <- foreach(row = seq_len(nrow(listaAvaliacao)), .combine = 'rbind') %do% {
  previsaoList <- listaAvaliacao[row,]
  acuracia.df  <- previsaoList$acuracia.df
  list(
    id             = previsaoList$id,
    loja           = previsaoList$loja,
    grupo          = previsaoList$grupo,
    descicaoModelo = previsaoList$descicaoModelo,
    mapeTreino     = acuracia.df[1,"MAPE"],
    mapeTeste      = acuracia.df[2,"MAPE"]
  )
} %>% as.data.frame()



