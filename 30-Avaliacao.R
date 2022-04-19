################################################################################
# Avaliação
################################################################################

listaTreino <- readRDS("lista.treino.rds")

listaTreino[1,]$lojaPar

vendasQuantTs <- ts(serieVenda$quant, start = treinoStart, end = testeEnd, frequency = 12)
vendasQuantTreino <- window(vendasQuantTs, start = treinoStart, end = treinoEnd)
vendasQuantTeste <- window(vendasQuantTs, start = testeStart, end = testeEnd)
vendasQuantIsolamento <- window(vendasQuantTs, start = isolamentoStart, end = isolamentoEnd)

vendasValorTs <- ts(serieVenda$valor, start = treinoStart, end = testeEnd, frequency = 12)
vendasValorTreino <- window(vendasValorTs, start = treinoStart, end = treinoEnd)
vendasValorTeste <- window(vendasValorTs, start = testeStart, end = testeEnd)
vendasValorIsolamento <- window(vendasValorTs, start = isolamentoStart, end = isolamentoEnd)

list <- list(
  vendasQuantTs = ts(serieVenda$quant, start = treinoStart, end = testeEnd, frequency = 12),
  vendasQuantTreino = window(vendasQuantTs, start = treinoStart, end = treinoEnd),
  vendasQuantTeste = window(vendasQuantTs, start = testeStart, end = testeEnd),
  vendasQuantIsolamento = window(vendasQuantTs, start = isolamentoStart, end = isolamentoEnd),
  
  vendasValorTs = ts(serieVenda$valor, start = treinoStart, end = testeEnd, frequency = 12),
  vendasValorTreino = window(vendasValorTs, start = treinoStart, end = treinoEnd),
  vendasValorTeste = window(vendasValorTs, start = testeStart, end = testeEnd),
  vendasValorIsolamento = window(vendasValorTs, start = isolamentoStart, end = isolamentoEnd)
)
