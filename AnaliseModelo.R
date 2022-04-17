
lista.modelo <- readRDS("lista.modelos.detalhado.rds")

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

paramentro.meses <- modelos %>%
  select(treino.start, treino.end, isolamento.start, isolamento.end, teste.start, teste.end) %>%
  unique()

paramentro.meses <- paramentro.meses %>%
  mutate(id=seq.int(nrow(paramentro.meses))) %>%
  select(id, treino.start, treino.end, isolamento.start, isolamento.end, teste.start, teste.end)

paramentro.loja.grupo <- modelos %>%
  select(loja, grupo) %>%
  arrange(loja, grupo) %>%
  unique()

acuracia.treino <- modelos %>%
  filter(modeloAceito == "SIM") %>%
  group_by(loja, grupo) %>%
  summarise(MAPE.Treino = min(MAPE.Treino))

acuracia.treino.teste <- modelos %>%
  filter(modeloAceito == "SIM") %>%
  inner_join(acuracia.treino, by = c("loja", "grupo", "MAPE.Treino")) %>%
  group_by(loja, grupo, MAPE.Treino) %>%
  summarise(MAPE.Teste = min(MAPE.Teste))

modelos %>%
  filter(modeloAceito == "SIM") %>%
  inner_join(acuracia.treino.teste, by = c("loja", "grupo", "MAPE.Treino", "MAPE.Teste")) %>%
  View()

lista.modelo[[264]]$plot() %>% ggplotly()
lista.modelo[[1842]]$plot() %>% ggplotly()
lista.modelo[[37]]$plot() %>% ggplotly()

