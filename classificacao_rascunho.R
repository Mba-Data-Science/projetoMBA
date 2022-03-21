#########################################################
## Classifica produtos

grupo.classificacao <- "Moveis"


produtos.grupos <- produtos %>%
  filter(grupo == grupo.classificacao) %>%
  select(prdno, preco, saldoAtual)

vendas.mes <- vendas %>%
  inner_join(produtos.grupos, by = "prdno") %>%
  group_by(prdno, mes = format(as.Date(data), "%m")) %>%
  summarise(
    quant.mean = mean(quant)
  )

vendas.mes.wider <- vendas.mes %>%
  pivot_wider(names_from = mes, values_from = quant.mean)  

dados.produtos <- produtos.grupos %>%
  left_join(vendas.mes.wider, by = "prdno")

dados.produtos[is.na(dados.produtos)] <- 0

dados.produtos.padronizados <- scale(dados.produtos[,-1])

dados.produtos.k4 <- kmeans(dados.produtos.padronizados, centers = 4)
dados.produtos.k4$cluster

produtos.classificacao <- cbind(dados.produtos, cluster = dados.produtos.k4$cluster) %>%
  select(prdno, cluster)

vendas.agrupadas.cluster <- vendas %>%
  merge(produtos.classificacao %>% filter(cluster == 1), by = "prdno") %>%
  group_by(data) %>%
  summarise(quant.venda = sum(quant)) %>%
  right_join(datas, by = c("data")) %>%
  mutate(vendas = replace_na(quant.venda, 0)) %>%
  arrange(data)

##########################################################################

produtos %>% select(grupo, depto, secao) %>%
  unique() %>%
  arrange(grupo, depto, secao) %>%
  View()

vendas.agrupadas.cluster <- vendas %>%
  merge(produtos %>% filter(grupo == "Moveis" & depto == "MOVEL/COLCHAO"), by = "prdno") %>%
  group_by(data) %>%
  summarise(quant.venda = sum(quant)) %>%
  right_join(datas, by = c("data")) %>%
  mutate(vendas = replace_na(quant.venda, 0)) %>%
  arrange(data)
