################################################################################
# Carrega dados
################################################################################
## produtos
descricaoGrupos <- read.csv("database/descricao_grupos.csv",
                            colClasses = c("codigo.grupo" = "integer"))

produtos <- read.csv(file = "database/bd_prd.csv.gz", sep = ";",
                     colClasses = c("prdno" = "character",
                                    "sku" = "character")) %>%
  mutate(grupo = as.factor(grupo)) %>%
  merge(descricaoGrupos, by = "grupo") %>%
  mutate(grupo = as.factor(descricao.grupo))

## vendas
vendas <- read.csv(file = "database/bd_vendas.csv.gz", sep = ";",
                   colClasses = c("data" = "character",
                                  "loja" = "character",
                                  "prdno" = "character",
                                  "valor" = "double")) %>%
  mutate(data = data %>%
    as.Date(format = "%Y%m%d") %>%
    yearmonth()) %>%
  filter(loja %in% c('1', '3', '5', '8', '9', '11')) %>%
  mutate(loja = paste("Loja", f_pad_left(loja, "0", 2))) %>%
  filter(year(data) %>% between(2009, 2021))

################################################################################
# Processa dados
################################################################################

datas <- vendas %>%
  select(data) %>%
  unique()

lojas <- vendas %>%
  select(loja) %>%
  unique()

grupos <- produtos %>%
  select(grupo) %>%
  unique()

datasGruposLoja <- merge(datas, lojas) %>% merge(grupos)

vendasAgrupadas <- vendas %>%
  merge(produtos, by = "prdno") %>%
  group_by(data, grupo, loja) %>%
  summarise(
    quant.venda = sum(quant),
    valor.venda = sum(valor)
  )

vendasGrupoLoja <- datasGruposLoja %>%
  left_join(vendasAgrupadas, by = c("data", "grupo", "loja")) %>%
  mutate(
    quant.venda = replace_na(quant.venda, 0), 
    valor.venda = replace_na(valor.venda, 0.00)
    ) %>%
  arrange(data, grupo, loja) %>%
  select(data, grupo, loja, quant.venda, valor.venda)

## totalização de grupos

totalLojaGrupo <- vendasGrupoLoja %>%
  group_by(loja, grupo) %>%
  summarise(
    valor.venda = sum(valor.venda),
  )

totalGrupo <- totalLojaGrupo %>%
  group_by(grupo) %>%
  summarise(
    valor.venda = sum(valor.venda)
  )

vendasPorcentagem <- totalLojaGrupo %>%
  inner_join(totalGrupo, by = "grupo") %>%
  mutate(loja, grupo, porcentagem = valor.venda.x*100/valor.venda.y) %>%
  select(loja, grupo, porcentagem)

vendasGrupoLojaAtivo <- vendasGrupoLoja %>%
  inner_join(vendasPorcentagem, by = c("grupo", "loja")) %>%
  filter(porcentagem >= 1.00) %>%
  select(data, loja, grupo, quant.venda, valor.venda) %>%
  group_by(data, grupo, loja) %>%
  summarise(
    quant.venda = sum(quant.venda),
    valor.venda = sum(valor.venda)
  )

vendasGrupoTodasLoja <- vendasGrupoLojaAtivo %>%
  select(data, loja, grupo, quant.venda, valor.venda) %>%
  group_by(data, grupo, loja = "TODAS") %>%
  summarise(
    quant.venda = sum(quant.venda),
    valor.venda = sum(valor.venda)
  )

vendasGrupo <- rbind(vendasGrupoLojaAtivo, vendasGrupoTodasLoja) %>%
  arrange(loja, grupo, data)

saveRDS(vendasGrupo, "vendas.grupo.rds")



