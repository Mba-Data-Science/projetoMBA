######################################################################################################################
# Carrega pacotes
######################################################################################################################

pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR", "scales",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

######################################################################################################################
# Funções
######################################################################################################################


######################################################################################################################
# Carrega dados
######################################################################################################################

df.vendas.bruta <-read.csv(file = "dados/pintos/vendas.csv")

str(df.vendas.bruta)

df.vendas <- df.vendas.bruta %>% mutate(
  data = data %>% as.character() %>% as.Date(format = "%Y%m%d")
)

str(df.vendas)

df.venda.mes <- df.vendas %>% 
  group_by(month = lubridate::floor_date(data, "month")) %>%
  summarize(vendas = sum(sumValor))

head(df.venda.mes)

ts=ts(df.venda.mes$vendas)

plot(ts)

scales::
  
point <- function(x) comma(x, big.mark = ".")

ggplotly(
  df.venda.mes %>%
    ggplot() + scale_y_continuous(labels = point)+
    geom_line(aes(x = month, y = vendas, color = "série")) +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)
