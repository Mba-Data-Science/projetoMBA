# Instalação e Carregamento de Todos os Pacotes ---------------------------
# Rotina prof. Rafael Souza e Prof Fávero

pacotes <- c("tidyverse", "forecast", "urca", "zoo", "FinTS", "foreach", "future",
             "broom", "modelr", "reprex", "rvest", "stringr")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}
