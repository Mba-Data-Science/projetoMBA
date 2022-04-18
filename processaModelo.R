################################################################################
# Carrega pacotes
################################################################################
options(digits = 4, scipen = 30)

library(tidyverse)
library(forecast)
library(urca)
library(zoo)
library(FinTS)
library(foreach)
library(future)

################################################################################
# Funções
################################################################################

#' @title transforma um vetor no formato c(ano, mes) em string no formato 
#' "mes/ano"
formatYM <- function(cym) {
  return(paste0(cym[2], '/', cym[1]))
}

################################################################################
# Processamento
################################################################################

vendasGrupo <- readRDS("vendas.grupo.rds")

