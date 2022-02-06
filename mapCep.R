pacotes <- c("RSelenium", "tidyverse", "foreach", "doParallel")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

openDriverFirefox <- function(num) {
  rD <- rsDriver(browser = "firefox", port = 4545L + num, verbose = FALSE)
  diver <- rD[["client"]]
  diver$setWindowSize(1, 1)
  return(list(diver, rD))
}

findCoords <- function(url) {
  coords <- str_match(url, "@([-/.0-9]+,[-/.0-9]+)")[2]
  return(if (is.na(coords)) "" else coords)
}

addressToCoords <- function(driver, address) {
  url <- "https://www.google.com.br/maps"
  driver$navigate(url)
  coord <- ""
  webElem <- NULL
  while (is.null(webElem)) {
    webElem <- driver$findElement(using = 'id', "searchboxinput")
    randsleep <- sample(seq(0.5, 1, by = 0.001), 1)
    Sys.sleep(randsleep)
  }

  webElem$sendKeysToElement(list(address, key = "enter"))
  currentUrl <- driver$getCurrentUrl() %>% unlist()
  currentCoord <- findCoords(currentUrl)
  while (coord == currentCoord) {
    currentUrl <- driver$getCurrentUrl() %>% unlist()
    currentCoord <- findCoords(currentUrl)
    randsleep <- sample(seq(0.5, 1, by = 0.001), 1)
    Sys.sleep(randsleep)
  }
  return(currentCoord)
}


#remDr <- openDriverFirefox()

#endereco <- "Rua VENEZUELA, 1914 - CIdade Nova, Tereina PI"
#print(endereco)
#addressToCoords(remDr, endereco)

#endereco <- "Rua Alvaro Mendes, 1237 - Centro, Tereina PI"
#print(endereco)
#addressToCoords(remDr, endereco)

#endereco <- "Butantã, São Paulo - SP, 05508-010"
#print(endereco)
#addressToCoords(remDr, endereco)

######################################################################################################################
## tablaha os enderecos dos estabelacimentos
######################################################################################################################

enderecos.clear <- function() {
  df.estabelacimento <- readRDS("df.estabelacimento.rda")

  df.enderecos <- df.estabelacimento %>%
    transmute(
      endereco = paste(endereco, municipio, uf),
      coordenadas = ""
    )

  df.enderecos %>% saveRDS("df.enderecos.rda")
}

## enderecos.clear()

######################################################################################################################
## carrega os enderecos
######################################################################################################################

df.enderecos <- readRDS("df.enderecos.rda")

df.enderecos %>%
  filter(coordenadas != "") %>%
  count()
df.enderecos %>%
  count()

driver_vector <- list(openDriverFirefox(10L), openDriverFirefox(20L),
                      openDriverFirefox(30L), openDriverFirefox(40L))
Sys.sleep(5)

numCores <- driver_vector %>% length()

registerDoParallel(numCores)
foreach(row = seq_len(nrow(df.enderecos))) %dopar% {
  if (df.enderecos[row, 2] == "") {
    numDv <- row %% numCores + 1
    driver <- driver_vector[[numDv]]
    coords <- tryCatch({ addressToCoords(driver[[1]], df.enderecos[row, 1]) }, error = function(cond) {
      print(cond)
      return("")
    })
    df.enderecos[row, 2] <- coords
    if ((row %% 200) == 0) {
      print("Salva arquivo")
      df.enderecos %>% saveRDS("df.enderecos.rda")
    }
    print(c(row, numDv))
  }
  print(row)
}


df.enderecos %>% saveRDS("df.enderecos.rda")
