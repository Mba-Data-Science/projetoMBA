pacotes <- c("RSelenium", "tidyverse", "foreach", "doParallel", "flock", "DBI", "RSQLite")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

zoom_firefox <- function(client, percent)
{
  store_page <- client$getCurrentUrl()[[1]]
  client$navigate("about:preferences")
  webElem <- client$findElement("css", "#defaultZoom")
  webElem$clickElement()
  webElem$sendKeysToElement(list(as.character(percent)))
  webElem$sendKeysToElement(list(key = "return"))
  client$navigate(store_page)
}

openDriverFirefox <- function(num) {
  fprof <- makeFirefoxProfile(list(
    "webdriver.load.strategy" = "eager"
  ))
  rD <- rsDriver(browser = "firefox", port = 4545L + num * 1L, verbose = FALSE, extraCapabilities = fprof)
  driver <- rD[["client"]]
  driver$setTimeout(type = "page load", milliseconds = 10000)
  driver$setWindowSize(800, 800)
  url <- "https://www.google.com.br/maps"
  driver$navigate(url)
  zoom_firefox(driver, 500)
  driver$screenshot(display = TRUE)
  return(list(driver, rD))
}

findCoords <- function(url) {
  coords <- str_match(url, "@(-5[/.0-9]+,-42[/.0-9]+)")[2]
  coordRet <- if (is.na(coords)) "" else coords
  if (coordRet == "")
    print(url)
  return(coordRet)
}

addressToCoords <- function(driver, address) {
  url <- "https://www.google.com.br/maps"
  driver$navigate(url)

  webElem <- driver$findElement(using = 'id', "searchboxinput")
  while (is.null(webElem)) {
    webElem <- driver$findElement(using = 'id', "searchboxinput")
    randsleep <- sample(seq(1, 3, by = 0.001), 1)
    Sys.sleep(randsleep)
  }
  webElem$sendKeysToElement(list(address, key = "enter"))

  timeout <- 0

  while ((driver$getCurrentUrl() %>% unlist()) == url) {
    randsleep <- sample(seq(1, 3, by = 0.001), 1)
    Sys.sleep(randsleep)
    timeout <- timeout + 1
    if(timeout > 4)
      url <- driver$getCurrentUrl() %>% unlist()
  }

  currentUrl <- driver$getCurrentUrl() %>% unlist()
  currentCoord <- findCoords(currentUrl)

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
      num = 1:n(),
      nome = nome,
      endereco = paste0(endereco, ", ", municipio, ", ", ifelse(uf == "PI", "PIAUÍ", ifelse(.$uf == "MA", "MARANHÃO", "")), ", BRASIL"),
      coordenadas = ""
    )

  con <- dbConnect(RSQLite::SQLite(), "dados.sqlite")
  dbListTables(con)
  dbWriteTable(con, "enderecos", df.enderecos, overwrite = TRUE)
}

## enderecos.clear()

######################################################################################################################
## carrega os enderecos
######################################################################################################################

con <- dbConnect(RSQLite::SQLite(), "dados.sqlite")

df.enderecos <- dbReadTable(con, "enderecos") %>% filter(coordenadas == "")

dbDisconnect(con)

df.enderecos %>%
  filter(coordenadas != "") %>%
  count()
df.enderecos %>%
  count()

driver_vector <- list(
  openDriverFirefox(10L),
  openDriverFirefox(20L),
  openDriverFirefox(30L),
  openDriverFirefox(40L)
)

Sys.sleep(5)

numCores <- driver_vector %>% length()

print(numCores)

registerDoParallel(numCores)
#registerDoSEQ()

foreach(row = seq_len(nrow(df.enderecos))) %dopar% {
  if (df.enderecos[row, 4] == "") {
    print(row)
    numDv <- row %% numCores + 1
    driver <- driver_vector[[numDv]]
    coords <- addressToCoords(driver[[1]], df.enderecos[row, 3])
    df.enderecos[row, 4] <- coords
    print(c(row, numDv))

    print("open")
    con <- dbConnect(RSQLite::SQLite(), "dados.sqlite")
    #dbBegin(con)
    dbExecute(con, "UPDATE enderecos set coordenadas = ? where num = ?", params = list(df.enderecos[row, 4], df.enderecos[row, 1]))
    #dbCommit(con)
    dbDisconnect(con)
    print("close")
  }
}

stopImplicitCluster()


driver <- openDriverFirefox(10L)

endereco <- "AVENIDA JOSE FRANCISCO DE ALMEIDA NETO, 4392, DIRCEU ARCOVERDE II, TERESINA, PIAUI"

addressToCoords(driver[[1]], endereco)
