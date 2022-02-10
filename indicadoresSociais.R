
pacotes <- c("BETS")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in seq_along(instalador)) {
    install.packages(instalador, dependencies = T)
    break() }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

desemprego <- BETSget(24369, from='2012-03-01', to='2017-11-01')
inflacao <- BETSget(13522, from='2013-03-01')

BETSget(24369, from='2012-03-01', to='2017-11-01')

BETS.get(4192)

BETS.search(description=  "desemprego")
BETSsearch(description = "sales ~ retail")
