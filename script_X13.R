### Carrengando o pacote seasonal
install.packages("seasonal")
library(seasonal)

## Chacando o pacote
checkX13()

## Serie estudada
venda.ts
plot(venda.ts)

monthplot(venda.ts, col.base =2, lty.base = 2)
legend("topleft", legend = c("pim", "média"))

## Ajuste automático
options(digits=4, scipen = 30)
ajuste <- seas(venda.ts)
summary(ajuste)

qs(ajuste)
plot(ajuste)
plot(venda.ts)

data.frame(series(ajuste, "sp0"))
