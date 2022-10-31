library(AER)
#install.packages('strucchange')
library(strucchange)
library(fBasics)
library(quantreg)



##### Questão 0 #####
data <- read.table('ArquivoExercicio3.csv',sep=';',dec='.',header = T)

#Div: valor de dividendos pagos pela empresa
#ValorMercado: valor da empresa segundo o mercado
#PL: Patrimônio Líquido da empresa
#Passivo: Passivo da empresa (Passivo de CP + Passivo de LP)
#AtivoTotal: Valor do Ativo da empresa
#LL: Lucro Líquido da empresa
# Significancia = 5%

##### Questão 1 #####
data$BtM <- data$PL / data$ValorMercado
data$RPLP <- data$PL / data$Passivo
data$ROA <- data$LL / data$AtivoTotal
data$ROE <- data$LL / data$PL

##### Questão 2 #####
reg <- lm(Div ~ BtM + RPLP + ROA + ROE + AtivoTotal, data = data)
summary(reg)

##### Questão 3 #####
vif(reg)
vcovHC(reg-)