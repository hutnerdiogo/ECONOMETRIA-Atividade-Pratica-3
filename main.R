library(AER)
#install.packages('strucchange')
library(strucchange)
library(fBasics)
library(quantreg)
library(quantmod)
library(stargazer)
#install.packages("tseries")
library(tseries)



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
#'A partir dos dados brutos, construa os seguintes indicadores e inclua-os no dataframe original.
#'Em seguida, gere o resumo estatístico da base de dados Dica: se usar o comando base$X<-y, é criada
#'uma nova coluna na tabela com nome de X contendo a variável y.
#'
#' a) Book-to-Market= 𝑃𝐿/𝑉𝑎𝑙𝑜𝑟𝑀𝑒𝑟𝑐𝑎𝑑𝑜 com nome de BtM
#'b) Relação PL e Passivo = 𝑃𝐿/𝑃𝑎𝑠𝑠𝑖𝑣𝑜 com nome de RPLP
#'c) ROA = 𝐿𝐿 / 𝐴𝑡𝑖𝑣𝑜𝑇𝑜𝑡𝑎𝑙 com nome de ROA
#'d) ROE= 𝐿𝐿 /𝑃𝐿 com nome de ROE
data$BtM <- data$PL / data$ValorMercado
data$RPLP <- data$PL / data$Passivo
data$ROA <- data$LL / data$AtivoTotal
data$ROE <- data$LL / data$PL

##### Questão 2 #####
#'Estime o modelo de regressão múltipla a seguir e analise os coeficientes, R^2, R^2 ajustado e o Teste F:
#' Div𝑖 = 𝛼 + 𝛽1BtM𝑖 + 𝛽2RPLP𝑖 + 𝛽3ROA𝑖 + 𝛽4ROE𝑖 + 𝛽5AtivoTotal
reg <- lm(Div ~ BtM + RPLP + ROA + ROE + AtivoTotal, data = data)
summary(reg)
#'

##### Questão 3 #####
#' Calcule e analise o Fator da Inflação da Variância e a matriz de covariância dos coeficientes
#' do modelo estimado. Analise os resultados e conclua sobre a adequabilidade da modelagem.
vif(reg)
vcov(reg)

##### Questão 4 #####
#'Reestime o modelo retirando o ROA e analise o Fator da Inflação da Variância e a matriz de
#'covariância dos coeficientes do modelo estimado. Ademais, realize o teste ANOVA para comparar
#'os dois modelos, sendo este modelo sem a variável ROA o modelo restrito.
reg2 <- lm(Div ~ BtM + RPLP + ROE + AtivoTotal, data = data)

vif(reg2)
vcov(reg2)
anova(reg,reg2)
#' Anova, hipotese nula é o modelo restrito melhor que o irrestrito. nesse caso se o p valor for abaixo de 5% o modelo
#' sem o ROA é melhor que o modelo com o ROA
##### Questão 5 #####
#'Analise os coeficientes, o R^2 Ajustado e a estatística F do modelo.
summary(reg2)

##### Questão 6 #####
#'Crie o gráfico de dispersão, histograma e o gráfico quantil-quantil dos resíduos da regressão.
#'Analise os gráficos gerados.
par(mfrow=c(2, 2))
plot(reg2$residuals)
hist(reg2$residuals)
qqPlot(reg2$residuals)

##### Questão 7 #####
#'Realize e analise os seguintes testes de normalidade nos resíduos da regressão, citando a H0 e
#'H1 de cada um:
#'a) Teste de Shapiro
shapiro.test(reg2$residuals)
#'b) Teste Jarque-Bera
jarque.bera.test(reg2$residuals)
#'c) Teste de Breusch-Pagan
bptest(reg2)
#'d) Teste de Durbin-Watson
dwtest(reg2)

##### Questão 8 #####
#'Realize os testes de heterocedasticidade (White e Breusch-Pagan) para verificar a
#'heterocedasticidade nos resíduos do modelo e analise o resultado dos mesmos.7
#Teste BP
bptest(reg2)
reg3 <- lm(Div ~
             BtM * RPLP +
               BtM * ROE +
               BtM * AtivoTotal +
               RPLP * ROE +
               ROE * AtivoTotal
               + I(AtivoTotal^2) + I(BtM^2)
               + I(RPLP^2) + I(ROE^2), data = data)
#TESTE WHITEEE <-----
bptest(reg3)

##### Questão 9 #####
#'Realize o teste RESET para verificar problemas de forma funcional no modelo e analise o
#'resultado do teste. Conclua sobre a validação do modelo estimado.
reset(reg2)
# Considerando o nivel de significancia como 5%, o p valor dele ter sido 0.001, podemos recusar a hipotese nula
# ou seja, o modelo proprio sem potencias é melhor que um modelo com potencias

##### Questão 10 #####
#'Estime a matriz de covariância com erros padrão de White e o valor dos coeficientes corrigidos.
#'Dica: procure sobre a função “coeftest”.
# Erros padrões de White
vcovHC(reg2)
#Valores corrigidos
vcov(reg2)

##### Questão 11 #####
#'Estime o modelo de regressão múltipla a seguir e analise os coeficientes, R2, R2 ajustado e o Teste F:
#'log(Div𝑖) = 𝛼 + 𝛽1BtM𝑖 + 𝛽2RPLP𝑖 + 𝛽3ROE𝑖 + 𝛽4log(AtivoTotal𝑖 )

reg4 <- lm(log(Div) ~ BtM + RPLP + ROE + log(AtivoTotal), data = data)
summary(reg4)

##### Questão 12 #####
#'Refaça os testes propostos nas questões 7,8 e 9. Conclua sobre a validade da modelagem e os
#'efeitos da reespecificação do modelo.

#' Questão 7
#'Realize e analise os seguintes testes de normalidade nos resíduos da regressão, citando a H0 e
#'H1 de cada um:
#'a) Teste de Shapiro
shapiro.test(reg4$residuals)
#'b) Teste Jarque-Bera
jarque.bera.test(reg4$residuals)
#'c) Teste de Breusch-Pagan
bptest(reg4)
#'d) Teste de Durbin-Watson
dwtest(reg4)


#' Questão 8
#'Realize os testes de heterocedasticidade (White e Breusch-Pagan) para verificar a
#'heterocedasticidade nos resíduos do modelo e analise o resultado dos mesmos.
#Teste BP
bptest(reg4)
reg4White <- lm(log(Div) ~
             BtM * RPLP +
               BtM * ROE +
               BtM * log(AtivoTotal) +
               RPLP * ROE +
               ROE * log(AtivoTotal)
               + I(log(AtivoTotal)^2) + I(BtM^2)
               + I(RPLP^2) + I(ROE^2), data = data)
#TESTE WHITEEE <-----
bptest(reg4White)


#' Questão 9
#'Realize o teste RESET para verificar problemas de forma funcional no modelo e analise o
#'resultado do teste. Conclua sobre a validação do modelo estimado.
reset(reg4)

##### Questão 13 #####
#'Faça análise gráfica e estatística para presença de outliers.
par(mfrow=c(2,2))
plot(reg4)
par(mfrow=c(1,2))
hist(reg4$residuals)
a<- qqPlot(reg4)

##### Questão 14 #####
#' Reestime o modelo excluindo os outliers e faça uma tabela comparativa dos modelos com e
#' sem outliers. Analise a robustez do modelo
outliers <- c(32,40,49)
dataSemOutliers <- data[-outliers,]
reg4SemOutliers <- lm(log(Div) ~ BtM + RPLP + ROE + log(AtivoTotal), data = data[-outliers,])
stargazer(reg4,reg4SemOutliers,type="text",column.labels = c("Com Outliers", "Sem Outliers"))

summary(reg4)
summary(reg4SemOutliers)
robustes(data,reg4, c(3,2))
robustes(dataSemOutliers,reg4, c(3,2))

robustes <- function(data,lm, vector_area) {
  results <- matrix(,
    nrow = 10000,
    ncol = length(names(lm$coefficients)))
  name_coeficientes <- names(lm$coefficients)

  colnames(results) <- name_coeficientes
  for (i in 1:10000) {
    index_amostras <- sample(1:dim(data)[1],size= dim(data)[1], T)
    amostra <- data[index_amostras,]
    mod <- lm(log(Div) ~ BtM + RPLP + ROE + log(AtivoTotal), data = amostra)
    results[i,] <- mod$coefficients
  }
  par(mfrow = vector_area,
      mar = c(2, 2, 2, 2))
  name <- name_coeficientes[1]
  Hist <- hist(results[, name], plot = F, breaks = 100)
  plot(Hist, main = name, xlab = "", col = ifelse(Hist$breaks <= quantile(results[, name], 0.025), "red", ifelse(Hist$breaks >= quantile(results[, name], 0.975), "red", "white")),
       xlim = c(-9,-5))

  name <- name_coeficientes[2]
  Hist <- hist(results[, name], plot = F, breaks = 100)
  plot(Hist, main = name, xlab = "", col = ifelse(Hist$breaks <= quantile(results[, name], 0.025), "red", ifelse(Hist$breaks >= quantile(results[, name], 0.975), "red", "white")),
       xlim = c(-0.8,-0.2))

  name <- name_coeficientes[3]
  Hist <- hist(results[, name], plot = F, breaks = 100)
  plot(Hist, main = name, xlab = "", col = ifelse(Hist$breaks <= quantile(results[, name], 0.025), "red", ifelse(Hist$breaks >= quantile(results[, name], 0.975), "red", "white")),
       xlim = c(-1,2))

  name <- name_coeficientes[4]
  Hist <- hist(results[, name], plot = F, breaks = 100)
  plot(Hist, main = name, xlab = "", col = ifelse(Hist$breaks <= quantile(results[, name], 0.025), "red", ifelse(Hist$breaks >= quantile(results[, name], 0.975), "red", "white")),
       xlim = c(-0.6,0.6))

  name <- name_coeficientes[5]
  Hist <- hist(results[, name], plot = F, breaks = 100)
  plot(Hist, main = name, xlab = "", col = ifelse(Hist$breaks <= quantile(results[, name], 0.025), "red", ifelse(Hist$breaks >= quantile(results[, name], 0.975), "red", "white")),
       xlim = c(1,1.3))
}
rmarkdown::pandoc_available()
rmarkdown::pandoc_exec()
install.packages("pandoc")