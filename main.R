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
summary(reg2)
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
plot(reg2,2)
hist(reg2)
qqPlot(reg2)

##### Questão 7 #####
#'Realize e analise os seguintes testes de normalidade nos resíduos da regressão, citando a H0 e
#'H1 de cada um:
#'a) Teste de Shapiro
#'b) Teste Jarque-Bera
#'c) Teste de Breusch-Pagan
#'d) Teste de Durbin-Watson


##### Questão 8 #####
#'Realize os testes de heterocedasticidade (White e Breusch-Pagan) para verificar a
#'heterocedasticidade nos resíduos do modelo e analise o resultado dos mesmos.



##### Questão 9 #####
#'Realize o teste RESET para verificar problemas de forma funcional no modelo e analise o
#'resultado do teste. Conclua sobre a validação do modelo estimado.
reset(reg2)

##### Questão 10 #####
#'Estime a matriz de covariância com erros padrão de White e o valor dos coeficientes corrigidos.
#'Dica: procure sobre a função “coeftest”.


##### Questão 11 #####
#'Estime o modelo de regressão múltipla a seguir e analise os coeficientes, R2, R2 ajustado e o Teste F:
#'log(Div𝑖) = 𝛼 + 𝛽1BtM𝑖 + 𝛽2RPLP𝑖 + 𝛽3ROE𝑖 + 𝛽4log(AtivoTotal𝑖 )

reg3 <- lm(log(Div) ~ BtM + RPLP + ROE + log(AtivoTotal), data = data)

##### Questão 12 #####
#'Refaça os testes propostos nas questões 7,8 e 9. Conclua sobre a validade da modelagem e os
#'efeitos da reespecificação do modelo.

#' Questão 7
#'Realize e analise os seguintes testes de normalidade nos resíduos da regressão, citando a H0 e
#'H1 de cada um:
#'a) Teste de Shapiro
#'b) Teste Jarque-Bera
#'c) Teste de Breusch-Pagan
#'d) Teste de Durbin-Watson

#' Questão 8
#'Realize os testes de heterocedasticidade (White e Breusch-Pagan) para verificar a
#'heterocedasticidade nos resíduos do modelo e analise o resultado dos mesmos.

#' Questão 9
#'Realize o teste RESET para verificar problemas de forma funcional no modelo e analise o
#'resultado do teste. Conclua sobre a validação do modelo estimado.


##### Questão 13 #####
#'Faça análise gráfica e estatística para presença de outliers.

##### Questão 14 #####
#' Reestime o modelo excluindo os outliers e faça uma tabela comparativa dos modelos com e
#'sem outliers. Analise a robustez do modelo