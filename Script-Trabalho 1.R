library(tidyverse)

#DECOMPOSI�AO

##para o id = 2044:

library(Mcomp)
data(M3)
id1 = 2044 #id1 da s�rie
s1 <- M3[[id1]]$x #dados de treinamento, para acessar a partein-sampleda s�rie temporal, isto �, a parte a ser utilizada para ajustar o modelo.
print(s1)

library(magrittr)
require(forecast)

#Via STL:
stl(s1, s.window='periodic') %>% plot( main='s.window=periodic' )
stl(s1, s.window=13) %>% plot( main='s.window=12' )

#via MSTL:
s1 %>% mstl(lambda = NULL) %>% plot
#obs: lambda = NULL => parametro lambda ignorado

s1 %>% mstl(lambda = "auto") %>% plot
#obs: lambda = "auto" => parametro lambda automaticamente estimado


##para o id = 2197:

library(Mcomp)
data(M3)
id2 = 2097 #id1 da s�rie
s2 <- M3[[id2]]$x #dados de treinamento, para acessar a partein-sampleda s�rie temporal, isto �, a parte a ser utilizada para ajustar o modelo.
print(s2)

library(magrittr)
require(forecast)

#Via STL:
stl(s2, s.window='periodic') %>% plot( main='s.window=periodic' )
stl(s2, s.window=13) %>% plot( main='s.window=12' )

#via MSTL:
s2 %>% mstl(lambda = NULL) %>% plot
#obs: lambda = NULL => parametro lambda ignorado

s2 %>% mstl(lambda = "auto") %>% plot
#obs: lambda = "auto" => parametro lambda automaticamente estimado



#ANALISE: Olhando a decomposi��o, preferi a s�rie d1!
#ANALISE: Olhando a decomposi��o acima, percebe-se uma tend�ncia linear crescente e, al�m disso, 
# � percept�vel a presen�a de sazonalidade, pois nota-se comportamentos que se repete ao longo do gr�fico.
#Em rela��o ao ru�do, aparenta ter media zero e vari�ncia constante, ou seja, a decomposi��o foi boa.

#################################################################################################

#MODELO ARIMA ADEQUADO

#A partir da decomposi��o, nota-se que a s�rie possui sazonalidade, dessa forma, o modelo mais adequado seria um modelo SARIMA.

##Diferen�as:
require(tseries)

###obs: como � uma serie com tendencia, obviamente a gente deve tomar pelo menos uma diferen�a simples:
##Primeira Diferen�a:
ndiffs(s1)
ds1 <- diff(s1)
ndiffs(ds1)

par(mfrow=c(3,1))
plot(ds1, main='ds1') #note que, quando a gente realiza a diferen�a simples, geralmente ela remove a tend�ncia, mas o padrao sazonal ainda ficou bem evidente e fica se repetindo ao longo do tempo
acf(ds1, lag =7*12) #note que, pros lags sazonais (1 ano, 2 anos, ...), a autocorrela��o � bastante significativa; se vc notar apenas o decaimento sazonal, ele � muito lento, lembrando um processo de raiz unitaria, nesse caso, raiz unitaria sazonal, ou seja, � muito proavel que ainda estejamos trabalhando com uma serie que nao � estacionaria, justamente por causa desses ciclos sazonais
pacf(ds1, lag = 7*12)

kpss.test(ds1) #hip�tese nula: s�rie estacion�ria
#obs: note que o teste nao conseguiu rejeitar h0 ; � muito comum isso acontecer, quando a serie tem raiz unitaria sazonal, o teste nao consegue identificar q isso � uma quebra da estacionairedade.
#Entao, esse tipo de teste, para esse caso, acaba sendo enganado na maioria das vezes
#MESMO COM O RESULTADO DO TESTE, VEMOS QUE A SERIE AINDA NAO � ESTACIONARIA APENAS COM UMA DIFEREN�A SIMPLES


###Diferen�a Sazonal:
require(forecast)

nsdiffs(ds1)
dsds1 <- diff(ds1,lag=12)
nsdiffs(dsds1)

par(mfrow=c(3,1)) 
plot(dsds1, main='dsds1') #essa serie, com a diferen�a simples e a diferen�a sazonal, ela aparenta ser estacionaria
acf(dsds1, lag=7*12) #ja nao amostra aquele comportamento de raiz unitaria sazonal
pacf(dsds1, lag=7*12) #ja nao amostra aquele comportamento de raiz unitaria sazonal

kpss.test(dsds1) #nao rejeita H0, s s�rie � estacion�ria

#AGORA ESTAMOS CONVENCIDOS DE QUE A SERIE � ESTACIONARIA.
#PORTANTO, A PROXIMA PREOCUPA��O � ACHAR UM MODELO PRA ESSA SERIE


################################
### Modelos Candidatos #########
################################

# Sabemos:

# d = 1, pois foi realizado uma diferen�a simples;
# D = 1, pois foi realizado uma diferen�a simples;
# p = 2 ou 4, pois, no PACF, ambos aparentam ter autocorrela��o significativo;
# q = 0 ou 1, pois, o ACF tem "quebra" no lag 1;
# P = 2, pois, no PACF, tem autocorrela��o significativa no lag sazonal 2
# Q = 1, pois, no ACF, tem autocorrela��o significativa no lag sazonal 1

#A partir dessas conclus�es, tem-se 8 modelos candidatos:
  
#modelo 1: SARIMA (4,1,0) x (2,1,1)
#modelo 2: SARIMA (4,1,1) x (2,1,1)
#modelo 3: SARIMA (2,1,0) x (2,1,1)
#modelo 4: SARIMA (2,1,1) x (2,1,1)


## Modelo 1: SARIMA (4,1,0) x (2,1,1)
mod1 <- arima(s1, order=c(4,1,0), seasonal = c(2,1,1), include.mean = FALSE)
mod1

#Res�duos:
E <- mod1$residuals
plot(E) #note a quantidade de zeros q ele usou pra ele inicializar o modelo, e isso ocorre pq ele usou a diferen�a sazonal, entao teve q dxar 12 pontos iguais a zero
#e dxar esse zeros pode comprometer a nossa analise de residuos, um exemplo � a normalidade, nossos residuos vao estar inflacionados de zero ai nao vai dar normalidade
#por conta disso, devemos tirar esses zeros antes de fazer as analises dos residuos

#Cortando os zeros:
E <- window(E,  start=time(s1)[14]) #cortando os zeros ate a observa��o 13, ou seja, a analise vai come�ar a partir da observa��o 14
plot(E)


## An�lise Visual:
par(mfrow=c(2,2))
plot(E)
acf(E) #quase todos ficaram iguais a zero -> ok
pacf(E, lag=12) #quase todos ficaram iguais a zero -> ok
qqnorm(E, lag=12) #a maioria dos pontos ficaram em cima da reta -> ok
qqline(E)

## Testes Estatisticos:
# Estacionaridade:
kpss.test(E) # hip�tese nula: s�rie estacion�ria
# Independencia:
Box.test(E, lag = 22, type ="Ljung-Box", fitdf = 7) ## use fitdf=p+q
# Normalidade:
shapiro.test(E)


#TUDO OK COM O PRIMEIRO MODELO? SIM!

## Modelo 2: SARIMA (4,1,1) x (2,1,1)
mod2 <- arima(s1, order=c(4,1,1), seasonal = c(2,1,1), include.mean = FALSE)
mod2

#Res�duos:
E <- mod2$residuals
plot(E) #note a quantidade de zeros q ele usou pra ele inicializar o modelo, e isso ocorre pq ele usou a diferen�a sazonal, entao teve q dxar 12 pontos iguais a zero
#e dxar esse zeros pode comprometer a nossa analise de residuos, um exemplo � a normalidade, nossos residuos vao estar inflacionados de zero ai nao vai dar normalidade
#por conta disso, devemos tirar esses zeros antes de fazer as analises dos residuos

#Cortando os zeros:
E <- window(E,  start=time(s1)[14]) #cortando os zeros ate a observa��o 13, ou seja, a analise vai come�ar a partir da observa��o 14
plot(E)


## An�lise Visual:
par(mfrow=c(2,2))
plot(E)
acf(E) #quase todos ficaram iguais a zero -> ok
pacf(E, lag=12) #quase todos ficaram iguais a zero -> ok
qqnorm(E, lag=12) #a maioria dos pontos ficaram em cima da reta -> ok
qqline(E)

## Testes Estatisticos:
# Estacionaridade:
kpss.test(E) # hip�tese nula: s�rie estacion�ria
# Independencia:
Box.test(E, lag = 23, type ="Ljung-Box", fitdf = 8) ## use fitdf=p+q
# Normalidade:
shapiro.test(E)


#TUDO OK COM O PRIMEIRO MODELO? SIM!

## Modelo 3: SARIMA (2,1,0) x (2,1,1)
mod3 <- arima(s1, order=c(2,1,0), seasonal = c(2,1,1), include.mean = FALSE)
mod3

#Res�duos:
E <- mod3$residuals
plot(E) #note a quantidade de zeros q ele usou pra ele inicializar o modelo, e isso ocorre pq ele usou a diferen�a sazonal, entao teve q dxar 12 pontos iguais a zero
#e dxar esse zeros pode comprometer a nossa analise de residuos, um exemplo � a normalidade, nossos residuos vao estar inflacionados de zero ai nao vai dar normalidade
#por conta disso, devemos tirar esses zeros antes de fazer as analises dos residuos

#Cortando os zeros:
E <- window(E,  start=time(s1)[14]) #cortando os zeros ate a observa��o 13, ou seja, a analise vai come�ar a partir da observa��o 14
plot(E)


## An�lise Visual:
par(mfrow=c(2,2))
plot(E)
acf(E) #quase todos ficaram iguais a zero -> ok
pacf(E, lag=12) #quase todos ficaram iguais a zero -> ok
qqnorm(E, lag=12) #a maioria dos pontos ficaram em cima da reta -> ok
qqline(E)

## Testes Estatisticos:
# Estacionaridade:
kpss.test(E) # hip�tese nula: s�rie estacion�ria
# Independencia:
Box.test(E, lag = 20, type ="Ljung-Box", fitdf = 5) ## use fitdf=p+q
# Normalidade:
shapiro.test(E)


#TUDO OK COM O PRIMEIRO MODELO? SIM!

## Modelo 4: SARIMA (2,1,1) x (2,1,1)
mod4 <- arima(s1, order=c(2,1,1), seasonal = c(2,1,1), include.mean = FALSE)
mod4

#Res�duos:
E <- mod4$residuals
plot(E) #note a quantidade de zeros q ele usou pra ele inicializar o modelo, e isso ocorre pq ele usou a diferen�a sazonal, entao teve q dxar 12 pontos iguais a zero
#e dxar esse zeros pode comprometer a nossa analise de residuos, um exemplo � a normalidade, nossos residuos vao estar inflacionados de zero ai nao vai dar normalidade
#por conta disso, devemos tirar esses zeros antes de fazer as analises dos residuos

#Cortando os zeros:
E <- window(E,  start=time(s1)[14]) #cortando os zeros ate a observa��o 13, ou seja, a analise vai come�ar a partir da observa��o 14
plot(E)


## An�lise Visual:
par(mfrow=c(2,2))
plot(E)
acf(E) #quase todos ficaram iguais a zero -> ok
pacf(E, lag=12) #quase todos ficaram iguais a zero -> ok
qqnorm(E, lag=12) #a maioria dos pontos ficaram em cima da reta -> ok
qqline(E)

## Testes Estatisticos:
# Estacionaridade:
kpss.test(E) # hip�tese nula: s�rie estacion�ria
# Independencia:
Box.test(E, lag = 21, type ="Ljung-Box", fitdf = 6) ## use fitdf=p+q
# Normalidade:
shapiro.test(E)


#TUDO OK COM O PRIMEIRO MODELO? SIM!



###########################################
### Criterio de sele��o de modelos: AIC ###
###########################################
mod1 
mod2
mod3
mod4 # --> escolhido pelo AIC

testes1 <- data.frame(Modelo = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
                     KPSS = c("0,1","0,1", "0,1", "0,1"), 
                     Ljung_Box = c("0,3682","0,591", "0,3417", "0,5257"),
                   Shapiro_Wilk = c("0,3833","0,2328", "0,2469", "0,2296"))

testes2 <- data.frame(Medidas = c("AIC", "Estimativa do Erro Padr�o"), 
                     Modelo_1 = c("1236,44","8156"), 
                     Modelo_2 = c("1236,88","7998"), Modelo_3 = c("1236,94","8664"),
                     Modelo_4 = c("1235,05","8309"))


#################################################################################################

M3[[id1]]$h # = 18

#PREVISAO PONTUAL

mod_w <- arima(s1, order=c(2,1,1), seasonal = c(2,1,1), include.mean = FALSE)
mod_s <- arima(s1, order=c(2,0,1), seasonal = c(2,0,1), include.mean = FALSE)

## Funcao base do R para previs�o
## previs�o s�rie {w}
prev_w <- predict(mod_w, 12) ## previs�o de 12 valores a frente de w_n

plot(cbind(s1,prev_w$pred) , plot.type = "single", col=c(1,2), ylab='W')

## previs�o s�rie {x}
prev_s1 <- predict(s1, 12) ## previs�o de 10 valores a frente de x_n

plot(cbind(s1,prev_s1$pred) , plot.type = "single", col=c(1,2), ylab='X')
prev_s1$pred

## a fun��o predict() retorna desvio padr�o condicional (erro padr�o), veja a equivalencia:
prev_w$pred
prev_w$se



#PREVISAO INTERVALAR

### s�rie {w}
LI <- prev_w$pred - 1.96*prev_w$se
LS <- prev_w$pred + 1.96*prev_w$se
plot(cbind(s1,prev_w$pred,LI,LS) , plot.type = "single", col=c(1,2,3,3), ylab='W')

### s�rie {x}
LI <- prev_x$pred - 1.96*prev_x$se
LS <- prev_x$pred + 1.96*prev_x$se
plot(cbind(x,prev_x$pred,LI,LS) , plot.type = "single", col=c(1,2,3,3), ylab='X')

##Com o pacote forecast:

### s�rie {w}
prev_w <- forecast(mod_w, h=18, level=c(90,95, 99))
prev_w
plot(prev_w, ylab='W')

### s�rie {x}
prev_x <- forecast(fit_x, h=18, level=c(80,95))
prev_x
plot(prev_x, ylab='X')

#########################################################################################

M3[[id1]]$h # = 18

#PREVISAO PONTUAL:
pre_pontual <- mod4 %>% predict(n.ahead = 18)
a <- data.frame(pre_pontual)
names(a) <- c("Previs�o Pontual", "Estimativa do Erro Padr�o")

#PREVIS�O INTERVALAR:
pi_90 <- mod4 %>% forecast (h= 18 , level =90)
pi_95 <- mod4 %>% forecast (h= 18 , level =95)
pi_99 <- mod4 %>% forecast (h= 18 , level =99)

#PREVISAO DE 90%:
a <- data.frame(pi_90)
names(a) <- c("Previs�o Pontual", "L.I (90%)", "L.S (90%)")

par(mfrow=c(1,1))
plot(mod4 %>% forecast(h=18 , level=90), main = "Previs�o (90%)", col = 2) 
lines(M3[[id1]]$xx, col = "darkgreen", type = 'l', lwd=1.5)
text(1984,5250,"---- out sample", col = "darkgreen")
text(1983.8,5100,"---- previs�o", col = 4)

#PREVISAO DE 95%:
a <- data.frame(pi_95)
names(a) <- c("Previs�o Pontual", "L.I (95%)", "L.S (95%)")

par(mfrow=c(1,1))
plot(mod4 %>% forecast(h=18 , level=95), main = "Previs�o (95%)", col = 2) 
lines(M3[[id1]]$xx, col = "darkgreen", type = 'l', lwd=1.5)
text(1984,5250,"---- out sample", col = "darkgreen")
text(1983.8,5100,"---- previs�o", col = 4)

#PREVISAO DE 99%:
a <- data.frame(pi_99)
names(a) <- c("Previs�o Pontual", "L.I (99%)", "L.S (99%)")

par(mfrow=c(1,1))
plot(mod4 %>% forecast(h=18 , level=99), main = "Previs�o (99%)", col = 2) 
lines(M3[[id1]]$xx, col = "darkgreen", type = 'l', lwd=1.5)
text(1984,5250,"---- out sample", col = "darkgreen")
text(1983.8,5100,"---- previs�o", col = 4)


#ACURACIA

## calculo do erro absoluto m�dio da previs�o:
MAE <- mean(abs(M3[[id1]]$xx-pre_pontual$pred))
MAE

#porcentagens de valores do teste que est�o dentro da previs�o intervalar
#em cada n�vel de confian�a:

porcent_90 <- (sum(M3[[id1]]$xx>pi_90$lower&M3[[id1]]$xx<pi_90$upper)/18)*100
porcent_95 <- (sum(M3[[id1]]$xx>pi_95$lower&M3[[id1]]$xx<pi_95$upper)/18)*100
porcent_99 <- (sum(M3[[id1]]$xx>pi_99$lower&M3[[id1]]$xx<pi_99$upper)/18)*100

a <- data.frame(Cobertura = c("90%", "95%", "99%"), 
                Porcentagem = c("66,67%","77,78%","100%"))


par(mfrow=c(1,1))
plot(mod4 %>% forecast(h=18 , level=95), main = "Previs�o (95%)", col = 2) 
lines(M3[[id1]]$xx, col = "darkgreen", type = 'l', lwd=1.5)
text(1984,5250,"---- out sample", col = "darkgreen")
text(1983.8,5100,"---- previs�o", col = 4)




























#############################################################################################
#mODELO AUTOMATICO:
k <- auto.arima(s1, allowdrift=FALSE)
plot(forecast(k))

mod <- arima(s1, order=c(1,1,0), seasonal = c(0,1,1), include.mean = FALSE)

modt <- mod %>% forecast(M3[[id1]]$h)  
MAE <- mean(abs(M3[[id1]]$xx - modt$mean))
plot(forecast(modt[[1]]))

M3[[id1]] %>% plot()
plot(modt$x)