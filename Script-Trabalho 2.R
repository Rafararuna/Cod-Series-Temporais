# importa??o dos dados:

pacman::p_load("Mcomp", "forecast", "ggplot2", "tseries", "knitr", "gridExtra","forecTheta")

## id1:
data(M3)
id <- 2209
M3[[id]]
h <- M3[[id]]$h
dados_treino <- M3[[id]]$x
dados_teste <- M3[[id]]$xx

## id2:
data(M3)
id <- 2768
M3[[id]]
h <- M3[[id]]$h
dados_treino <- M3[[id]]$x
dados_teste <- M3[[id]]$xx

# grafico da serie historica:

autoplot(dados_treino) +
  labs(x = "Ano", y = "N?mero de Vendas", title = "Vendas dos Aquecedores de ?gua a g?s") +
  theme_bw()

# decomposicao MSTL:
dcp_mstl <- mstl(dados_treino)
autoplot(dcp_mstl) +
  labs(x = "Ano", title = "Decomposi??o via MSTL") +
  theme_bw()

# estacionariedade:

kpss.test(dados_treino) #rejeita a hipotese nula
ndiffs(dados_treino) #precisa fazer uma diferenca simples 
x <- data.frame("Teste de KPSS" = c("0.01", "1"),
                row.names = c("P-valor", "ndiffs"))
kable(x, align = 'c')


# primeira diferenca

dif_treino_1 <- diff(dados_treino)

x <- autoplot(dados_treino) +
  labs(x = "Ano", y = "Valores", title = "S?rie Original") +
  theme_bw()

y <- autoplot(dif_treino_1) +
  labs(x = "Ano", y = "Valores", title = "S?rie com a Primeira diferenca") +
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 2)

kpss.test(dif_treino_1) #nao rejeita h0

x <- data.frame("Teste de KPSS" = c("0.1", "0"),row.names = c("P-valor", "ndiffs"))
kable(x, align = 'c')

# diferenca sazonal:

nsdiffs(dif_treino_1) #precisa fazer uma diferenca sazonal

x <- data.frame("nsdiffs" = c(1))
kable(x,align = 'c')

dif_treino_2 <- diff(dif_treino_1, lag = 12)

x <- autoplot(dif_treino_1) +
  labs(x = "Ano", y = "Valores", title = "S?rie com a Primeira diferenca") +
  theme_bw()

y <- autoplot(dif_treino_2) +
  labs(x = "Ano", y = "Valores", title = "S?rie com a diferenca Sazonal") +
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 2)

# acf e pacf:

par(mfrow = c(1, 2))
acf(dif_treino_2, lag.max = 5*12, main = "") #1,2,3
pacf(dif_treino_2, lag.max = 5*12, main = "") #1,3

# modelos arima:

# d: 1, pois teve uma diferenca simples
# D: 1, pois teve uma diferenca sazonal
# p: p=2 ou 4, pois ha uma quebra em ambos os lags; PACF
# P: 1 ou 0, pois apresenta autocorrelacao siginifcativa no primeiro lag sazonal; PACF
# q: q = 2 ou 3, pois ha uma quebra em ambos os lags; ACF
# Q: 1 ou 0, pois apresenta autocorrelacao siginifcativa no primeiro lag sazonal;ACF


mod1 <- arima(dados_treino, order = c(2, 1, 2), seasonal = c(0, 1, 0))
mod2 <- arima(dados_treino, order = c(2, 1, 2), seasonal = c(1, 1, 1))
mod3 <- arima(dados_treino, order = c(2, 1, 2), seasonal = c(0, 1, 1))
mod4 <- arima(dados_treino, order = c(2, 1, 2), seasonal = c(1, 1, 0))

mod5 <- arima(dados_treino, order = c(2, 1, 3), seasonal = c(0, 1, 0))
mod6 <- arima(dados_treino, order = c(2, 1, 3), seasonal = c(1, 1, 1))
mod7 <- arima(dados_treino, order = c(2, 1, 3), seasonal = c(0, 1, 1))
mod8 <- arima(dados_treino, order = c(2, 1, 3), seasonal = c(1, 1, 0))

mod9  <- arima(dados_treino, order = c(4, 1, 2), seasonal = c(0, 1, 0))
mod10 <- arima(dados_treino, order = c(4, 1, 2), seasonal = c(1, 1, 1))
mod11 <- arima(dados_treino, order = c(4, 1, 2), seasonal = c(0, 1, 1))
mod12 <- arima(dados_treino, order = c(4, 1, 2), seasonal = c(1, 1, 0))

mod13 <- arima(dados_treino, order = c(4, 1, 3), seasonal = c(0, 1, 0))
mod14 <- arima(dados_treino, order = c(4, 1, 3), seasonal = c(1, 1, 1))
mod15 <- arima(dados_treino, order = c(4, 1, 3), seasonal = c(0, 1, 1))
mod16 <- arima(dados_treino, order = c(4, 1, 3), seasonal = c(1, 1, 0))

# parcim?nia:

n <- length(dados_treino)
k <- c(4, 6, 5, 5,
       5, 7, 6, 6,
       6, 8, 7, 7,
       7, 9, 9, 8)

aic.arima <- unlist(lapply(list(mod1,mod2,mod3,mod4,
                                mod5, mod6, mod7, mod8,
                                mod9, mod10, mod11, mod12,
                                mod13, mod14, mod15, mod16), AIC))

aicc.arima <- aic.arima + (2*k^2 + 2*k)/(n - k - 1)

x <- data.frame("AIC" = aic.arima, "AICc" = aicc.arima,
                row.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4",
                              "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8",
                              "Modelo 9", "Modelo 10", "Modelo 11", "Modelo 12",
                              "Modelo 13", "Modelo 14", "Modelo 15", "Modelo 16"))

kable(x, caption = "Crit?rios de Informa??o de Akaike", align = 'c')


# modelos ets:

ets1 <- ets(dados_treino, model = "AAA", damped = F)
ets2 <- ets(dados_treino, model = "AAA", damped = T)
ets3 <- ets(dados_treino, model = "AMA", damped = F, restrict = F)
ets4 <- ets(dados_treino, model = "AMA", damped = T, restrict = F)
ets5 <- ets(dados_treino, model = "MAA", damped = F)
ets6 <- ets(dados_treino, model = "MAA", damped = T)
ets7 <- ets(dados_treino, model = "MMA", damped = F, restrict = F)
ets8 <- ets(dados_treino, model = "MMA", damped = T, restrict = F)
ets9 <- ets(dados_treino, model = "MAM", damped = F)
ets10 <- ets(dados_treino, model = "MAM", damped = T)
ets11 <- ets(dados_treino, model = "MMM", damped = F)
ets12 <- ets(dados_treino, model = "MMM", damped = T)

df <- data.frame("AIC" = c(ets1$aic, ets2$aic, ets3$aic, ets4$aic,
                           ets5$aic, ets6$aic, ets7$aic, ets8$aic,
                           ets9$aic, ets10$aic, ets11$aic, ets12$aic),
                 "AICc" = c(ets1$aicc, ets2$aicc, ets3$aicc, ets4$aicc,
                            ets5$aicc, ets6$aicc, ets7$aicc, ets8$aicc,
                            ets9$aicc, ets10$aicc, ets11$aicc, ets12$aicc),
                 row.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4",
                               "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8",
                               "Modelo 9", "Modelo 10", "Modelo 11", "Modelo 12"))

kable(df, caption = "Crit?rios de Informação de Akaike", align = 'c')

# Analise de Residuos:

## Modelo SARIMA

res_sarima <- window(mod15$residuals, start = time(treino)[13])

x <- autoplot(res_sarima) +
  labs(x = "Ano", y = "Resíduos") +
  ggtitle("Estacionariedade")+
  theme_bw()

y <- data.frame(res_sarima) %>%
  ggplot(aes(sample = res_sarima)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  ggtitle("Normalidade")+
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 1)

par(mfrow = c(1, 2))
acf(res_sarima, lag.max = 7*4, main = "Autocorrelações")
pacf(res_sarima, lag.max = 7*4, main = "Autocorrelações Parciais")


a <- kpss.test(res_sarima)
b <- shapiro.test(res_sarima)
c <- Box.test(res_sarima, lag = 23, type = "Ljung-Box", fitdf = 8)

x <- data.frame("Teste" = c("KPSS", "Shapiro-Wilk", "Ljung-Box"),
                "Hipótese" = c("Estacionariedade", "Normalidade", "Independência"),
                "Estatística" = round(c(a$statistic, b$statistic, c$statistic), 3),
                "p-valor" = round(c(a$p.value, b$p.value, c$p.value), 3),
                row.names = NULL)
kable(x, caption = "Testes de Hipóteses", align = 'c')

## Modelo ETS

res_ets <- window(ets9$residuals, start = time(dados_treino)[13])

x <- autoplot(res_ets) +
  labs(x = "Ano", y = "Resíduos") +
  ggtitle("Estacionariedade")+
  theme_bw()

y <- data.frame(res_ets) %>%
  ggplot(aes(sample = res_ets)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  ggtitle("Normalidade")+
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 1)

par(mfrow = c(1, 2))
acf(res_ets, lag.max = 7*4, main = "Autocorrelações")
pacf(res_ets, lag.max = 7*4, main = "Autocorrelações Parciais")


a <- kpss.test(res_ets)
b <- shapiro.test(res_ets)
c <- Box.test(res_ets, lag = 15, type = "Ljung-Box", fitdf = 2)

x <- data.frame("Teste" = c("KPSS", "Shapiro-Wilk", "Ljung-Box"),
                "Hipótese" = c("Estacionariedade", "Normalidade", "Independência"),
                "Estatística" = round(c(a$statistic, b$statistic, c$statistic), 3),
                "p-valor" = round(c(a$p.value, b$p.value, c$p.value), 3),
                row.names = NULL)
kable(x, caption = "Testes de Hipóteses")

# Capacidade Preditiva

origem <- length(dados_treino) - 14
erros_sarima <- data.frame()
erros_ets <- data.frame()
mes_fim <- 6
ano_fim <- 1992

for (i in 1:10) {
  serie_janela <- ts(dados_treino[1:origem], start = c(1982, 1),
                     end = c(ano_fim, mes_fim), frequency = 12)
  janela_ets <- ets(serie_janela, model = "MAM", damped = F)
  janela_sarima <- arima(serie_janela, order = c(4, 1, 3), seasonal = c(0, 1, 1))
  previsao_ets <- forecast(janela_ets, h = 5)
  previsao_sarima <- forecast(janela_sarima, h = 5)
  errosEts_janela <- dados_treino[(origem + 1):(origem + 5)] - previsao_ets$mean
  errosSarima_janela <- dados_treino[(origem + 1):(origem + 5)] - previsao_sarima$mean
  erros_ets <- rbind(erros_ets, errosEts_janela)
  erros_sarima <- rbind(erros_sarima, errosSarima_janela)
  origem <- origem + 1
  mes_fim <- mes_fim + 1
  
  if (mes_fim == 13){
    mes_fim <- 1
    ano_fim <- 1993
  }
}

colnames(erros_ets) <- c("e1", "e2", "e3", "e4", "e5")
colnames(erros_sarima) <- c("e1", "e2", "e3", "e4", "e5")
row.names(erros_ets) <- c("Passo 1", "Passo 2", "Passo 3", "Passo 4", "Passo 5",
                          "Passo 6","Passo 7","Passo 8","Passo 9","Passo 10")
row.names(erros_sarima) <- c("Passo 1", "Passo 2", "Passo 3", "Passo 4", "Passo 5",
                             "Passo 6","Passo 7","Passo 8","Passo 9","Passo 10")

erro_medio <- data.frame("Horizonte" = c(1:5),
                         "erros" = c(sum(abs(erros_sarima$e1)) / 10,
                                     sum(abs(erros_sarima$e2)) / 10,
                                     sum(abs(erros_sarima$e3)) / 10,
                                     sum(abs(erros_sarima$e4)) / 10,
                                     sum(abs(erros_sarima$e5)) / 10,
                                     sum(abs(erros_ets$e1)) / 10,
                                     sum(abs(erros_ets$e2)) / 10,
                                     sum(abs(erros_ets$e3)) / 10,
                                     sum(abs(erros_ets$e4)) / 10,
                                     sum(abs(erros_ets$e5)) / 10),
                         "modelo" = c(rep("SARIMA", 5), rep("ETS", 5)))

kable(erros_sarima, caption = "Validação cruzada por janela deslizante do Modelo SARIMA", align = 'c')
kable(erros_ets, caption = "Validação cruzada por janela deslizante do Modelo ETS", align = 'c')

ggplot(erro_medio, aes(x = erro_medio$Horizonte, y = erro_medio$erros))+
  geom_point(aes(color = erro_medio$modelo))+
  geom_line(aes(color = erro_medio$modelo))+
  scale_color_manual(values=c("#b32020", "#2036b3"),
                     name="Modelo",
                     labels=c("ETS", "SARIMA"))+
  theme_bw()+
  labs(x = "Horizonte de Previsão", y = "Erro Absoluto Médio do Modelo",
       title = "Erro Médio dos Modelos em cada Horizonte")

# Previsão

#previsao intervalar modelo sarima
pp_sarima <- mod15 %>% predict(n.ahead = 18)
pS_90 <- mod15 %>% forecast (h = 18 , level = 90)
pS_95 <- mod15 %>% forecast (h = 18 , level = 95)
pS_99 <- mod15 %>% forecast (h = 18 , level = 99)

x_sarima <- data.frame("LI99%" = round(pS_99$lower, 2),
                       "LI95%" = round(pS_95$lower, 2),
                       "LI90%" = round(pS_90$lower, 2),
                       "previsão" = round(pp_sarima$pred, 2),
                       "LS90%" = round(pS_90$upper, 2),
                       "LS95%" = round(pS_95$upper, 2),
                       "LS99%" = round(pS_99$upper, 2))

colnames(x_sarima) <- c("LI 99%", "LI 95%", "LI 90%", "Prev. Pontual",
                        "LS 90%", "LS 95%", "LS 99%")
kable(x_sarima, caption = "Previsões do Modelo SARIMA")

#previsao intervalar modelo ets
pE_90 <- ets9 %>% forecast (h = 18 , level = 90)
pE_95 <- ets9 %>% forecast (h = 18 , level = 95)
pE_99 <- ets9 %>% forecast (h = 18 , level = 99)

x_ets <- data.frame("LI99%" = round(pE_99$lower, 2),
                    "LI95%" = round(pE_95$lower, 2),
                    "LI90%" = round(pE_90$lower, 2),
                    "previsao" = round(pE_90$mean, 2),
                    "LS90%" = round(pE_90$upper, 2),
                    "LS95%" = round(pE_95$upper, 2),
                    "LS99%" = round(pE_99$upper, 2))

colnames(x_ets) <- c("LI 99%", "LI 95%", "LI 90%", "Prev. Pontual",
                     "LS 90%", "LS 95%", "LS 99%")
kable(x_ets, caption = "Previs?es do Modelo ETS")

#previsao pontual modelo sarima
x <- autoplot(ts(c(dados_treino, dados_teste), start = c(1983, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo SARIMA")+
  theme_bw() +
  autolayer(pS_90$mean, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#2036b3"), breaks = c("Previsão"),
                      name = "")

#previsao pontual modelo ets
y <- autoplot(ts(c(dados_treino, dados_teste), start = c(1983, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo ETS")+
  theme_bw() +
  autolayer(pE_90$mean, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#b32020"), breaks = c("Previsão"),
                      name = "")

grid.arrange(x, y, nrow = 2)

#fazendo grafico pra cada banda

xS <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 90% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_90, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

xE <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 90% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_90, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

yS <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 95% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_95, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

yE <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 95% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_95, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

zS <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 99% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_99, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

zE <- autoplot(dados_treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 99% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_99, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

grid.arrange(xS, xE, yS, yE, zS, zE, nrow = 3, newpage = F)

# modelo M:

M <- (pS_90$mean + pE_90$mean)/2
kable(data.frame("Horizonte" = c(1:18), "Prev. Pontual" = M),
      caption = "Previsões do Modelo M", align = 'c')

autoplot(ts(c(dados_treino, dados_teste), start = c(1982, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo M")+
  theme_bw() +
  autolayer(M, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#e6bf00"),
                      breaks = c("Previsão"), name = "")

# acuracia do modelo:

autoarima_pred <- forecast(auto.arima(dados_treino), h = h)
ses_pred <- ses(dados_treino, h = h)
holt_pred <- holt(dados_treino , h = h)
autoets_pred <- forecast(ets(dados_treino), h = h)
stlf_pred <- stlf(dados_treino, h = h)
bats_pred <- forecast(bats(dados_treino), h = h)
tbats_pred <- forecast(tbats(dados_treino), h = h)
lista <- list(pS_95, pE_95, autoarima_pred, ses_pred, holt_pred,
              autoets_pred, stlf_pred, bats_pred, tbats_pred)

mae <- unlist(lapply(lista, function(x) return(mean(abs(dados_teste - x$mean)))))

MAE <- data.frame("MAE" = mae)
MAE <- rbind(mean(abs(dados_teste - M)), MAE)

row.names(MAE) <- c("Modelo M", "Modelo SARIMA Manual", "Modelo ETS Manual",
                    "auto.arima", "SES", "Holt", "ETS Automático",
                    "stlf", "bats", "tbats")

kable(MAE, caption = "Comparação de benchmarks dos modelos", align = 'c')

