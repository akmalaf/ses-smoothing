mpdw3 <- read.csv('/Users/Mala/Documents/datasesd1d2.csv')
mpdw3.d1.ts <- ts(mpdw3$Demand_1)
mpdw3.d2.ts <- ts(mpdw3$Demand_2)
plot(mpdw3.d1.ts, main = 'Data Aktual')
points(mpdw3.d1.ts)

library(forecast)

SES.1 <- HoltWinters(mpdw3.d1.ts, alpha = 0.1, beta = FALSE, gamma = FALSE)
xhat.1 <- SES.1$fitted[,1]
error.1 <- residuals(SES.1)
SSE.1 <- SES.1$SSE
MSE.1 <- mean(error.1^2)
MAD.1 <- mean(abs(error.1))
RMSE.1 <- sqrt(MSE.1)
MAPE.1 <- mean(abs(error.1/mpdw3.d1.ts)*100)

forecast1 <- predict(SES.1, n.ahead = 1) #forecasting one ahead

SES.2 <- HoltWinters(mpdw3.d1.ts, alpha = 0.3, beta = FALSE, gamma = FALSE)
xhat.2 <- SES.2$fitted[,1]
error.2 <- residuals(SES.2)
forecast2 <- predict(SES.2, n.ahead = 1)
SSE.2 <- SES.2$SSE
MSE.2 <- mean(error.2^2)
MAD.2 <- mean(abs(error.2))
RMSE.2 <- sqrt(MSE.2)
MAPE.2 <- mean(abs(error.2/mpdw3.d1.ts)*100)


SES.3 <- HoltWinters(mpdw3.d1.ts, alpha = 0.5, beta = FALSE, gamma = FALSE)
xhat.3 <- SES.3$fitted[,1]
error.3 <- residuals(SES.3)
forecast3 <- predict(SES.3, n.ahead = 1)
SSE.3 <- SES.3$SSE
MSE.3 <- mean(error.3^2)
MAD.3 <- mean(abs(error.3))
RMSE.3 <- sqrt(MSE.3)
MAPE.3 <- mean(abs(error.3/mpdw3.d1.ts)*100)


SES.4 <- HoltWinters(mpdw3.d1.ts, alpha = 0.7, beta = FALSE, gamma = FALSE)
xhat.4 <- SES.4$fitted[,1]
error.4 <- residuals(SES.4)
forecast4 <- predict(SES.4, n.ahead = 1)
SSE.4 <- SES.4$SSE
MSE.4 <- mean(error.4^2)
MAD.4 <- mean(abs(error.4))
RMSE.4 <- sqrt(MSE.4)
MAPE.4 <- mean(abs(error.4/mpdw3.d1.ts)*100)

data.forecast <- data.frame(modelSES = c('Alpha = 0.1', 'Alpha = 0.3', 'Alpha = 0.5', 'Alpha = 0.7'),
              SES = c(SSE.1, SSE.2, SSE.3, SSE.4), MSE = c(MSE.1, MSE.2, MSE.3, MSE.4),
              MAD  = c(MAD.1, MAD.2, MAD.3, MAD.4), RMSE = c(RMSE.1, RMSE.2, RMSE.3, RMSE.4),
              MAPE = c(MAPE.1, MAPE.2, MAPE.3, MAPE.4), forecast = c(forecast1, forecast2, forecast3, forecast4))

#plot model comparison
plot(mpdw3.d1.ts, main = 'Actual Data and Predictions made using Provided Alphas', type = 'l', col = 'black')
lines(xhat.1, type = 'l', col = 'red')
lines(xhat.2, type = 'l', col = 'green')
lines(xhat.3, type = 'l', col = 'blue')
lines(xhat.4, type = 'l', col = 'brown')
lines(forecast3, type = 'p', col = 'blue')
legend('bottomleft', c('Actual Data', expression(paste(SES, ' ', alpha,'= 0.1')), 
                       expression(paste(SES, ' ', alpha,'= 0.3')), expression(paste(SES, ' ', alpha,'= 0.5')),
                       expression(paste(SES, ' ', alpha,'= 0.7'))), col = c('black', 'red', 'green', 'blue', 
                                                                            'brown'), lty = 1, cex = 0.6)
