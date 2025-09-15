# Portfolio Allocation

#1(a) Reducing the data set for out of sample testing
data_pa <- data_final[1:1152, ] 

data_pa2 <- subset(data_pa, select = -yyyymm)

x2 <- model.matrix(CRSP_SPvw ~ ., data_pa2)[, -1]
y2 <- data_pa2$CRSP_SPvw

newdata <- data_final[1153,]
newdata <- subset(newdata, select = -yyyymm)
x.new <- model.matrix(CRSP_SPvw ~ ., newdata)[, -1]


#2 Model Prediction CI Frequentest Regression (Elastic Net)
elasticnet.model2 = glmnet(x2,y2,alpha=best.combination$Alpha)

predict(elasticnet.model2,s=best.combination$Lambda,type= "coefficients")

library(boot)
boot_predict_elasticnet <- function(data, indices) {
  d <- data[indices, ]
  x_boot <- model.matrix(CRSP_SPvw ~ ., d)[,-1]
  y_boot <- d$CRSP_SPvw
  model <- glmnet(x_boot, y_boot, alpha = best.combination$Alpha)
  pred <- predict(model, newx = x.new, s = best.combination$Lambda)
  return(pred)
}

set.seed(123)
boot_results <- boot(data = data_pa2, statistic = boot_predict_elasticnet, R = 1000)

predictions.elasticnet <- boot.ci(boot_results, type = "perc")
predictions.elasticnet 

forecast_return <- as.numeric(predict(elasticnet.model2, newx = x.new, s = best.combination$Lambda, type = "response"))
forecast_return 
forecast_se <- sd(boot_results$t)

n <- length(y2)  
fitted.elasticnet2 <- as.numeric(
  predict(elasticnet.model2,
          newx = x2,
          s    = best.combination$Lambda,
          type = "response")
)

rss.elasticnet2 <- sum((y2 - fitted.elasticnet2)^2)
coefficients.elasticnet2 <- as.numeric(
  predict(elasticnet.model2,
          s    = best.combination$Lambda,
          type = "coefficients")
)
df.elasticnet2 <- sum(coefficients.elasticnet2 != 0) - 1

log_likelihood.elasticnet2 <- -n/2 * (log(2 * pi * rss.elasticnet2 / n) + 1)
aic.elasticnet2 <- -2 * log_likelihood.elasticnet2 + 2 * df.elasticnet2
aic.elasticnet2


#3 Model Prediction CI Heavy Time Series (t-dist. GARCH(1,1))
returns_ts_pa <- ts(data_pa$CRSP_SPvw, start = c(1927, 1), frequency = 12)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)
garch11.3 <- ugarchfit(spec = spec, data = returns_ts_pa)

garch11.3_forecast <- ugarchforecast(garch11.3, n.ahead = 1)

forecast_mean <- fitted(garch11.3_forecast)
forecast_mean
forecast_sigma <- sigma(garch11.3_forecast)
forecast_sigma

model_coef <- coef(garch11.3)
show(model_coef)

t_multiplier <- qt(0.975, df = 8.02)

lower_bound <- forecast_mean - t_multiplier * forecast_sigma
upper_bound <- forecast_mean + t_multiplier * forecast_sigma

cat("95% Confidence Interval for the next step returns under the t-distribution:\n")
cat("[", lower_bound, ",", upper_bound, "]\n")

#4 Model Prediction CI Heavy regression + GARCH(3,3)
X3 <- as.matrix(data_pa[, c("d.p","d.y","e.p",
                            "svar","b.m","ntis",
                            "tbl","lty","ltr",
                            "dfy","dfr","infl")])

spec3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE, external.regressors = X3),
  distribution.model = "std"
)
heavy_OLS_garch14_11.2 <- ugarchfit(spec = spec3, data = data_pa$CRSP_SPvw)

new_row <- data_final[1153, ]
X3_next <- as.matrix(new_row[, colnames(X3), drop = FALSE])

garch14_11_forecast <- ugarchforecast(
  fit                = heavy_OLS_garch14_11.2,
  n.ahead            = 1,
  external.forecasts = list(mregfor = X3_next)
)

forecast_mean2 <- fitted(garch14_11_forecast)
forecast_mean2
forecast_sigma2 <- sigma(garch14_11_forecast)
forecast_sigma2

model_coef2 <- coef(heavy_OLS_garch14_11.2)
show(heavy_OLS_garch14_11.2)

t_multiplier <- qt(0.975, df = 3.817657)

lower_bound2 <- forecast_mean2 - t_multiplier * forecast_sigma2
upper_bound2 <- forecast_mean2 + t_multiplier * forecast_sigma2

cat("95% Confidence Interval for the next step returns under the t-distribution:\n")
cat("[", lower_bound2, ",", upper_bound2, "]\n")