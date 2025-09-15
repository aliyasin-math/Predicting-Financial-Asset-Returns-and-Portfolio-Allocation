# Conditional Volatility Modelling (Time Series)

#1 Creating the Time Series Data Set
library(tseries)

returns_ts <- ts(data_final$CRSP_SPvw, start = c(1927, 1), frequency = 12)


#2(a) ARCH Model
library(rugarch)

arch_models <- list()

for (i in 1:12) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(i, 0)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit <- ugarchfit(spec = spec, data = returns_ts)
  arch_models[[i]] <- fit
  
  aic_value <- infocriteria(fit)[1]
  cat("ARCH order:", i, "AIC:", aic_value, "\n")
}

coef(arch_models[[9]])

#2(b) ARCH Model Checking 
par(mfrow=c(1,2))

fit9 <- arch_models[[9]]
u_t_fit9 <- residuals(fit9, standardize=TRUE)
qqnorm(u_t_fit9); qqline(u_t_fit9, col="red")


#3(a) GARCH Model
garch_models <- list()
model_info <- data.frame(p = integer(), q = integer(), AIC = numeric(), stringsAsFactors = FALSE)

counter <- 1
for(p in 1:3) {
  for(q in 1:3) {
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "norm"
    )
    fit <- ugarchfit(spec = spec, data = returns_ts)
    
    aic_value <- infocriteria(fit)[1]
    
    model_info[counter,] <- c(p, q, aic_value)
    garch_models[[counter]] <- fit
    
    cat("GARCH order: (", p, ",", q, ") - AIC:", aic_value, "\n")
    counter <- counter + 1
  }
}

garch33_coefs <- coef(garch_models[[1]])
print(garch33_coefs)

#3(b) GARCH Model Checking
fit1_1 <- garch_models[[1]]
u_t_fit1_1 <- residuals(fit1_1, standardize=TRUE)
qqnorm(u_t_fit1_1); qqline(u_t_fit1_1, col="red")


#4(a) Heavy ARCH Model
library(rugarch)

heavy_arch_models <- list()

for (i in 1:12) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(i, 0)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std",
  )
  
  fit <- ugarchfit(spec = spec, data = returns_ts)
  heavy_arch_models[[i]] <- fit
  
  heavy_aic_value <- infocriteria(fit)[1]
  cat("Heavy ARCH order:", i, "AIC:", heavy_aic_value, "\n")
}

coef(heavy_arch_models[[9]])

#4(b) Heavy ARCH Model Model Checking
heavy_fit9 <- heavy_arch_models[[9]]
u_t_heavy_fit9 <- residuals(heavy_fit9, standardize=TRUE)
qqnorm(u_t_heavy_fit9); qqline(u_t_heavy_fit9, col="red")


#5(a) Heavy GARCH Model
heavy_garch_models <- list()
heavy_model_info <- data.frame(p = integer(), q = integer(), AIC = numeric(), stringsAsFactors = TRUE)

heavy_counter <- 1
for(p in 1:3) {
  for(q in 1:3) {
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "std",
    )
    fit <- ugarchfit(spec = spec, data = returns_ts)
    
    heavy_aic_value <- infocriteria(fit)[1]
    
    heavy_model_info[heavy_counter,] <- c(p, q, heavy_aic_value)
    heavy_garch_models[[heavy_counter]] <- fit
    
    cat("Heavy GARCH order: (", p, ",", q, ") - AIC:", heavy_aic_value, "\n")
    heavy_counter <- heavy_counter + 1
  }
}

heavy_garch11_coefs <- coef(heavy_garch_models[[1]])
print(heavy_garch11_coefs)

#5(b) Heavy ARCH Model Model Checking
heavy_fit1_1 <- heavy_garch_models[[1]]
u_t_heavy_fit1_1 <- residuals(heavy_fit1_1, standardize=TRUE)
qqnorm(u_t_heavy_fit1_1); qqline(u_t_heavy_fit1_1, col="red")
