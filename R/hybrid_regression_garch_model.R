# Hybrid Predictive Regression and t-distributed GARCH Model

#1(a) Heavy OLS+GARCH
X2 <- as.matrix(data_final[, c("d.p","d.y","e.p",
                               "svar","b.m","ntis",
                               "tbl","lty","ltr",
                               "dfy","dfr","infl")])

heavy_OLS_garch_models <- list()
heavy_model_info2 <- data.frame(p = integer(), q = integer(), AIC = numeric(), stringsAsFactors = TRUE)

heavy_counter <- 1
for(p in 1:3) {
  for(q in 1:3) {
    spec2 <- ugarchspec(
      variance.model = list(model      = "sGARCH", garchOrder = c(p,q)),
      mean.model     = list(armaOrder = c(0,0), include.mean = TRUE, external.regressors = X2),
      distribution.model = "std"
    )
    fit <- ugarchfit(spec = spec2, data = data_final$CRSP_SPvw)
    
    heavy_aic_value <- infocriteria(fit)[1]
    
    heavy_model_info2[heavy_counter,] <- c(p, q, heavy_aic_value)
    heavy_OLS_garch_models[[heavy_counter]] <- fit
    
    cat("Heavy OLS+GARCH order: (", p, ",", q, ") - AIC:", heavy_aic_value, "\n")
    heavy_counter <- heavy_counter + 1
  }
}

heavy_fit14.11 <- heavy_OLS_garch_models[[9]]

show(heavy_fit14.11)

#1(b) Heavy OLS+GARCH Model Checking
u_t_heavy_fit14.11 <- residuals(heavy_fit14.11, standardize=TRUE)
qqnorm(u_t_heavy_fit14.11); qqline(u_t_heavy_fit14.11, col="red")