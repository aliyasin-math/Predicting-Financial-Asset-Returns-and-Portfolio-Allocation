#Frequentist Regression


#1(a) Ordinary Linear Regression Model
ModelFull <- lm(CRSP_SPvw ~ d.p+d.y+e.p+d.e+svar+b.m+ntis+tbl+lty+ltr+tms+dfy+dfr+infl, data = data_final)   #Creating a Model with all variables
summary(ModelFull)

#1(b) OLS Model Evaluation Metrics
install.packages("caret")  
library(caret)

set.seed(1)
train.control <- trainControl(method = "cv", number = 10)  #10-fold cross-validation

model.cv <- train(CRSP_SPvw ~ d.p + d.y + e.p + d.e + svar + b.m + ntis + tbl + lty + ltr + tms + dfy + dfr + infl, 
                  data = data_final, 
                  method = "lm", 
                  trControl = train.control)

OLS.cv <- mean((model.cv$resample$RMSE)^2)  #Convert RMSE to MSE
print(OLS.cv)           #10-fold cross-validation MSE

AIC(ModelFull)          #AIC of OLS Model


#2(a) Ridge Regression
install.packages("glmnet")
library(glmnet)

data <- subset(data_final, select = -yyyymm)                    #Removes yyyymm column for ease of use 

x <- model.matrix(CRSP_SPvw ~ ., data)[, -1]
y <- data_final$CRSP_SPvw

ridge.model = glmnet(x,y,alpha=0)                                #Creates our ridge regression model, ridge.model$lambda[x], coef(ridge.model)[,x]

set.seed (1)
cv.ridge = cv.glmnet(x,y,alpha=0)                                 #Performs 10-fold cross validation
plot(cv.ridge)                                                   #Plots log(λ) against MSE
bestlam.ridge = cv.ridge$lambda.min                              #Calculates the lambda that gives minimum MSE
bestlam.ridge

predict(ridge.model,s=bestlam.ridge,type= "coefficients")        #Outputs the ridge regression coefficients associated with the optimal lambda

#2(b) Ridge Regression Model Evaluation Metrics
ridge.mse <- min(cv.ridge$cvm)                                                       #Stores the 10-fold CV MSE value

fitted.ridge <- predict(ridge.model, newx=x, s=bestlam.ridge)

rss.ridge = sum((data_final$CRSP_SPvw - fitted.ridge)^2)                              # Compute residual sum of squares (RSS) using the optimal lambda

coefficients.ridge = predict(ridge.model, s = bestlam.ridge, type = "coefficients")   # Degrees of freedom: number of non-zero coefficients
df.ridge = sum(coefficients.ridge != 0) - 1                                           # Exclude the intercept if present

n = length(data_final$CRSP_SPvw)                                                      # Number of observations

log_likelihood.ridge = -n / 2 * (log(2 * pi * rss.ridge / n) + 1)                     # Log-Likelihood

aic.ridge = -2 * log_likelihood.ridge + 2 * df.ridge                                  # AIC calculation


#3(a) LASSO
lasso.model = glmnet(x,y,alpha=1) 

set.seed (1)
cv.lasso = cv.glmnet(x,y,alpha=1)                                 #Performs 10-fold cross validation
plot(cv.lasso)                                                    #Plots log(λ) against MSE
bestlam.lasso = cv.lasso$lambda.min                               #Calculates the lambda that gives minimum MSE
bestlam.lasso

predict(lasso.model,s=bestlam.lasso,type= "coefficients")         #Outputs the lasso coefficients associated with the optimal lambda

#3(b) LASSO Model Evaluation Metrics
lasso.mse <- min(cv.lasso$cvm)                                    #Stores the 10-fold CV MSE value
lasso.mse 

fitted.lasso <- predict(lasso.model, newx=x, s=bestlam.lasso)

rss.lasso = sum((data_final$CRSP_SPvw - fitted.lasso)^2)                              # Compute residual sum of squares (RSS) using the optimal lambda


coefficients.lasso = predict(lasso.model, s = bestlam.lasso, type = "coefficients")   # Degrees of freedom: number of non-zero coefficients
df.lasso = sum(coefficients.lasso != 0) - 1                                           # Exclude the intercept if present

log_likelihood.lasso = -n / 2 * (log(2 * pi * rss.lasso / n) + 1)                     # Log-Likelihood

aic.lasso = -2 * log_likelihood.lasso + 2 * df.lasso                                  # AIC calculation
aic.lasso


#4(a) Elastic Net
alphas <- seq(0,1, by = 0.0001)

results <- data.frame(Alpha = numeric(), Lambda = numeric(), MSE = numeric())

for (a in alphas) {
  set.seed(1)
  cv.elasticnet = cv.glmnet(x,y,alpha=a)                                        #Performs 10-fold cross validation
  bestlam.elasticnet = cv.elasticnet$lambda.min                                 #Calculates the lambda that gives minimum MSE
  best_mse <- min(cv.elasticnet$cvm)
  results <- rbind(results, data.frame(Alpha = a, Lambda = bestlam.elasticnet, MSE = best_mse))
}

best.combination <- results[which.min(results$MSE), ]                           #Calculates the lambda and alpha combination which give minimum MSE
best.combination

elasticnet.model = glmnet(x,y,alpha=best.combination$Alpha) 

predict(elasticnet.model,s=best.combination$Lambda,type= "coefficients")        #Outputs the elastic net coefficients associated with the optimal lambda

#4(b) Elastic Net Model Evaluation Metrics
elasticnet.mse <- best.combination$MSE                                          #Stores the 10-fold CV MSE value
elasticnet.mse

rss.elasticnet = sum((data_final$CRSP_SPvw - fitted.elasticnet)^2)                                 # Compute residual sum of squares (RSS) using the optimal lambda


coefficients.elasticnet = predict(elasticnet.model, s = bestlam.elasticnet, type = "coefficients")   # Degrees of freedom: number of non-zero coefficients
df.elasticnet = sum(coefficients.elasticnet != 0) - 1                                             # Exclude the intercept if present

log_likelihood.elasticnet = -n / 2 * (log(2 * pi * rss.elasticnet / n) + 1)                       # Log-Likelihood

aic.elasticnet = -2 * log_likelihood.elasticnet + 2 * df.elasticnet                                         # AIC calculation
aic.elasticnet


#5(a) Model Checking 
par(mfrow=c(1,3))
plot(ModelFull, which = c(1,2,3), main="ModelFull")
par(mfrow=c(1,1))

#Box-Cox Transformation:
library(MASS)

data_boxcox <- data_final
data_boxcox$CRSP_SPvw <-data_boxcox$CRSP_SPvw + 1                      #Adds a constant value 1 to all returns values

ModelFull2 <- lm(CRSP_SPvw ~ d.p+d.y+e.p+d.e+svar+b.m+ntis+tbl+lty+ltr+tms+dfy+dfr+infl, data = data_boxcox)   #Creating a Model with all variables and our shifted response

boxcox_result <- boxcox(ModelFull2)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]                  #Finds the optimal lambda for the box cox transformation
lambda

ModelBox.Cox <- lm(log(CRSP_SPvw) ~ d.p+d.y+e.p+d.e+svar+b.m+ntis+tbl+lty+ltr+tms+dfy+dfr+infl, data = data_boxcox)

par(mfrow=c(1,3))
plot(ModelBox.Cox, which = c(1,2,3), main="ModelBox.Cox")


#Yeo-Johnson Transformation:
install.packages("car")
library(car)

yeo_johnson.result <- powerTransform(data_final$CRSP_SPvw, family = "yjPower")         #Finds the optimal lambda for the yeo johnson transformation
summary(yeo_johnson.result)

CRSP_SPvw.trans <- yjPower(data_final$CRSP_SPvw, lambda=yeo_johnson.result$lambda)     #Apply yeo johnson transformation using this lambda
print(CRSP_SPvw.trans)

ModelYeo.Johnson <- lm(CRSP_SPvw.trans ~ d.p+d.y+e.p+d.e+svar+b.m+ntis+tbl+lty+ltr+tms+dfy+dfr+infl, data = data_final)

par(mfrow = c(2, 2))
plot(ModelYeo.Johnson, main="ModelYeo.Johnson")

#5(b) t-distributed OLS Model

#MLE of degree of freedom 
install.packages("MASS")
library(MASS)

set.seed(123)
data <- data_final$CRSP_SPvw

fit <- fitdistr(data, "t",start = list(m=mean(data),s=sd(data),df=5), lower=c(-1,0.001,1))
print(fit)                                                                       

#t-distributed OLS Model based on MLE degrees of freedom
library(heavy)

heavy.model <- heavyLm(CRSP_SPvw ~ d.p+d.y+e.p+svar+b.m+ntis+tbl+lty+ltr+dfy+dfr+infl, data = data_final, family = Student(3.68), control = heavy.control(fix.shape = TRUE))
summary(heavy.model)

qqnorm(residuals(heavy.model), main = "Q-Q Residuals")
qqline(residuals(heavy.model), lty = 2)

AIC.heavy <- 2*14 - 2*as.numeric(5014.441)
print(AIC.heavy)

#t-distributed OLS Model based on EM degrees of freedom
heavy.model.2 <- heavyLm(CRSP_SPvw ~ d.p+d.y+e.p+svar+b.m+ntis+tbl+lty+ltr+dfy+dfr+infl, data = data_final)
summary(heavy.model.2)

qqnorm(residuals(heavy.model.2), main = "Q-Q Residuals")
qqline(residuals(heavy.model.2), lty = 2)

AIC.heavy.2 <- 2*14 - 2*as.numeric(5045.865)
print(AIC.heavy.2)





