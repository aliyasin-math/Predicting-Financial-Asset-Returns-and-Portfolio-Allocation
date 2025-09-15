#Construction of the Usable Data Set + Empirical Density


#1. Cleaning The Data
install.packages("readxl")
library(readxl)

returns <- read_excel("PredictorData2023.xlsx", col_names = TRUE)

returns[returns == "NaN"] <- NA                                     #Sets NaN data entries to NA(to be interpreted as missing value)

returns_1 <- returns[ , !(names(returns) %in% c("csp"))]            #Removes the csp column

x <- max(which(!complete.cases(returns_1)))                         #Sets x to be the value of the last row that has NaN as any column entry
returns_2 <- returns_1[(x + 1):nrow(returns_1), ]                   #Removes rows 1 to x from returns_1

str(returns_2)                                                      #This shows me that every column that previously contained NaN entries were still interpreting numbers as characters

returns_cleaned <- data.frame(lapply(returns_2, function(x) as.numeric(as.character(x))))    #This fixes the issue


#2. Creating a usable data set:
install.packages("dplyr")
library(dplyr)

data_new <- returns_cleaned %>%
  mutate(
    d.p = log(D12) - log(Index),
    d.y = log(D12) - log(lag(Index)),
    e.p = log(E12) - log(Index),
    d.e = log(D12) - log(E12),
    tms = lty - tbl,
    dfy = BAA - AAA,
    dfr = corpr - ltr)                                              #Computes the variables values not in the dataset


data_final <- data_new %>%
  select(yyyymm, d.p, d.y, e.p, d.e, svar, b.m, ntis, tbl, lty, ltr, tms, dfy, dfr, infl, CRSP_SPvw)    #Creates a dataset containing only the required variables

data_final <- data_final[-1, ]                                      #Removes the 192612 row to start the data from 192701


#3 Empirical Density Plot:
install.packages("ggplot2")
library(ggplot2)

set.seed(123)

empirical_density <- density(data_final$CRSP_SPvw)   #Empirical density function

mean_return <- mean(data_final$CRSP_SPvw)            #Sample Mean
sd_return <- sd(data_final$CRSP_SPvw)                #Sample sd

xs <- seq(min(data_final$CRSP_SPvw), max(data_final$CRSP_SPvw), length.out = 1000)             #Normal Density curve
normal_density <- dnorm(xs, mean = mean_return, sd = sd_return)    #This constructs our Normal Density Curve

plot(empirical_density,
     main = "Empirical Density",
     xlab = "Returns",
     ylab = "Density",
     lwd = 2)                               #Plot Empirical Density
lines(xs, normal_density,lwd=2, lty=2)      #Plot Normal Density

legend("topright",
       legend = c("Empirical Density", "Normal Density"),
       lwd = 2, lty = c(1,2))