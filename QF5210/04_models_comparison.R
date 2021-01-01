# MODELS COMPARISON 
# =================
#
# Compares the fit of our three forecasting
# models given the realised returns.
#

library(hydroGOF)
library(ggpubr)
library(car)

#Read the dataset
data = readRDS("estimates.rds")

#Compute the mean squared error for the predictions made by ARIMA model
cat("ARIMA\n", sep="")

avg_mse_arima = rep(0, length=452)

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  data[[7]][[i]]$arima_mse = 0
  counter <<- counter + 1
  cat("Processing [", counter, "/", total, "]...\n", sep="")
}

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  if (nrow(data[[7]][[i]]) > 120.5) {
    mse_arima = rep(NA, length=nrow(data[[7]][[i]]))
    
    true_y = data[[7]][[i]]$logreturn
    pred_y = data[[7]][[i]]$arima_pred
    
    for (j in 121:nrow(data[[7]][[i]])){
      data[[7]][[i]]$arima_mse[j] = mse(true_y[j], pred_y[j], na.rm=FALSE)
    }
    
    counter <<- counter + 1
    avg_mse_arima[[counter]] = mse(true_y[121:nrow(data[[7]][[i]])], pred_y[121:nrow(data[[7]][[i]])], na.rm=TRUE)
    cat("Processing [", counter, "/", total, "]...\n", sep="")
  }
}

#Compute the mean squared error for the predictions made by GARCH model
cat("GARCH\n", sep="")

avg_mse_garch = rep(0, length=452)

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  data[[7]][[i]]$garch_mse = 0
  counter <<- counter + 1
  cat("Processing [", counter, "/", total, "]...\n", sep="")
}

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  if (nrow(data[[7]][[i]]) > 120.5) {
    mse_garch = rep(NA, length=nrow(data[[7]][[i]]))
    
    true_y = data[[7]][[i]]$logreturn
    pred_y = data[[7]][[i]]$garch_pred
    
    for (j in 121:nrow(data[[7]][[i]])){
      data[[7]][[i]]$garch_mse[j] = mse(true_y[j], pred_y[j], na.rm=FALSE)
    }
    
    counter <<- counter + 1
    avg_mse_garch[[counter]] = mse(true_y[121:nrow(data[[7]][[i]])], pred_y[121:nrow(data[[7]][[i]])], na.rm=TRUE)
    cat("Processing [", counter, "/", total, "]...\n", sep="")
  }
}

#Compute the mean squared error for the predictions made by MSGARCH model
cat("MSGARCH\n", sep="")

avg_mse_msgarch = rep(0, length=452)

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  data[[7]][[i]]$msgarch_mse = 0
  counter <<- counter + 1
  cat("Processing [", counter, "/", total, "]...\n", sep="")
}

counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  if (nrow(data[[7]][[i]]) > 120.5) {
    mse_msgarch = rep(NA, length=nrow(data[[7]][[i]]))
    
    true_y = data[[7]][[i]]$logreturn
    pred_y = data[[7]][[i]]$msgarch_pred
    
    for (j in 121:nrow(data[[7]][[i]])){
      data[[7]][[i]]$msgarch_mse[j] = mse(true_y[j], pred_y[j], na.rm=FALSE)
    }
    
    counter <<- counter + 1
    avg_mse_msgarch[[counter]] = mse(true_y[121:nrow(data[[7]][[i]])], pred_y[121:nrow(data[[7]][[i]])], na.rm=TRUE)
    cat("Processing [", counter, "/", total, "]...\n", sep="")
  }
}

#Collect all mean squared error data across all stocks for each model
all_mse_arima = c()
all_mse_garch = c()
all_mse_msgarch = c()
for (i in 1:nrow(data)){
  if (nrow(data[[7]][[i]]) > 120.5){
    all_mse_arima <- append(all_mse_arima, data[[7]][[i]]$arima_mse[121:nrow(data[[7]][[i]])])
    all_mse_garch <- append(all_mse_garch, data[[7]][[i]]$garch_mse[121:nrow(data[[7]][[i]])])
    all_mse_msgarch <- append(all_mse_msgarch, data[[7]][[i]]$msgarch_mse[121:nrow(data[[7]][[i]])])
  }
}

#Create a table summarizing the mean squared error data
Models <- c('ARIMA', 'GARCH', 'MSGARCH')
Counts <- c(length(avg_mse_arima), length(avg_mse_garch), length(avg_mse_msgarch))
Means <- c(mean(avg_mse_arima, na.rm = TRUE), mean(avg_mse_garch, na.rm = TRUE), mean(avg_mse_msgarch, na.rm = TRUE))
SDs <- c(sd(avg_mse_arima, na.rm = TRUE), sd(avg_mse_garch, na.rm = TRUE), sd(avg_mse_msgarch, na.rm = TRUE))

dataframe <- data.frame(Models, Counts, Means, SDs, stringsAsFactors=FALSE)
dataframe

#Create a table consisting of all the mean squared error data
mse_data = avg_mse_arima
Model = rep("ARIMA", length(avg_mse_arima))
dataframe_arima = data.frame(mse_data, Model, stringsAsFactors=FALSE)

mse_data = avg_mse_garch
Model = rep("GARCH", length(avg_mse_garch))
dataframe_garch = data.frame(mse_data, Model, stringsAsFactors=FALSE)

mse_data = avg_mse_msgarch
Model = rep("MSGARCH", length(avg_mse_msgarch))
dataframe_msgarch = data.frame(mse_data, Model, stringsAsFactors=FALSE)

data_frame = rbind(dataframe_arima, dataframe_garch, dataframe_msgarch)

#Visualize the summary of the mean squared error data using box plot
ggboxplot(data_frame, x = "Model", y = "mse_data", 
          color = "Model", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ARIMA", "GARCH", "MSGARCH"),
          ylab = "Mean Squared Error", xlab = "Model")

#Perform one-way ANOVA test
res.aov <- aov(mse_data ~ label, data = data_frame)
summary(res.aov)

#Check ANOVA assumptions
#Check the homogeneity of variance assumption (fulfilled)
plot(res.aov, 1)

leveneTest(mse_data ~ label, data = data_frame)

#Check the normality assumption (violated)
plot(res.aov, 2)

aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

#Perform Kruskal-Wallis H test
kruskal.test(mse_data ~ label, data = data_frame)

#Visualize the mean squared error data for the first 100 stocks
stocks_ID = 1:100
plot(stocks_ID, avg_mse_arima[stocks_ID], type="b", col=alpha("green", 0.5), lwd=1, pch=16, xlab="Stocks", ylab="Mean Squared Error", ylim=range(avg_mse_arima,avg_mse_garch,avg_mse_msgarch))
lines(stocks_ID, avg_mse_garch[stocks_ID], type="b", col=alpha("red", 0.5), lwd=1, pch=16)
lines(stocks_ID, avg_mse_msgarch[stocks_ID], type="b", col=alpha("blue", 0.5), lwd=1, pch=16)
legend(x="topright",legend=c("ARIMA","GARCH","MSGARCH"), lwd=c(2,2,2), col=alpha(c("green","red","blue"), alpha=0.5), pch=c(16,16,16))

#Count the average data points per stock and the average absolute log-return
data_size_per_stock = c()
all_log_return = c()
counter = 0
total = nrow(data)
for (i in 1:nrow(data)){
  if (nrow(data[[7]][[i]]) > 120.5) {
    data_size_per_stock = append(data_size_per_stock, nrow(data[[7]][[i]]))
    all_log_return = append(all_log_return, data[[7]][[i]]$logreturn[121:nrow(data[[7]][[i]])])
    counter <<- counter + 1
    cat("Processing [", counter, "/", total, "]...\n", sep="")
  }
}
