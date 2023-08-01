library(dplyr)
library(tidyverse)
library(bayesboot)
library(fixest)


# Variable identifying incidents in the same neighborhood and year
data$id = paste(data$Neighborhood,data$Year)

# Compute per 10 %-point increase in probability
data$probability_of_busy_per10pst = data$probability_of_busy*10



bayesbootfun1 <- function(data,w) {
  fit1 <- summary(feols(Responstid~Weekday+Month+Hour+probability_of_busy_per10pst|id,data=data,weights=w))

  delay_per_10pst    = fit1$coefficients["probability_of_busy_per10pst"]
  delay_per_incident = mean(predict(fit1,newdata = data) - predict(fit1,newdata=data %>% mutate(probability_of_busy_per10pst=0)))
  return(c(delay,tt))
}

results <- bayesboot(data=data %>% filter(sub==T),bayesbootfun1,R=10000,use.weights = T)
CI      <- quantile(results$probability_of_busy_per10pst,c(0.025,.5,.975))

