library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)

source("script.R")

# getting data and assigning scores
df <- all.data.density
df <- df %>% arrange(-totalScore)
df$scores <- seq.int(nrow(df))

# creating linear regression model
fit <- lm(scores ~ emissions + withdrawals + waste.pop + combustion + lfg.collected + renews, data=df)
summary(fit)

# checking to make sure there's a linear relationship between 
# independent and dependent variables (prerequisite for linear regression)
a <- plot(df$emissions, df$scores, xlab = "Emissions", ylab = "Scores", main = "Scores Given Emmissions")
b <- plot(df$withdrawals, df$scores, xlab = "Withdrawals", ylab = "Scores", main = "Scores Given Withdrawals")
c <- plot(df$waste.pop, df$scores, xlab = "Waste by Population", ylab = "Scores", main = "Scores Given Waste by Population")
d <- plot(df$combustion, df$scores, xlab = "Combustion", ylab = "Scores", main = "Scores Given Combustion")
e <- plot(df$lfg.collected, df$scores, xlab = "LFG Collected", ylab = "Scores", main = "Scores Given LFG Collected")
f <- plot(df$renews, df$scores, xlab = "Renews", ylab = "Scores", main = "Scores Given Renewables")

# plotting the residuals versus fitted values
# don't want to see a pattern
p <- plot(fitted(fit), residuals(fit), xlab = "Fitted Values", ylab = "Residuals")
title("Residual vs Fit. value - OK model")
p <- p + abline(0,0, col = "red")

# checking distribution of the dependent variable
h <- hist(df$scores, xlab = "Scores", ylab = "Frequency", breaks = 20, main = "Distribution of Scores")
