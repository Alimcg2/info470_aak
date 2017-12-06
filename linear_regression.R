library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)

source("script.R")

# getting data and assigning scores
df <- all.data.pop
df <- df %>% arrange(-totalScore)
df$scores <- seq.int(nrow(df))

# creating linear regression model
fit <- lm(scores ~ emissions + withdrawals + waste.pop + combustion + lfg.collected + renews, data=df)
summaryof.fit <- summary(fit)
tableSummary <- as.data.frame(summaryof.fit["coefficients"])

# checking to make sure there's a linear relationship between 
# independent and dependent variables (prerequisite for linear regression)
plotFunc <- function(xVar, colorInside, colorOutside, titleText, xaxisTitle) {
  plot_ly(data = df, x = xVar, y = ~scores, mode = "markers",
             text = ~state.name,
             marker = list(size = 10,
                           color = colorInside,
                           line = list(color = colorOutside,
                                       width = 2))) %>%
  layout(title = titleText,
         yaxis = list(zeroline = FALSE, title = "Rank"),
         xaxis = list(zeroline = FALSE, title = xaxisTitle))
}
a <- plotFunc(df$emissions,'rgba(10, 36, 99, .6)', 'rgba(10, 36, 99, .9)', 'Scores Given Emissions', "Emissions Score")
b <- plotFunc(df$withdrawals,'rgba(63, 136, 197, .6)', 'rgba(63, 136, 197, .9)', "Scores Given Withdrawals", "Withdrawals Score")
c <- plotFunc(df$waste.pop, 'rgba(232, 151, 44, .6)', 'rgba(232, 151, 44, .9)',"Scores Given Waste by Population", "Waste Score")
d <- plotFunc(df$lfg.collected, 'rgba(239, 219, 127, .6)', 'rgba(239, 219, 127, .9)', "Scores Given LFG Collected", "LFG Score")
e <- plotFunc(df$renews, 'rgba(215, 38, 56, .6)', 'rgba(215, 38, 56, .9)', "Scores Given Renewables", "Renewables Score")
f <- plotFunc(df$combustion, 'rgba(148, 16, 32, .6)', 'rgba(148, 16, 32, .9)', "Scores Given Combustion", "Combustion Score")

# plotting the residuals versus fitted values
# don't want to see a pattern
p.emissions <- plot_ly(x = residuals(fit), y = df$emissions, mode = "markers", text = ~state.name, 
                       marker = list(size = "10", color = 'rgba(10, 36, 99, .7)')) %>% 
  layout(title = "Emission Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "Emissions Score"))
p.withdrawals <- plot_ly(x = residuals(fit), y = df$withdrawals, mode = "markers", text = ~state.name, 
                         marker = list(size = "10", color = 'rgba(63, 136, 197, .7)')) %>% 
  layout(title = "Withdrawal Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "Withdrawals Score"))
p.waste.pop <- plot_ly(x = residuals(fit), y = df$waste.pop, mode = "markers", text = ~state.name, 
                       marker = list(size = "10", color = 'rgba(232, 151, 44, .7)')) %>% 
  layout(title = "Waste Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "Waste Score"))
p.lfg.collected <- plot_ly(x = residuals(fit), y = df$lfg.collected, mode = "markers", text = ~state.name, 
                           marker = list(size = "10", color = 'rgba(239, 219, 127, .7)')) %>% 
  layout(title = "LFG Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "LFG Score"))
p.renews <- plot_ly(x = residuals(fit), y = df$renews, mode = "markers", text = ~state.name, 
                    marker = list(size = "10", color = 'rgba(215, 38, 56, .7)')) %>% 
  layout(title = "Renewables Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "Renewables Score"))
p.combustion <- plot_ly(x = residuals(fit), y = df$combustion, mode = "markers", text = ~state.name, 
                        marker = list(size = "10", color = 'rgba(148, 16, 32, .7)')) %>% 
  layout(title = "Combustion Score Residuals vs Fitted Values",
         xaxis = list(zeroline = FALSE, title = "Risiduals"),
         yaxis = list(zeroline = FALSE, title = "Combustion Score"))


# checking distribution of the dependent variable
h <- plot_ly(x = c(1:50), y = 1, type = "bar", text = df$state.name,  marker = list(color = 'rgb(198, 198, 198)'))

