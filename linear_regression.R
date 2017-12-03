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
plotFunc <- function(xVar, colorInside, colorOutside, titleText) {
  plot_ly(data = df, x = xVar, y = ~scores, mode = "markers",
             text = ~state.name,
             marker = list(size = 10,
                           color = colorInside,
                           line = list(color = colorOutside,
                                       width = 2))) %>%
  layout(title = titleText,
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))
}
a <- plotFunc(df$emissions,'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)', 'Scores Given Emissions')
b <- plotFunc(df$withdrawals,'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)', "Scores Given Withdrawals")
c <- plotFunc(df$waste.pop, 'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)',"Scores Given Waste by Population")
d <- plotFunc(df$combustion, 'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)', "Scores Given Combustion")
e <- plotFunc(df$lfg.collected, 'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)', "Scores Given LFG Collected")
f <- plotFunc(df$renews, 'rgba(255, 182, 193, .9)', 'rgba(152, 0, 0, .8)', "Scores Given Renewables")

# plotting the residuals versus fitted values
# don't want to see a pattern
p.emissions <- plot_ly(x = df$emissions, y = residuals(fit), mode = "markers", text = ~state.name)
p.withdrawals <- plot_ly(x = df$withdrawals, y = residuals(fit), mode = "markers", text = ~state.name)
p.waste.pop <- plot_ly(x = df$waste.pop, y = residuals(fit), mode = "markers", text = ~state.name)
p.combustion <- plot_ly(x = df$combustion, y = residuals(fit), mode = "markers", text = ~state.name)
p.lfg.collected <- plot_ly(x = df$lfg.collected, y = residuals(fit), mode = "markers", text = ~state.name)
p.renews <- plot_ly(x = df$renews, y = residuals(fit), mode = "markers", text = ~state.name)

# checking distribution of the dependent variable
h <- plot_ly(data = df, x = ~scores, type = "histogram")
