
choroplthFunc <- function(data, zData, stateAbb, colorData, titleText, colorPick, hoverText){
  
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(scope = 'usa', projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))
  plot_geo(data, locationmode = 'USA-states') %>%
    add_trace( z = zData, locations = stateAbb, color = colorData, colors = colorPick, 
               marker = list(line = l), text = hoverText)  %>%
    layout(title = titleText, geo = g, autosize = F, width = 900, height = 750, margin = m, font = list(family = "times"))
}
bubbleGraphFunc <- function(data, lon, lat, sizeData, colorData, hoverText, titleText) {
  g <- list(
    scope = 'usa', projection = list(type = 'albers usa'), showland = TRUE, landcolor = toRGB("gray85"), 
    subunitcolor = toRGB("white"), countrycolor = toRGB("white")
  )
  plot_geo(landfill, locationmode = 'USA-states', sizes = c(1, 50)) %>%
    add_markers(
      x = lon, y = lat, size = sizeData, color = colorData, colors="PRGn", hoverinfo = "text",
      text = hoverText
    ) %>%
    layout(title = titleText, geo = g,  font = list(family = "times"))
}
stackChartFunc <- function(data, xData, YData, YDataName, YData2, YDataName2){
  plot_ly(data, x = xData, y = YData, type = 'bar', name = YDataName) %>%
    add_trace(y = YData2, name = YDataName2) %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack',  font = list(family = "times"))
}
