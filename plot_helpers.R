library(plotly)


stack_hists <- function(vector1, vector2, alpha = 0.5){
  fig <- plot_ly(alpha = alpha)
  fig <- fig %>% add_histogram(x = vector1)
  fig <- fig %>% add_histogram(x = vector2)
  fig <- fig %>% layout(barmode = "overlay")
  fig
}

stack_lines_probs <- function(x, y1, y2, y3){

  data <- data.frame(x, y1, y2)
  
  fig <- plot_ly(data, x = ~x, y = ~y1, name = 'Johnson', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = ~y2, name = 'Kaplan Meier', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = ~y3, name = 'Nelson', type = 'scatter', mode = 'lines') 
  
  fig <- fig %>% layout(
    title = 'nicht parametrische Risikoabschätzung',
    xaxis = list(
      title = 'Fahrstrecke s [km]'
    ),
    yaxis = list(
      title = 'Ausfallwahrscheinlichkeit F(s) [1]'
    )
  )
  fig
}