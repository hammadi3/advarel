library(plotly)


stack_hists <- function(vector1, vector2, alpha = 0.5){
  fig <- plot_ly(alpha = alpha)
  fig <- fig %>% add_histogram(x = vector1)
  fig <- fig %>% add_histogram(x = vector2)
  fig <- fig %>% layout(barmode = "overlay")
  fig
}

stack_lines <- function(x, y1, y2){

  data <- data.frame(x, y1, y2)
  
  fig <- plot_ly(data, x = ~x, y = ~y1, name = 'trace 0', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = ~y2, name = 'trace 1', type = 'scatter', mode = 'lines') 
  fig
}