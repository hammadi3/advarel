library(dplyr)

johnson <- function(time, event){
  df <- data.frame(time = time, event = event)
  df <- df %>% group_by(time) %>% 
    mutate(failure = sum(event == 1), survivor = sum(event == 0)) %>%
    distinct(time, .keep_all = TRUE) %>% 
    arrange(time) %>% 
    ungroup() %>% 
    mutate(n_i = failure + survivor,
           n_out = lag(cumsum(n_i), n = 1L, default = 0),
           rank = failure) %>%
    mutate(rank = calculate_ranks(f = failure, n_out = n_out, n = sum(n_i))) %>% 
    mutate(prob = (rank - 0.3)/(sum(n_i) + 0.4))
  df
}


