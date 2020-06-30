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


kaplan_meier <- function(time, event){
  df <- data.frame(time = time, status = event) %>%
    group_by(time) %>% 
    mutate(failure = sum(status == 1), survivor = sum(status == 0)) %>% 
    distinct(time, .keep_all = TRUE) %>%
    arrange(time) %>% 
    ungroup() %>%
    mutate(n_i = failure + survivor,
           n_out = lag(cumsum(n_i), n = 1L, default = 0),
           n_in = sum(n_i) - n_out)
  
  if (event[which.max(time)] == 0) {
    df <- mutate(df, prob = 1 - cumprod((n_in - failure)/n_in))
  }
  else {
    df <- mutate(df, prob = 1 - (((n_in + 0.7)/(n_in + 0.4)) * cumprod(((n_in + 0.7) - failure)/(n_in + 1.7))))
  }
  df
}


nelson <- function(time, event){
  df <- data.frame(time = time, status = event) %>% 
    group_by(time) %>%
    mutate(failure = sum(status == 1),
           survivor = sum(status == 0)) %>%
    distinct(time, .keep_all = TRUE) %>% 
    arrange(time) %>% 
    ungroup() %>% 
    mutate(n_out = failure + survivor,
           n_in = length(time) - lag(cumsum(n_out), n = 1L, default = 0),
           lam_nel = ifelse(status == 1, failure/n_in, 0),
           H_nel = cumsum(lam_nel),
           prob = 1 - exp(-H_nel))
}



