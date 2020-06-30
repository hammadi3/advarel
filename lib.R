# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, weibulltools, Metrics, zoo, DT, dplyr, plotly, keras)

# Set option stringsAsFactors = F globaly
options(stringsAsFactors = F)