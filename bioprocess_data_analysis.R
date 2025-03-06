library(ggplot2)
library(tidyr)

library(readxl)
bioprocess_data <- read_excel("bioprocess_data.xlsx", sheet='Data')
View(bioprocess_data)