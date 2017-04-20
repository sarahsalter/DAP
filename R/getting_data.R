#getting data
getwd()
source("R/functions.R")
hr.115 <- get_data(congress = 115, billrange = 19:2003, debug = T)
hr.114 <- get_data(congress = 114, billrange = c(2,3,20:6536), debug = T)
hr.113 <- get_data(congress = 113, billrange = 1:5893, debug = T)
