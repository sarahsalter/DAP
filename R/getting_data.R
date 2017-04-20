#getting data
getwd()
library(readr)
source("R/functions.R")
# source("bashscripts/datasave.sh")
# system("sh bashscripts/datasave.sh")
# system("date > '../data/timestamp.txt'")
hr.115 <- get_data(congress = 115, billrange = 19:2003, debug = T)
write_csv(hr.115, "data/hr115.csv")

hr.114 <- get_data(congress = 114, billrange = c(2,3,20:6536), debug = T)
write_csv(hr.114, "data/hr114.csv")

hr.113 <- get_data(congress = 113, billrange = 100:5893, debug = T)
write_csv(hr.113, "data/hr113.csv")

l.115 <- get_raw_data(congress = 115, billrange = 19:2003, debug = T)
l.114 <- get_raw_data(congress = 114, billrange = c(2,3,20:6536), debug = T)
l.113 <- get_raw_data(congress = 113, billrange = 1:5893, debug = T)


save(l.115,l.112,l.113,file="data/list_data.RData")
