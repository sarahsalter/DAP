library(devtools)
devtools::install_github("PMassicotte/gtrendsR") #dev version
library(gtrendsR)
devtools::install_github("facebookincubator/prophet/R")
library(dplyr)
library(prophet)

healthcare <- gtrends(c("health care"), geo = c("US"))
plot(healthcare)

#str(healthcare)
#healthcare[[1]][,c(1,2)] %>% head()
dat <- healthcare[[1]][,c(1,2)]
colnames(dat) <- c("ds", "y")


prophet.model <- prophet(dat)

plot(prophet.model)

prophet_plot_components(prophet.model, predict(prophet.model, dat))


predict.prophet(prophet.model, dat) %>% plot()

library(ggplot2)

