# analysis
library(readr)
library(dplyr)

# reading in data
hr.115 <- read_csv("data/hr115.csv")
# removing rows with only NA values
hr.115 <- hr.115[apply(hr.115,1,function(x) sum(is.na(x))) != 21, ]

hr.114 <- read_csv("data/hr114.csv")
hr.114 <- hr.114[apply(hr.114,1,function(x) sum(is.na(x))) != 21, ]

hr.113 <- read_csv("data/hr113.csv")
hr.113 <- hr.113[apply(hr.113,1,function(x) sum(is.na(x))) != 21, ]


# LETS look at 114 and 113 congress
set.seed(150)
alldata <- rbind(hr.114, hr.113)
training <- sample(1:nrow(alldata), round(nrow(alldata) * 0.8))
alldata.train <- alldata[training,]
alldata.test <- alldata[-training,]

reg1 <- glm(h_enacted ~ number + introhousedate, 
    family=poisson(),
    data = alldata.train,
    weights = ifelse(h_enacted == T, 1/mean(alldata.train$h_enacted, na.rm = T),1))
1/mean(alldata.train$h_enacted, na.rm = T)
reg1.predicted <- ifelse(predict.glm(reg1, alldata.test[complete.cases(alldata.test[,c(20,2)]),], 
                                     type = "response") > 0.4,
                         1,0)
test.y <- alldata.test[complete.cases(alldata.test[,c(20,2)]),]$h_enacted

mean((reg1.predicted == test.y)[test.y == 0])

# just a poission regression 
# sensitivity = 0.667
# specificity = 0.673

library(randomForest)
rforest <- randomForest(as.factor(h_enacted) ~ number + introhousedate, 
             data = alldata.train[complete.cases(alldata.train[,c(20,2,15)]),],
             classwt = c(1,30))
rforest1.pred <- predict(rforest, alldata.test[complete.cases(alldata.test[,c(20,2,15)]),])
mean(rforest1.pred == test.y)
mean((rforest1.pred == test.y)[test.y == T])
mean((rforest1.pred == test.y)[test.y == F])
mean((rforest1.pred == test.y)[rforest1.pred == T])
mean((rforest1.pred == test.y)[rforest1.pred == F])
