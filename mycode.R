### Loading Libraries

library(Flury)
library(ggplot2)
data("microtus")
str(microtus)
levels(microtus$Group)

#### Discriptive analysis & Exploratory of the data ####
summary(microtus)
dim(microtus)
table(microtus$Group)
attach(microtus)
sapply(microtus[2:9],mean)
#### Summary of all the variables
summary (microtus$M1Left)
summary (microtus$Rostrum)
summary (microtus$M2Left)
summary (microtus$M3Left)
summary (microtus$Foramen)
summary (microtus$Pbone)
summary (microtus$Length)
summary (microtus$Height)
##### Vizualizing all the  variables for Normality
hist(microtus$Height , col = "purple", breaks =50)
hist(microtus$M1Left , col = "red", breaks = 50)
hist(microtus$M2Left,col = "orange",breaks = 50)
hist(microtus$M3Left,col = "blue",breaks = 50)
hist(microtus$Foramen,col = "grey",breaks = 50)
hist(microtus$Pbone,col = "pink",breaks = 50)
hist(microtus$Length,col = "brown",breaks = 50)
hist(microtus$Height,col = "green", breaks = 50)

hist(microtus$Height,col = "brown")
abline(v= 12,lwd = 2)
abline(v = median(microtus$Height),col= "magenta",lwd=4)

barplot(table(microtus$Group), col = "wheat",main = "Number of species")

########## Exploratory for all the variables ##############
library(gridExtra)

plt_list <- list()

for(i in  1:9){
        
        var <- names(microtus)
        
        plt_list[[i]] <- ggplot(microtus,aes_string(x = var[i])) + 
                geom_histogram(colour="black", fill="purple") +
                ggtitle(var[i])
        
}

do.call(grid.arrange,plt_list)

qplot(M1Left,M2Left,colour=Group,data=microtus)

##### using entire known data set to classify unknown data #####
library(Flury)
library(mclust)
library(ggplot2)
data("microtus")
head(microtus)
train <- microtus[microtus$Group != "unknown",]
train$Group <- factor(train$Group) 
test <- microtus[microtus$Group == "unknown",]
# need to use package MASS
library(MASS)
## first argument are variables to use and the second for classification
mod1 = lda(train[,2:9],train[,1])
mod1

plot(mod1, dimen=1, type="both")
# outputs are the coefficients of  linear discriminant  function
# means of variables in the groups
# and prior probabilities
# let us look at the errors
# using 'predict', the model is applied to the observations
burger.ld <- predict(mod1)
apply(burger.ld$posterior, MARGIN=1, FUN=min)

# In this case i apply the model to the observations used to build the model
#it is an "in-sample" error
burger.ld$class[1:10]
burger.ld$posterior[1:10]
#table(burger.ld$posterior[1:10])
# class says the classification

# to see how many errors we got..
table(train$Group,burger.ld$class)
# build a table with true classification (train$Group) and that obtained through
# 39 observations of class 1 attributed to class 1,...
# 4 of class 1 attributed to 2, ecc.
# in total 5 errors.
45/(1+45)

# LOOcv cross-validation  
subway.lda_cv <- lda(train[,2:9],train[,1],CV=TRUE)
subway.lda_cv$posterior[1:10]
subway.lda_cv$class [1:10]
# output is similar, but is the result of 89 different models, applied to datasets with 1 observation missing.
# Thus the values in th enew coordinates are not shown
print(table(train$Group,subway.lda_cv$class))
44/(2+44)
# 6 errors, only 1 more than in-sample
# the method works reasonably well
# with predict() apply results from t to the data of test (unknown) 
# classification of unknown (test) mice..
dat.ld <- predict(mod1,test[,2:9])
dat.ld$class [1:10]  # classification of unknown (test) mice..
# expect 4.4% of these classifications to be wrong

###### QDA #############
mod2 = qda(train[,2:9],train[,1])
mod2

# using 'predict', the model is applied to the observations
burger.ld2 <- predict(mod2)
table(train$Group,burger.ld2$class)
45/(1+45)

# LOOcv cross-validation  
suv.qda_cv <- qda(train[,2:9],train[,1],CV=TRUE)
suv.qda_cv$posterior[1:10]
suv.qda_cv$class [1:10]

# output is similar, but is the result of 89 different models, applied to datasets with 1 observation missing.
# Thus the values in th enew coordinates are not shown
print(table(train$Group,suv.qda_cv$class))
39/(7+39)
# 11 errors, only 1 more than in-sample
# with predict() apply results from t to the data of test (unknown) 
# classification of unknown (test) mice..
dat2.ld <- predict(mod2,test[,2:9])
dat2.ld$class [1:10]  # classification of unknown (test) mice..
# expect 15.3% of these classifications to be wrong

## MClust using full data set. DO NOT INCLUDE DATA AND CLASS TOGETHER!!!!!
library(Flury)
data("microtus")
train <- microtus[microtus$Group != "unknown",]
train$Group <- factor(train$Group) 
test <- microtus[microtus$Group == "unknown",]

pairs(train, col=train$Group)
mc.train.data <- train[,c(2:9)]
mc.train.class <- train[,1]
mc.test.data <- test[,c(2:9)]
mc.test.class <- test[,1]
mc.mod=MclustDA(data=mc.train.data, class=c(mc.train.class), G=3)
#mc.mod <- MclustDA(mc.train.data,class=mc.train.class)
summary(mc.mod)
plot(mc.mod)

par(mfrow=c(1,2))
plot(predict(mc.mod)$z[,1], col=c(train$Group), pch="z")
plot(predict(mc.mod)$z[,1], col=c(test$Group), pch="+")

#### k folds  Cross Validation ########
set.seed(0)
cv = cvMclustDA(mc.mod, nfold = 10, verbose = FALSE)
str(cv)

pred <- predict(mc.mod, mc.test.data)
classError(pred$classification, mc.test.class)$errorRate
summary(mc.mod, newdata = mc.test.data, newclass = mc.test.class)

#cv <- cvMclustDA(mc.mod) # default 10-fold CV
#cv[c("error", "se")]


mclust.pred <- predict.MclustDA(mc.mod, mc.test.data)
1-classError(mclust.pred$classification, mc.test.class)$errorRate
summary(mc.mod, newdata=mc.test.data, newclass=mc.test.class)
