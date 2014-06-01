library(dplyr)

## Reading in the data (first 10000 rows)
data <- read.table("YearPredictionMSD.txt", sep=",")
dim(data)
unique(data[,1])  # Looking at the years we have
table(data[,1])  # Table of our years...small numbers before 1990's

# Splitting the training set and the test set
X.train <- as.matrix(data[1:463715,2:91])
X.test <- as.matrix(data[463716:515345, 2:91])
Y.train <- as.vector(data[1:463715, 1])
Y.test <- as.vector(data[463716:515345, 1])

# Performing PCA on the Correlation matrix of predictors for the training set
cor.X <- cor(X.train)
prin.comp <- prcomp(cor.X)
summary(prin.comp)
# Pulling out the first 8 PCs
# The reason to pull the first 8 was they explain a lot of variation (~80%) in the predictors
PC <- prin.comp$x[,1:8]
# Creating the new predictor set based on these PCs for the training data
new.pred <- X.train%*%PC

file_out <- cbind(Y.train, new.pred)
# Writing a CSV file to use: The 8 PCs as new predictors
write.csv(file_out, "PC_predictors_train.csv")

# Running the linear model on the training data
mod <- lm(Y.train ~ new.pred)
summary(mod)
# Pulling out the coefficients to use on the test data
coef.mod.pc <- mod$coef

# Predicting the test data
X.test.PC <- cbind(rep(1, length(Y.test)), X.test %*% PC)
Preds.test <- X.test.PC %*% coef.mod.pc
Res.test <- Y.test-Preds.test

write.csv(Res.test, "Residuals_test.csv")
plot(Y.test, Res.test, pch=16)
# Not doing too well...but it is probably coming from the small amount of data before 1990's

# SG: attempting multi-gaussian
library(mclust)
new.pred <- read.csv("PC_predictors_train.csv", header = T)
head(new.pred[,3:9])
model1 <- Mclust(new.pred[,3:9])


# AO: Tabling the years in the training set
table(Y.train)
year<-rep(0,90)
for(i in 1922:2011){
  year[i-1921]<-length(which(data[,1]==i))
}
decade<-rep(0,10)
for(i in 192:201){
  decade[i-191]<-length(which(trunc(data[,1]/10)==i))
}
barplot(decade,names=c("1920s","1930s","1940s",
                       "1950s","1960s","1970s","1980s","1990s","2000s","2010-1"),
        ylab="number of songs in training set",main="Highly Uneven Distribution of Songs")

plot(1922:2011,year,type="l",main="Number of Songs per Year in Training Set",ylab="Numer of songs",
     xlab="Year of Song's Release")

#SG: plotting years
library(ggplot2)
require(scales)
year_data <- read.table("YearDistributions.tab", header = F)
year_dist <- ggplot() + 
  layer(data = year_data,
        geom = "line",
        mapping = aes(x = V2, y = V1)) +
  labs(x = "Year", y = "Number of songs") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme_bw(18)
year_dist
ggsave("year_dist.pdf")

# SG: plot predictions
library(dplyr)
pred_df <- as.data.frame(Preds.test)
predictions <- mutate(pred_df, year = round(V1)) 

predict_plot <- ggplot() + 
  geom_histogram(aes(x = predictions$year), binwidth = 2) +
  labs(x = "Year", y = "Number of songs") +
  scale_x_continuous(breaks=pretty_breaks(n=10), limits = c(1920, 2015)) +
  theme_bw(18)
predict_plot
ggsave("predict_plot.pdf")

# SG: plot residuals
plot_resids <- ggplot() + 
  geom_point(aes(x = Y.test, y = Res.test)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = c(-10,10)) +
  labs(x = "Year", y = "Prediction residual") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  theme_bw(18)
plot_resids
ggsave("plot_resids.pdf")

##########double-checking by ignoring PCA for a moment
Response<-Y.train
Predictors<-X.train[,1:12]
model<-lm(Response~Predictors)
summary(model)

plot(model$fit,model$res)
plot(jitter(model$fit),jitter(model$res),pch=".")
plot(jitter(model$fit+model$res),jitter(model$fit),pch=".",ylab="Predicted Year",xlab="Year")
legend("bottomright",legend="R squared is 0.15")
title("Predicton using presumed 12 means")

bigmodel<-lm(Response~X.train)
plot(jitter(bigmodel$fit-model$res),jitter(bigmodel$fit),pch=".",ylab="Predicted Year",xlab="Year")
legend("bottomright",legend="R squared is 0.24")
title("Predicton using all 90 data columns")

#plotting years
years<-table(data[,1])
plot(1924:2010,years[2:88],type="l",main="Number of songs per year in data set",ylab="Numer of songs")

plot(jitter(data[,1]),jitter(data[,2]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,3]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,4]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,5]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,6]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,7]),pch=".",xlab="Year")
par(mfrow=c(4,4))
plot(jitter(data[,1]),jitter(data[,8]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,9]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,10]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,11]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,12]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,13]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,14]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,15]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,16]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,17]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,18]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,19]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,20]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,21]),pch=".",xlab="Year")
plot(jitter(data[,1]),jitter(data[,22]),pch=".",xlab="Year")

for(i in 23:38){
plot(jitter(data[,1]),jitter(data[,i]),pch=".",xlab="Year")
}

for(i in 39:54){
plot(jitter(data[,1]),jitter(data[,i]),pch=".",xlab="Year")
}

for(i in 55:70){
plot(jitter(data[,1]),jitter(data[,i]),pch=".",xlab="Year")
}

for(i in 71:86){
plot(jitter(data[,1]),jitter(data[,i]),pch=".",xlab="Year")
}

for(i in 87:90){
plot(jitter(data[,1]),jitter(data[,i]),pch=".",xlab="Year")
}

par(mfrow=c(3,4))
for(i in 2:13){
plot(jitter(data[,1]),data[,i],pch=".",xlab="Year")
}

