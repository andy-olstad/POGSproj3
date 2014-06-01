
## Reading in the data (first 10000 rows)
data <- read.table("YearPredictionMSD.txt", sep=",")
dim(data)
unique(data[,1])  # Looking at the years we have
table(data[,1])  # Table of our years...small numbers before 1990's

#plotting years
years<-table(data[,1])
plot(1924:2010,years[2:88],type="l",main="Number of songs per year in data set",ylab="Numer of songs")

# Splitting the training set and the test set
X.train <- as.matrix(data[1:463715,2:91])
X.test <- as.matrix(data[463716:515345, 2:91])
Y.train <- as.vector(data[1:463715, 1])
Y.test <- as.vector(data[463716:515345, 1])


# Tabling the years in the training set
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


