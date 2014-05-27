
## Reading in the data (first 10000 rows)
data <- read.table("YearPredictionMSD.txt", nrows=10000, sep=",")
dim(data)
names(data)
unique(data[,1])  # Looking at the years we have
table(data[,1])   # Table of our years...small numbers before 1990's

# Splitting the training set and the test set
X.train <- data[1:7500,2:91]
X.test <- data[7501:10000, 2:91]
Y.train <- data[1:7500, 1]
Y.test <- data[7501:10000, 1]
# Making the data sets numeric matrices
X.train <- as.matrix(X.train)
X.test <- as.matrix(X.test)

# Performing PCA on the Correlation matrix of predictors for the training set
cor.X <- cor(X.train)
prin.comp <- prcomp(cor.X)
summary(prin.comp)
# Pulling out the first 8 PCs
# The reason to pull the first 8 was they explain a lot of variation (~80%) in the predictors
PC <- prin.comp$x[,1:17]
# Creating the new predictor set based on these PCs for the training data
new.pred <- X.train%*%PC
# Running the linear model on the training data
mod <- lm(Y.train ~ new.pred)
summary(mod)
# Pulling out the coefficients to use on the test data
coef.mod.pc <- mod$coef

# Predicting the test data
X.test.PC <- cbind(rep(1, 2500), X.test %*% PC)
Preds.test <- X.test.PC %*% coef.mod.pc
Preds.test
plot(Y.test, Y.test-Preds.test, pch=16)
# Not doing to well...but it is probably coming from the small amount of data before 1990's
