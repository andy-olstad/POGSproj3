
## Reading in the data (first 10000 rows)
data <- read.table("YearPredictionMSD.txt", sep=",")
dim(data)
unique(data[,1])  # Looking at the years we have
table(data[,1])  # Table of our years...small numbers before 1990's

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

# Splitting the training set and the test set
X.train <- as.matrix(data[1:463715,2:91])
X.test <- as.matrix(data[463716:515345, 2:91])
Y.train <- as.vector(data[1:463715, 1])
Y.test <- as.vector(data[463716:515345, 1])


# Tabling the years in the training set
table(Y.train)

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

# SG: plot predictions
library(dplyr)
pred_df <- as.data.frame(Preds.test)
predictions <- mutate(pred_df, year = round(V1)) 

predict_plot <- ggplot() + 
  geom_histogram(aes(x = predictions$year), binwidth = 2) +
  labs(x = "Year", y = "Number of songs") +
  scale_x_continuous(breaks=pretty_breaks(n=10), limits = c(1920, 2015)) +
 # xlim(c(1920,2015)) +
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
