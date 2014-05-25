#set working directory

#read in data set
mini_sample <- read.csv("Year10000RS.csv", header=FALSE)
head(mini_sample)

#develop a model with the training set
#linear fit
#how do we develop this first model?? How do we know which varaiables to include? Do we need a special R package?
training <- lm(V1 ~ V2 +V3 , data=mini_sample)
summary(training)

# then we want to predict the test dataset
# test_sample <- read.csv(another data set)
#test <- predict(training, data=test_sample)



#other regression options
#polynomial fit (2nd order)
training_poly <- lim( V1 ~ poly(V2, 2), data=mini_sample)