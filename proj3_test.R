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

#### SG: trying PCA
# rationale: find cols which produce greatest variance, only use those for further analysis
# since high variance b/w years = best signal for classification
# according to 80/20 rule, 20% of columns should give 80% of the variance
pcares <- prcomp(mini_sample, center = T, scale = T)
weights <- data.frame(pcares$rotation) # weight, aka importance assigned to each column by the analysis
# variances <- data.frame(pcares$variances)
# doesn't work, don't remember how to get variance of each column. Weights is probably good enough
pca <- data.frame(pcares$x)

# plot weights of PC1 & PC2
library(ggplot2)
pca_weight_plot <- ggplot() +
  geom_text(aes(x = weights$PC1, y = weights$PC2, label=row.names(weights)), size=3)
pca_weight_plot

# Preliminary results: use ###V1###, (V2, V3, V72, V77), (V22, V20, V16, V25, V18, V15, V64, V14, V37)

pca_plot <- ggplot() +
  geom_point(aes(x = pca$PC1, y = pca$PC2))
pca_plot
# want to color by year but not sure how
# none of this makes sense right now...