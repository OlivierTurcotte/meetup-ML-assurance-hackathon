################## Guillaume Michel et Olivier Turcotte ###############

# Download all the package needed if not already installed
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,logisticPCA,rARPACK,ggplot2,rpart)

data_train_features <- fread("data/train-features.csv")
data_train_reponses <- fread("data/data-id-train.csv")

# Creating a new dataframe for modelling training.
# The new dataframe looks like this : [Y,V1,V2,...,V2048]
data_train_features <-  as.data.frame(data_train_features[order(data_train_features$ID),])
data_train_features_Y <- cbind(data_train_reponses$green_roof_ind ,data_train_features[,-1])
names(data_train_features_Y)[1] <- "Y" 

set.seed(1)
sample_ratio <- sample(nrow(data_train_features), nrow(data_train_features)*0.75)

train.data       <- data_train_features_Y[sample_ratio,]
validation.data  <- data_train_features_Y[-sample_ratio,]

# PCA analysis ----
pca <- prcomp(train.data[,-1], center=TRUE, scale=TRUE)

expl.var <- pca$sdev^2/sum(pca$sdev^2)*100
plot(expl.var)

# From plot:
n_pc <- 200

pca.train.data <- as.data.frame(pca$x[,1:n_pc]) # From plot analysis
pca.train.data["Y"] <- train.data[,1]

glm.model <- suppressWarnings(glm(formula = cbind(Y,1-Y) ~ . ,family = binomial,data = pca.train.data))

# Converting validation data into PCA
pca.validation.data <- predict(pca, validation.data[,-1])
pca.validation.data <- as.data.frame(pca.validation.data)[,1:n_pc]

glm.prediction <- predict(glm.model,pca.validation.data,type = "response")

sum(glm.prediction > 0.5)
sum(validation.data$Y)


# DECISION TREE ---- 


rpart.model <- rpart(Y ~ .,data = pca.train.data,method = "anova")
rpart.prediction <- predict(rpart.model,pca.validation.data)
sum(rpart.prediction >= 0.5)

