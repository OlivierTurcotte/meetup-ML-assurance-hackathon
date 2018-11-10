################## Guillaume Michel et Olivier Turcotte ###############

# Downloads all the package needed if not already installed
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(data.table, logisticPCA, rARPACK, ggplot2, rpart)

data_train_features <- fread("data/train-features.csv")
data_train_reponses <- fread("data/data-id-train.csv")

# Creating a new dataframe for modelling training.
# Dataframe content : [Y,V1,V2,...,V2048]
data_train_features <-
  as.data.frame(data_train_features[order(data_train_features$ID),])
train.data <-
  cbind(data_train_reponses$green_roof_ind , data_train_features[, -1])
names(train.data)[1] <- "Y"

# Validation sampling, if needed
# set.seed(1)
# sample_ratio <- sample(nrow(train.data), nrow(train.data)*0.75)
# validation.data  <- train.data[-sample_ratio,]
# train.data       <- train.data[sample_ratio,]

validation.data <- fread("path/to/test/dataset")

# PCA analysis ----
pca <- prcomp(train.data[, -1], center = TRUE, scale = TRUE)

expl.var <- pca$sdev ^ 2 / sum(pca$sdev ^ 2) * 100
gg <- cbind(seq(1:1350),as.data.frame(expl.var))
names(gg) <- c("Index","Expl.Var")
gg[1,]
ggplot(gg,aes(x = n1, y = n2)) + geom_point(aes(colour = n2))

# From plot:
n_pc <- 200

pca.train.data <- as.data.frame(pca$x[, 1:n_pc]) 
pca.train.data["Y"] <- train.data[, 1]

glm.model <-
  suppressWarnings(glm(
    formula = cbind(Y, 1 - Y) ~ . ,
    family = binomial,
    data = pca.train.data
  ))

# Converting validation data into PCA
pca.validation.data <- predict(pca, validation.data[, -1])
pca.validation.data <- as.data.frame(pca.validation.data)[, 1:n_pc]

glm.prediction <-
  predict(glm.model, pca.validation.data, type = "response")

# Model predictivity analysis ----

# Number of greenroofs detected
sum(glm.prediction > 0.5)

# Real number of greenroofs
sum(validation.data$Y)

# Predictive power
sum((glm.prediction >= 0.5) == validation.data$Y) / length(validation.data$Y)

# Number of predictions really far from the truth
sum(validation.data$Y[which(round(glm.prediction) == 0)])

# Ambiguity visualisation
ggplot(data = as.data.frame(table(round(glm.prediction * 10) / 10)), 
       aes(x = Var1, y = Freq))  + geom_bar(stat = "identity")

# DECISION TREE ----

rpart.model <- rpart(Y ~ ., data = pca.train.data, method = "anova")
rpart.prediction <- predict(rpart.model, pca.validation.data)

sum(rpart.prediction >= 0.5)
