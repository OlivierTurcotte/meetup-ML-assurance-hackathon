################## Guillaume Michel et Olivier Turcotte ###############

# Downloads all the package needed if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, rpart)

data.train.features <- fread("data/train-features.csv")
data.train.reponses <- fread("data/data-id-train.csv")

# Creating a new dataframe for modelling training.
# Dataframe content : [Y,V1,V2,...,V2048]
data.train.features <-
  as.data.frame(data.train.features[order(data.train.features$ID), ])
train.data <-
  cbind(data.train.reponses$green_roof_ind , data.train.features[, -1])
names(train.data)[1] <- "Y"



#Validation sampling, if needed
set.seed(1)
sample_ratio <- sample(nrow(train.data), nrow(train.data)*0.75)
validation.data  <- train.data[-sample_ratio,]
train.data       <- train.data[sample_ratio,]

# PCA analysis ----
pca <- prcomp(train.data[, -1], center = TRUE, scale = TRUE)

# Explication of variance for each principal components
expl.var <- pca$sdev ^ 2 / sum(pca$sdev ^ 2) * 100
plot(expl.var)

# From plot:
n_pc <- 200

# Retrieving the data from the PCA tranformation for modeling
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

# Precision
VP <-
  sum(((glm.prediction >= 0.5) == validation.data$Y &
         (glm.prediction >= 0.5)
  ))
FP <-
  sum(((glm.prediction >= 0.5) != validation.data$Y &
         (glm.prediction >= 0.5)
  ))
VP / (VP + FP)

# Recall
FN <-
  sum(((glm.prediction >= 0.5) != validation.data$Y &
         (glm.prediction < 0.5)
  ))
VP / (VP + FN)

# Ambiguity visualisation
ggplot(data = as.data.frame(table(round(glm.prediction * 10) / 10)),
       aes(x = Var1, y = Freq))  + geom_bar(stat = "identity") + xlab("P( Observation = Toit Vert)")

