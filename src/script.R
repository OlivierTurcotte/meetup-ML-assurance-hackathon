################## Ceci est l'art de Guillaume Michel et Olivier Turcotte ###############
data("house_votes84")
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,logisticPCA,rARPACK)

data_train_features <- fread("data/train-features.csv")
data_train_reponses <- fread("data/data-id-train.csv")

# Creating a new dataframe for modelling training
data_train_features <-  as.data.frame(data_train_features[order(data_train_features$ID),])
data_train_features_svd <- cbind(data_train_reponses$green_roof_ind ,data_train_features[,-1])
names(data_train_features_svd)[1] <- "Y"


# data_train_features_svd2 <- apply(data_train_features[,-1],2,function(x) log(1+x))
data_train_features_svd2 <- log(1+data_train_features[,-1])
data.pca <- prcomp(~ ., data=data_train_features_svd2, na.action=na.omit, scale=TRUE,center = TRUE)




logsvd_model <-  logisticSVD(data_train_features_svd2, k = 2)





