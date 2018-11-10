################## Ceci est l'art de Guillaume Michel et Olivier Turcotte ###############

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

data_train_features <- fread("data/train-features.csv")
data_train_reponses <- fread("data/data-id-train.csv")
