library(testthat)
library(ForesToolboxRS)
library(stars)
library(raster)
library(caret)
data(endm)
context("ForesToolboxRS ndfiSMA")


training_split = 80
vegt <- extract(img_l8, endm)

endm <- data.frame(vegt, class = endm@data)
endm$class <- as.factor(endm$class)
# Training and Testing
sample_split <- sample(1:dim(endm)[1],
                       training_split*dim(endm)[1]/100)
testing <- endm[sample_split,]
training <- endm[-sample_split,]

errorMatrix <- function(prediction, reference){

  # Confusion matrix
  MC_ini <- table(prediction, reference)
  OA <- sum(diag(MC_ini))/sum(MC_ini)

  ua <- diag(MC_ini)/rowSums(MC_ini)*100
  co <- 100-ua

  pa <- diag(MC_ini)/colSums(MC_ini)*100
  om <- 100-pa

  MCp <- cbind(MC_ini, Total = as.vector(rowSums(MC_ini)),Users_Accuracy = as.vector(ua), Commission=as.vector(co))

  MCf <- rbind(MCp, Total = c(as.vector(colSums(MC_ini)),rep(NA,3)), Producer_Accuracy = c(as.vector(pa), rep(NA,3)),
               Omission = c(as.vector(om), rep(NA,3)))

  mc <- list(MCf = MCf, MC_ini = MC_ini)

  return(mc)

}

# Applying rpart
model_algo <- knn3(class~., data = training, k = 5)
prediction <- predict(model_algo, testing[,-dim(endm)[2]], type = "class")

beginCluster(type="SOCK")
raster_class <- clusterR(img_l8, raster::predict, args = list(model = model_algo, type = "class"))
endCluster()

raster_class <- predict(img_l8, model = model_algo)

# Confusion matrix
MC <- errorMatrix(prediction = prediction, reference = testing[,dim(endm)[2]])

results <- list(Overall_accuracy = (confusionMatrix(MC$MC_ini)$overall[1:6])*100,
                Confusion_matrix = MC$MCf,
                Classification = raster_class)
