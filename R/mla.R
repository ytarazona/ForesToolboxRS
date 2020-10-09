#' Supervised and unsupervised classification in Remote Sensing
#'
#' This developed function allows to execute supervised and unsupervised classification
#' in satellite images through various algorithms.
#'
#' @author Yonatan Tarazona
#'
#' @section References:
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. (2013).
#' An introduction to statistical learning : with applications in R. New York: Springer.
#'
#' Mountrakis, G., Im, J., Ogole, C. (2011). Support vector machines in remote sensing:
#' A review. ISPRS Journal of Photogrammetry and Remote Sensing, 66, 247-259.
#'
#' Belgiu, M., Dragut., L. (2016). Random Forest in Remote Sensing: A Review of Applications
#' and Future Directions. ISPRS Journal of Photogrammetry and Remote Sensing, 114, 24-31.
#'
#' Maxwell, A.E., Warner, T.A., Fang, F. (2018). Implementation of machine-learning
#' classification in remote sensing: an applied review. International Journal of Remote
#' Sensing, 29(9), 2784-2817.
#'
#' Pradhan, R., Ghose, M.K., Jeyaram, A. (2010). Land Cover Classification of Remotely
#' Sensed Satellite Data using Bayesian and Hybrid classifier. International Journal
#' of Computer Applications, 7(11).
#'
#' Holloway, J., Mengersen, K. (2018). Statistical Machine Learning Methods and Remote
#' Sensing for Sustainable Development Goals: A Review. Remote Sensing, 10(9), 1365.
#'
#' @section Note:
#'
#' @importFrom caret confusionMatrix
#' @importFrom raster getValues extract
#' @importFrom e1071 svm naiveBayes
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest
#' @importFrom nnet nnet
#' @importFrom kknn train.kknn
#' @importFrom rgeos gIntersects
#'
#' @param img RasterStack or RasterBrick.
#' @param endm SpatialPointsDataFrame or SpatialPolygonsDataFrame (typically shapefile)
#' containing the training data.
#' @param model Model to use. It can be \link[e1071]{svm} like \code{model = 'svm'}, \link[randomForest]{randomForest} like
#' \code{model = 'randomForest'}, \link[e1071]{naiveBayes} like \code{model = 'naiveBayes'}, \link[rpart]{rpart} like
#' \code{model = 'rpart'}, \link[nnet]{nnet} like \code{model = 'nnet'}, \link[kknn]{train.kknn} like
#' \code{model = 'train.kknn'}.
#' @param training_split For splitting samples into two subsets, i.e. training data and for
#' testing data.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#' library(snow)
#'
#' # Load the dataset
#' data(FTdata)
#'
#' # Signatures
#' soil<-c(8980,8508,8704,13636,16579,11420)
#' forest<-c(8207,7545,6548,16463,9725,6673)
#' water<-c(9276,9570,10089,6743,5220,5143)
#' val <- matrix(c(soil,forest,water), 3, 6, byrow = TRUE)
#' colnames(val) <- c("B1", "B2", "B3","B4","B5","B6")
#' endm <- data.frame(val, class = c("soil","forest","water"))
#'
#' reptime <- 50
#' indx <- rep(1:nrow(endm), reptime)
#' endm <- endm[indx, ]
#'
#' # K-Nearest Neighbor Classifier
#' knn <- mla(img = img, endm, model = "kknn", kmax = 10, training_split = 80)
#'
#' @export
#'

mla <- function(img, endm, model = "svm", training_split = 80, verbose = FALSE, ...){

  if(!inherits(img, "Raster")) stop("img must be a RasterBrick or RasterStack", call. = TRUE)

  if(!compareCRS(img, endm)) stop("img and endm must have the same projection", call. = TRUE)

  if(!gIntersects(as(extent(img),"SpatialPolygons"), as(extent(endm),"SpatialPolygons"))) stop("img and endm do not overlap", call. = TRUE)

  vegt <- extract(image, endm)

  endm <- data.frame(vegt, class = endm@data)

  if (model!="kmeans"){

    if (is.character(endm$class)) {
      endm$class <- as.factor(endm$class)
    }

    # Training and Testing
    sample_split <- sample(1:dim(endm)[1],
                           training_split*dim(endm)[1]/100)
    testing <- endm[sample_split,]
    training <- endm[-sample_split,]
  }

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

  if (model=="kmeans") {

    # Values and NA position
    vr <- getValues(img)
    i <- which(!is.na(vr))

    # Applying kmeans
    km <- kmeans(na.omit(vr), ...)

    raster_class <- raster(img)
    raster_class[i] <- km$cluster

  } else if (model=="svm"){

    # Applying svm
    model_algo <- svm(class~., data=training, ...)
    prediction <- predict(model_algo, testing)

  } else if(model=="randomForest"){

    # Applying randomForest
    model_algo <- randomForest(class~., data=training, ...)
    prediction <- predict(model_algo, testing[,-dim(endm)[2]])

  } else if (model=="naiveBayes"){

    # Applying naiveBayes
    model_algo <- naiveBayes(class~., data=training, ...)
    prediction <- predict(model_algo, testing[,-dim(endm)[2]])

  } else if (model=="rpart"){

    # Applying rpart
    model_algo <- rpart(class~., data=training, ...)
    prediction <- predict(model_algo, testing[,-dim(endm)[2]], type="class")

  } else if (model=="nnet"){

    # Applying nnet
    model_algo <- nnet(class~., data=training, ...)
    prediction <- predict(model_algo, testing[,-dim(endm)[2]], type="class")

  } else if (model == "train.kknn") {

    # Applying kknn
    model_algo <- train.kknn(class~., data=training, ...)
    prediction <- predict(model_algo, testing[,-dim(endm)[2]])

  } else stop("Unsupported classification method.", call. = TRUE)

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Model summary " , paste0(rep("*",10), collapse = "")))
    print(model_algo)
  }

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Apply model to the raster " , paste0(rep("*",10), collapse = "")))
  }

  if (model!="kmeans"){

    # Apply model to the raster
    beginCluster(type="SOCK")
    raster_class <- clusterR(img, raster::predict, args = list(model = model_algo))
    endCluster()
  }

  if (model=="kmeans"){

    return(raster_class)
    names(raster_class) <- kmeans_class

  } else {

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Confusion Matrix and finel result " , paste0(rep("*",10), collapse = "")))
    }

    # Confusion matrix
    MC <- errorMatrix(prediction = prediction, reference = testing[,dim(endm)[2]])

    results <- list(Overall_accuracy = (confusionMatrix(MC$MC_ini)$overall[1:6])*100,
                    Confusion_matrix = MC$MCf,
                    Classification = raster_class)
    return(results)
  }

}
