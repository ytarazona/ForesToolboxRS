#' Calibrating Supervised classification in Remote Sensing
#'
#' This function allows to calibrate supervised classification in satellite images
#' through various algorithms and using approches such as Set-Approach,
#' Leave-One-Out Cross-Validation (LOOCV), Cross-Validation (k-fold) and
#' Monte Carlo Cross-Validation (MCCV).
#'
#' @author Yonatan Tarazona
#'
#' @section References:
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. (2013).
#' An introduction to statistical learning : with applications in R. New York: Springer.
#'
#' Thomas G. Dietterich. (2006).Approximate Statistical Tests for Comparing Supervised
#' Classification Learning Algorithms. The MIT Press Journal, 10 (7).
#'
#' ' Mountrakis, G., Im, J., Ogole, C. (2011). Support vector machines in remote sensing:
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
#' @section Note: At the moment, only one calibration approach can be used.
#'
#' @details If the "Set-Approach" method is being used, it is not necessary to use parameter \code{k}.
#'  \code{k} only can be used when the Cross-Validation (k-fold) method is used. On the other hand,
#'  to create groups in Cross-Validation, the \code{createFolds} function of the "confusionMatrix" is used.
#'  See \link[confusionMatrix]{createFolds} for more details. In addition, to generate random splits
#'  in Monte Carlos Cross-Validation the \code{generate.split} function of the "WilcoxCV" package was used.
#'  Please see \link[WilcoxCV]{generate.split} for more details.
#'
#' @importFrom caret confusionMatrix createFolds train knn3
#' @importFrom raster getValues extract
#' @importFrom e1071 svm naiveBayes
#' @importFrom randomForest randomForest
#' @importFrom rgeos gIntersects
#' @importFrom WilcoxCV generate.split
#'
#' @param img RasterStack or RasterBrick.
#' @param endm SpatialPointsDataFrame or SpatialPolygonsDataFrame (typically shapefile)
#' containing the training data.
#' @param model Model to use. It can be Support Vector Machine (\link[e1071]{svm}) like
#' \code{model = 'svm'}, Random Forest (\link[randomForest]{randomForest})
#' like \code{model = 'randomForest'}, Naive Bayes (\link[e1071]{naiveBayes})
#' like \code{model = 'naiveBayes'}, Decision Tree (\link[caret]{train})
#' like \code{model = 'LMT'}, Neural Networks (\link[nnet]{nnet})
#' like \code{model = 'nnet'}, K-nearest Neighbors (\link[caret]{knn3}) like \code{model = 'knn'}.
#' @param training_split For splitting samples into two subsets, i.e. training data and
#' for testing data.
#' @param approach Calibration method. There are for options: Simple training and testing
#' (Set-Approach) like \code{approach = 'Set-Approach'}, Leave One Out Cross-Validation (LOOCV) like
#' \code{approach = 'LOOCV'}, Cross-Validation (K-fold) like \code{approach = 'K-fold'} and
#' Monte Carlo Cross-Validation (MCCV) like \code{approach = 'MCCV'}.
#' @param k Number of groups for splitting samples. It must be used only with the
#' Cross-Validation (k-fold) approach.
#' @param iter Number of iterations, i.e number of times the analysis is executed.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#' library(caret)
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
#' knn <- mla(img = img, endm, model = "knn", kmax = 10, training_split = 80)
#'
#' @export
#'

calmla <- function(img, endm, algo = c("svm", "randomForest", "naiveBayes", "LMT", "nnet", "knn"),
                   training_split = 50, approach = "Set-Approach", k = 5, iter = 10, verbose = FALSE, ...){

  if(!inherits(img, "Raster")) stop("img must be a RasterBrick or RasterStack", call. = TRUE)

  if(!compareCRS(img, endm)) stop("img and endm must have the same projection", call. = TRUE)

  if(!gIntersects(as(extent(img),"SpatialPolygons"), as(extent(endm),"SpatialPolygons"))) stop("img and endm do not overlap", call. = TRUE)

  algo_test <- c("svm", "randomForest", "naiveBayes", "LMT", "nnet", "knn")

  if (!algo %in% algo_test) stop("Unsupported algorithm. \nAlgortihm must be svm, randomForest, naiveBayes, LMT, nnet or knn", call. = TRUE)

  vegt <- extract(image, endm)

  endm <- data.frame(vegt, class = endm@data)

  approach_test <- c("Set-Approach", "LOOCV", "k-fold", "MCCV")

  if (!approach %in% approach_test) stop("Unsupported approach. \nApproach must be Set-Approach, LOOCV, k-fold or MCCV", call. = TRUE)

  if ("Set-Approach" %in% approach){

    svm_error_sa <- rep(0,iter)
    rf_error_sa <- rep(0,iter)
    nb_error_sa <- rep(0,iter)
    rp_error_sa <- rep(0,iter)
    nt_error_sa <- rep(0,iter)
    kn_error_sa <- rep(0,iter)

    for (i in 1:iter) {

      # Training and Testing
      sample_split <- sample(1:dim(endm)[1], training_split*dim(endm)[1]/100)
      testing <- endm[sample_split,]
      training <- endm[-sample_split,]

      if ("svm" %in% algo){

        model <- svm(class~., data=training, type = "C-classification", ...)
        prediction <- predict(model, testing)

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        svm_error_sa[i] <- error
        lesvm <- list(svm = svm_error_sa)
      } else {
        lesvm <- list()
      }

      if ("randomForest" %in% algo){

        model <- randomForest(class~., data=training, importance=TRUE, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]])

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        rf_error_sa[i] <- error
        lesvm <- c(lesvm, list(randomForest = rf_error_sa))
      } else {
        lesvm <- list()
      }

      if ("naiveBayes" %in% algo){

        model <- naiveBayes(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]])

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        nb_error_sa[i] <- error
        lesvm <- c(lesvm, list(naiveBayes = nb_error_sa))
      } else {
        lesvm <- list()
      }

      if ("LMT" %in% algo){

        model <- train(class~., method = "LMT", data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]], type = "raw")

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        rp_error_sa[i] <- error
        lesvm <- c(lesvm, list(rpart = rp_error_sa))
      } else {
        lesvm <- list()
      }

      if ("nnet" %in% algo){

        nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
        model <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]], type = "raw")

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        nt_error_sa[i] <- error
        lesvm <- c(lesvm, list(nnet = nt_error_sa))
      } else {
        lesvm <- list()
      }

      if ("knn" %in% algo){

        model <- knn3(class~., data = training, k = 5, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]], type = "class")

        # Matriz de confusion
        MC <- table(prediction, testing)
        # Precision global
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        kn_error_sa[i] <- error
        lesvm <- c(lesvm, list(train.kknn = kn_error_sa))
      } else {
        lesvm <- list()
      }

      return(lesvm)

    }

  }

  if ("LOOCV" %in% approach){

    svm_error_loocv <- rep(0,iter)
    rf_error_loocv <- rep(0,iter)
    nb_error_loocv <- rep(0,iter)
    rp_error_loocv <- rep(0,iter)
    nt_error_loocv <- rep(0,iter)
    kn_error_loocv <- rep(0,iter)

    for (i in 1:iter) {

      svm_ini_error <- 0
      rf_ini_error <- 0
      nb_ini_error <- 0
      rp_ini_error <- 0
      nt_ini_error <- 0
      kn_ini_error <- 0

      for (j in 1:dim(endm)[1]) {

        # Training and Testing
        testing <- endm[j,]
        training <- endm[-j,]

        if ("svm" %in% algo){

          model <- svm(class~., data=training, type = "C-classification", ...)
          prediction <- predict(model, testing)
          if(prediction != testing$class){
            svm_ini_error <- svm_ini_error + 1
          }
          svm_error_loocv[i] <- svm_ini_error/dim(endm)[1]
          losvm <- list(svm_error_loocv = svm_error_loocv)
        } else {
          losvm <- c()
        }

        if ("randomForest" %in% algo){

          model <- randomForest(class~., data=training, importance=TRUE, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]])
          if(prediction != testing$class){
            rf_ini_error <- rf_ini_error + 1
          }
          rf_error_loocv[i] <- rf_ini_error/dim(endm)[1]
          losvm <- c(losvm, list(rf_error_loocv = rf_error_loocv))
        } else {
          losvm <- c()
        }

        if ("naiveBayes" %in% algo){

          model <- naiveBayes(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]])
          if(prediction != testing$class){
            nb_ini_error <- nb_ini_error + 1
          }
          nb_error_loocv[i] <- nb_ini_error/dim(endm)[1]
          losvm <- c(losvm, list(nb_error_loocv = nb_error_loocv))
        } else {
          losvm <- c()
        }

        if ("LMT" %in% algo){

          model <- train(class~., method = "LMT", data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]], type = "raw")
          if(prediction != testing$class){
            rp_ini_error <- rp_ini_error + 1
          }
          rp_error_loocv[i] <- rp_ini_error/dim(endm)[1]
          losvm <- c(losvm, list(rp_error_loocv = rp_error_loocv))
        } else {
          losvm <- c()
        }

        if ("nnet" %in% algo){

          nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
          model <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]], type = "raw")
          if(prediction != testing$class){
            nt_ini_error <- nt_ini_error + 1
          }
          rt_error_loocv[i] <- nt_ini_error/dim(endm)[1]
          losvm <- c(losvm, list(nt_error_loocv = nt_error_loocv))
        } else {
          losvm <- c()
        }

        if ("knn" %in% algo){

          model <- knn3(class~., data = training, k = 5, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]], type = "class")
          if(prediction != testing$class){
            kn_ini_error <- kn_ini_error + 1
          }
          kn_error_loocv[i] <- kn_ini_error/dim(endm)[1]
          losvm <- c(losvm, list(kn_error_loocv = kn_error_loocv))
        } else {
          losvm <- c()
        }
      }
    }

    return(losvm)

  }


  if ("k-fold" %in% approach){

    svm_error_sa <- rep(0,iter)
    rf_error_sa <- rep(0,iter)
    nb_error_sa <- rep(0,iter)
    rp_error_sa <- rep(0,iter)
    nt_error_sa <- rep(0,iter)
    kn_error_sa <- rep(0,iter)

    for (i in 1:iter) {

      svm_ini_error <- 0
      rf_ini_error <- 0
      nb_ini_error <- 0
      rp_ini_error <- 0
      nt_ini_error <- 0
      kn_ini_error <- 0

      groups <- createFolds(1:dim(endm)[1], 10)

      for(g in 1:K){

        # Training and Testing
        muestra <- groups[[g]]
        testing <- endm[muestra,]
        training <- endm[-muestra,]

        if ("svm" %in% algo){

          model <- svm(class~., data=training, ...)
          prediction <- predict(model, testing)

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          svm_ini_error <- svm_ini_error + error
          svm_error_sa[i] <- svm_ini_error/k
          lesvm <- list(svm = svm_error_sa)
        } else {
          lesvm <- list()
        }

        if ("randomForest" %in% algo){

          model <- randomForest(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]])

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          rf_ini_error <- rf_ini_error + error
          rf_error_sa[i] <- rf_ini_error/k
          lesvm <- c(lesvm, list(randomForest = rf_error_sa))
        } else {
          lesvm <- list()
        }

        if ("naiveBayes" %in% algo){

          model <- naiveBayes(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]])

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          nb_ini_error <- nb_ini_error + error
          nb_error_sa[i] <- nb_ini_error/k
          lesvm <- c(lesvm, list(naiveBayes = nb_error_sa))
        } else {
          lesvm <- list()
        }

        if ("rpart" %in% algo){

          model <- rpart(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]], type="class")

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          rp_ini_error <- rp_ini_error + error
          rp_error_sa[i] <- rp_ini_error/k
          lesvm <- c(lesvm, list(rpart = rp_error_sa))
        } else {
          lesvm <- list()
        }

        if ("nnet" %in% algo){

          model <- nnet(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]], type="class")

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          nt_ini_error <- nt_ini_error + error
          nt_error_sa[i] <- nt_ini_error/k
          lesvm <- c(lesvm, list(nnet = nt_error_sa))
        } else {
          lesvm <- list()
        }

        if ("train.kknn" %in% algo){

          model <- train.kknn(class~., data=training, ...)
          prediction <- predict(model, testing[,-dim(endm)[2]])

          # Matriz de confusion
          MC <- table(prediction, testing)
          # Precision global
          oa <- sum(diag(MC))/sum(MC)
          error <- 1 - oa
          kn_ini_error <- kn_ini_error + error
          kn_error_sa[i] <- kn_ini_error/k
          lesvm <- c(lesvm, list(train.kknn = kn_error_sa))
        } else {
          lesvm <- list()
        }
      }
    }

    return(lesvm)

  }


  if ("MCCV" %in% approach){

    svm_error_sa <- rep(0,iter)
    rf_error_sa <- rep(0,iter)
    nb_error_sa <- rep(0,iter)
    rp_error_sa <- rep(0,iter)
    nt_error_sa <- rep(0,iter)
    kn_error_sa <- rep(0,iter)

    n <- dim(endm)[1]
    groups_mc <- generate.split(niter = iter, n = n, ntest = 100 - training_split)

    for (i in 1:iter) {

      # Training and Testing
      muestra <- groups_mc[[i]]
      testing <- endm[muestra,]
      training <- endm[-muestra,]

      if ("svm" %in% algo){

        model <- svm(class~., data=training, ...)
        prediction <- predict(model, testing)

        # Confusion matrix
        MC <- table(prediction, testing)
        # error
        svm_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- list(svm = svm_error_sa)
      } else {
        lesvm <- list()
      }

      if ("randomForest" %in% algo){

        model <- randomForest(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]])

        # Matriz de confusion
        MC <- table(prediction, testing)
        # error
        rf_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- c(lesvm, list(randomForest = rf_error_sa))
      } else {
        lesvm <- list()
      }

      if ("naiveBayes" %in% algo){

        model <- naiveBayes(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]])

        # Matriz de confusion
        MC <- table(prediction, testing)
        # error
        nb_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- c(lesvm, list(naiveBayes = nb_error_sa))
      } else {
        lesvm <- list()
      }

      if ("rpart" %in% algo){

        model <- rpart(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]], type="class")

        # Matriz de confusion
        MC <- table(prediction, testing)
        # error
        rp_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- c(lesvm, list(rpart = rp_error_sa))
      } else {
        lesvm <- list()
      }

      if ("nnet" %in% algo){

        model <- nnet(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]], type="class")

        # Matriz de confusion
        MC <- table(prediction, testing)
        # error
        nt_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- c(lesvm, list(nnet = nt_error_sa))
      } else {
        lesvm <- list()
      }

      if ("train.kknn" %in% algo){

        model <- train.kknn(class~., data=training, ...)
        prediction <- predict(model, testing[,-dim(endm)[2]])

        # Matriz de confusion
        MC <- table(prediction, testing)
        # error
        kn_error_sa[i] <- 1 - sum(diag(MC))/sum(MC)
        lesvm <- c(lesvm, list(train.kknn = kn_error_sa))
      } else {
        lesvm <- list()
      }
    }

    return(lesvm)

  }
}

