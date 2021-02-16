#' Calibrating Supervised classification in Remote Sensing
#'
#' This function allows to calibrate supervised classification in satellite images
#' through various algorithms and using approaches such as Set-Approach,
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
#' (Set-Approach) like \code{approach == 'Set-Approach'}, Leave One Out Cross-Validation (LOOCV) like
#' \code{approach == 'LOOCV'}, Cross-Validation (K-fold) like \code{approach == 'k-fold'} and
#' Monte Carlo Cross-Validation (MCCV) like \code{approach == 'MCCV'}.
#' @param k Number of groups for splitting samples. It must be used only with the
#' Cross-Validation (K-fold) approach.
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
#' # Support Vector Machine and Random Forest Classifiers
#' # Calibrating using "Set-Approach"
#' cal_ml <- calmla(img = image, endm = endm, model = c("svm", "randomForest"), training_split = 80,
#'            approach = "Set-Approach", iter = 10)
#'
#' @export
#'

calmla <- function(img, endm, model = c("svm", "randomForest", "naiveBayes", "LMT", "nnet", "knn"),
                   training_split = 50, approach = "Set-Approach", k = 10, iter = 10, verbose = FALSE, ...){

  if(!inherits(img, "Raster")) stop("img must be a RasterBrick or RasterStack", call. = TRUE)

  if(!compareCRS(img, endm)) stop("img and endm must have the same projection", call. = TRUE)

  if(inherits(endm, 'SpatialPointsDataFrame')) {
    TypeEndm <- "points"

  } else {

    if(inherits(endm, 'SpatialPolygonsDataFrame')){
      TypeEndm <- "polygons"

    } else stop("Signatures (endm) must be SpatialPointsDataFrame or SpatialPolygonsDataFrame", call. = TRUE)
  }

  algo_test <- c("svm", "randomForest", "naiveBayes", "LMT", "nnet", "knn")

  if (!identical(intersect(model, algo_test), model)) stop("Unsupported algorithm, it must be svm, randomForest, naiveBayes, LMT, nnet or knn", call. = TRUE)

  #if (model %in% algo_test) stop("Unsupported algorithm, it must be svm, randomForest, naiveBayes, LMT, nnet or knn", call. = TRUE)

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " The origin of the signatures are ", TypeEndm , paste0(rep("*",10), collapse = "")))
  }

  vegt <- extract(image, endm)

  endm <- data.frame(vegt, class = endm@data)

  #if (is.numeric(endm$class)) {
    #endm$class <- as.factor(endm$class)
  #}

  approach_test <- c("Set-Approach", "LOOCV", "k-fold", "MCCV")

  if (!approach %in% approach_test) stop("Unsupported approach. \nApproach must be Set-Approach, LOOCV, k-fold or MCCV", call. = TRUE)

  if (approach == "Set-Approach"){

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calibrating with Set-Approach ", TypeEndm , paste0(rep("*",10), collapse = "")))
    }

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
      testing$class <- as.factor(testing$class)

      training <- endm[-sample_split,]
      training$class <- as.factor(training$class)


      model_svm <- svm(class~., data=training, type = "C-classification", ...)
      prediction <- predict(model_svm, testing)
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # Overall accuracy
      oa <- sum(diag(MC))/sum(MC)
      error <- 1 - oa
      svm_error_sa[i] <- error


      model_rf <- randomForest(class~., data=training, importance=TRUE, ...)
      prediction_rf <- predict(model_rf, testing[,-dim(endm)[2]])
      # Confusion Matrix
      MC_rf <- table(prediction_rf, testing[,dim(endm)[2]])
      # Precision global
      oa_rf <- sum(diag(MC_rf))/sum(MC_rf)
      error_rf <- 1 - oa_rf
      rf_error_sa[i] <- error_rf


      models <- naiveBayes(class~., data=training, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]])
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # Overall accuracy
      oa <- sum(diag(MC))/sum(MC)
      error <- 1 - oa
      nb_error_sa[i] <- error


      models <- train(class~., method = "LMT", data=training, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # Overall accuracy
      oa <- sum(diag(MC))/sum(MC)
      error <- 1 - oa
      rp_error_sa[i] <- error


      nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
      models <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # Overall accuracy
      oa <- sum(diag(MC))/sum(MC)
      error <- 1 - oa
      nt_error_sa[i] <- error


      models <- knn3(class~., data = training, k = 5, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "class")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # Overall accuracy
      oa <- sum(diag(MC))/sum(MC)
      error <- 1 - oa
      kn_error_sa[i] <- error

    }

    # svm
    int_svm <- intersect("svm", model)
    if (length(int_svm) == 0) int_svm <- "NoValue"
    if ("svm" == int_svm){
      lesvm <- list(svm = svm_error_sa)
    } else lesvm <- list()

    # randomForest
    int_rf <- intersect("randomForest", model)
    if (length(int_rf) == 0) int_rf <- "NoValue"
    if ("randomForest" == int_rf){
      lerf <- list(randomForest = rf_error_sa)
    } else lerf <- list()

    # naiveBayes
    int_nb <- intersect("naiveBayes", model)
    if (length(int_nb) == 0) int_nb <- "NoValue"
    if ("naiveBayes" == int_nb){
      lenb <- list(naiveBayes = nb_error_sa)
    } else lenb <- list()

    # LMT
    int_rp <- intersect("LMT", model)
    if (length(int_rp) == 0) int_rp <- "NoValue"
    if ("LMT" == int_rp){
      ledt <- list(rpart = rp_error_sa)
    } else ledt <- list()

    # nnet
    int_nt <- intersect("nnet", model)
    if (length(int_nt) == 0) int_nt <- "NoValue"
    if ("nnet" == int_nt){
      lennet <- list(nnet = nt_error_sa)
    } else lennet <- list()

    # knn
    int_knn <- intersect("knn", model)
    if (length(int_knn) == 0) int_knn <- "NoValue"
    if ("knn" == int_knn){
      leknn <- list(knn = kn_error_sa)
    } else leknn <- list()

    resulFinal <- c(lesvm, lerf, lenb, ledt, lennet, leknn)

    return(resulFinal)

  } else if (approach == "LOOCV"){

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calibrating with Leave One Out Cross-Validation (LOOCV) ", TypeEndm , paste0(rep("*",10), collapse = "")))
    }

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
        testing$class <- as.factor(testing$class)

        training <- endm[-j,]
        training$class <- as.factor(training$class)


        models <- svm(class~., data=training, type = "C-classification", ...)
        prediction <- predict(models, testing)
        if(prediction != testing$class){
          svm_ini_error <- svm_ini_error + 1
        }


        model_rf <- randomForest(class~., data=training, importance=TRUE, ...)
        prediction_rf <- predict(model_rf, testing[,-dim(endm)[2]])
        if(prediction != testing$class){
          rf_ini_error <- rf_ini_error + 1
        }


        models <- naiveBayes(class~., data=training, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]])
        if(prediction != testing$class){
          nb_ini_error <- nb_ini_error + 1
        }


        models <- train(class~., method = "LMT", data=training, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
        if(prediction != testing$class){
          rp_ini_error <- rp_ini_error + 1
        }


        nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
        models <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
        if(prediction != testing$class){
          nt_ini_error <- nt_ini_error + 1
        }


        models <- knn3(class~., data = training, k = 5, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "class")
        if(prediction != testing$class){
          kn_ini_error <- kn_ini_error + 1
        }

      }

      svm_error_loocv[i] <- svm_ini_error/dim(endm)[1]
      rf_error_loocv[i] <- rf_ini_error/dim(endm)[1]
      nb_error_loocv[i] <- nb_ini_error/dim(endm)[1]
      rp_error_loocv[i] <- rp_ini_error/dim(endm)[1]
      nt_error_loocv[i] <- nt_ini_error/dim(endm)[1]
      kn_error_loocv[i] <- kn_ini_error/dim(endm)[1]

    }

    # svm
    int_svm <- intersect("svm", model)
    if (length(int_svm) == 0) int_svm <- "NoValue"
    if ("svm" == int_svm){
      lesvm <- list(svm_loocv = svm_error_loocv)
    } else lesvm <- list()

    # randomForest
    int_rf <- intersect("randomForest", model)
    if (length(int_rf) == 0) int_rf <- "NoValue"
    if ("randomForest" == int_rf){
      lerf <- list(randomForest_loocv = rf_error_loocv)
    } else lerf <- list()

    # naiveBayes
    int_nb <- intersect("naiveBayes", model)
    if (length(int_nb) == 0) int_nb <- "NoValue"
    if ("naiveBayes" == int_nb){
      lenb <- list(naiveBayes_loocv = nb_error_loocv)
    } else lenb <- list()

    # LMT
    int_rp <- intersect("LMT", model)
    if (length(int_rp) == 0) int_rp <- "NoValue"
    if ("LMT" == int_rp){
      ledt <- list(rpart_loocv = rp_error_loocv)
    } else ledt <- list()

    # nnet
    int_nt <- intersect("nnet", model)
    if (length(int_nt) == 0) int_nt <- "NoValue"
    if ("nnet" == int_nt){
      lennet <- list(nnet_loocv = nt_error_loocv)
    } else lennet <- list()

    # knn
    int_knn <- intersect("knn", model)
    if (length(int_knn) == 0) int_knn <- "NoValue"
    if ("knn" == int_knn){
      leknn <- list(knn_loocv = kn_error_loocv)
    } else leknn <- list()

    resulFinal <- c(lesvm, lerf, lenb, ledt, lennet, leknn)

    return(resulFinal)

  } else if (approach == "k-fold"){

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calibrating with Cross-Validation (k-fold) ", TypeEndm , paste0(rep("*",10), collapse = "")))
    }

    svm_error_kf <- rep(0,iter)
    rf_error_kf <- rep(0,iter)
    nb_error_kf <- rep(0,iter)
    rp_error_kf <- rep(0,iter)
    nt_error_kf <- rep(0,iter)
    kn_error_kf <- rep(0,iter)

    for (i in 1:iter) {

      svm_ini_error <- 0
      rf_ini_error <- 0
      nb_ini_error <- 0
      rp_ini_error <- 0
      nt_ini_error <- 0
      kn_ini_error <- 0

      groups <- createFolds(1:dim(endm)[1], 10)

      for(g in 1:k){

        # Training and Testing
        muestra <- groups[[g]]
        testing <- endm[muestra,]
        testing$class <- as.factor(testing$class)

        training <- endm[-muestra,]
        training$class <- as.factor(training$class)


        models <- svm(class~., data=training, type = "C-classification", ...)
        prediction <- predict(models, testing)
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        svm_ini_error <- svm_ini_error + error


        models <- randomForest(class~., data=training, importance=TRUE, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]])
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        rf_ini_error <- rf_ini_error + error


        models <- naiveBayes(class~., data=training, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]])
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        nb_ini_error <- nb_ini_error + error


        models <- train(class~., method = "LMT", data=training, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        rp_ini_error <- rp_ini_error + error


        nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
        models <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        nt_ini_error <- nt_ini_error + error


        models <- knn3(class~., data = training, k = 5, ...)
        prediction <- predict(models, testing[,-dim(endm)[2]], type = "class")
        # Confusion Matrix
        MC <- table(prediction, testing[,dim(endm)[2]])
        # Overall accuracy
        oa <- sum(diag(MC))/sum(MC)
        error <- 1 - oa
        kn_ini_error <- kn_ini_error + error

      }

      svm_error_kf[i] <- svm_ini_error/k
      rf_error_kf[i] <- rf_ini_error/k
      nb_error_kf[i] <- nb_ini_error/k
      rp_error_kf[i] <- rp_ini_error/k
      nt_error_kf[i] <- nt_ini_error/k
      kn_error_kf[i] <- kn_ini_error/k

    }

    # svm
    int_svm <- intersect("svm", model)
    if (length(int_svm) == 0) int_svm <- "NoValue"
    if ("svm" == int_svm){
      lesvm <- list(svm_kfold = svm_error_kf)
    } else lesvm <- list()

    # randomForest
    int_rf <- intersect("randomForest", model)
    if (length(int_rf) == 0) int_rf <- "NoValue"
    if ("randomForest" == int_rf){
      lerf <- list(randomForest_kfold = rf_error_kf)
    } else lerf <- list()

    # naiveBayes
    int_nb <- intersect("naiveBayes", model)
    if (length(int_nb) == 0) int_nb <- "NoValue"
    if ("naiveBayes" == int_nb){
      lenb <- list(naiveBayes_kfold = nb_error_kf)
    } else lenb <- list()

    # LMT
    int_rp <- intersect("LMT", model)
    if (length(int_rp) == 0) int_rp <- "NoValue"
    if ("LMT" == int_rp){
      ledt <- list(rpart_kfold = rp_error_kf)
    } else ledt <- list()

    # nnet
    int_nt <- intersect("nnet", model)
    if (length(int_nt) == 0) int_nt <- "NoValue"
    if ("nnet" == int_nt){
      lennet <- list(nnet_kfold = nt_error_kf)
    } else lennet <- list()

    # knn
    int_knn <- intersect("knn", model)
    if (length(int_knn) == 0) int_knn <- "NoValue"
    if ("knn" == int_knn){
      leknn <- list(knn_kfold = kn_error_kf)
    } else leknn <- list()

    resulFinal <- c(lesvm, lerf, lenb, ledt, lennet, leknn)

    return(resulFinal)

  } else if (approach == "MCCV"){

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calibrating with Monte Carlo Cross-Validation (MCCV) ", TypeEndm , paste0(rep("*",10), collapse = "")))
    }

    svm_error_mccv <- rep(0,iter)
    rf_error_mccv <- rep(0,iter)
    nb_error_mccv <- rep(0,iter)
    rp_error_mccv <- rep(0,iter)
    nt_error_mccv <- rep(0,iter)
    kn_error_mccv <- rep(0,iter)

    n <- dim(endm)[1]
    groups_mc <- generate.split(niter = iter, n = n, ntest = 100 - training_split)

    for (i in 1:iter) {

      # Training and Testing
      muestra <- groups_mc[i,]
      testing <- endm[muestra,]
      testing$class <- as.factor(testing$class)

      training <- endm[-muestra,]
      training$class <- as.factor(training$class)


      models <- svm(class~., data=training, type = "C-classification", ...)
      prediction <- predict(models, testing)
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      svm_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)


      models <- randomForest(class~., data=training, importance=TRUE, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]])
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      rf_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)


      models <- naiveBayes(class~., data=training, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]])
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      nb_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)


      models <- train(class~., method = "LMT", data=training, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      rp_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)


      nnet.grid = expand.grid(size = c(10, 50), decay = c(5e-4, 0.2))
      models <- train(class~., data = training, method = "nnet", tuneGrid = nnet.grid, trace = FALSE, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "raw")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      nt_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)


      models <- knn3(class~., data = training, k = 5, ...)
      prediction <- predict(models, testing[,-dim(endm)[2]], type = "class")
      # Confusion Matrix
      MC <- table(prediction, testing[,dim(endm)[2]])
      # error
      kn_error_mccv[i] <- 1 - sum(diag(MC))/sum(MC)

    }

    # svm
    int_svm <- intersect("svm", model)
    if (length(int_svm) == 0) int_svm <- "NoValue"
    if ("svm" == int_svm){
      lesvm <- list(svm_mccv = svm_error_mccv)
    } else lesvm <- list()

    # randomForest
    int_rf <- intersect("randomForest", model)
    if (length(int_rf) == 0) int_rf <- "NoValue"
    if ("randomForest" == int_rf){
      lerf <- list(randomForest_mccv = rf_error_mccv)
    } else lerf <- list()

    # naiveBayes
    int_nb <- intersect("naiveBayes", model)
    if (length(int_nb) == 0) int_nb <- "NoValue"
    if ("naiveBayes" == int_nb){
      lenb <- list(naiveBayes_mccv = nb_error_mccv)
    } else lenb <- list()

    # LMT
    int_rp <- intersect("LMT", model)
    if (length(int_rp) == 0) int_rp <- "NoValue"
    if ("LMT" == int_rp){
      ledt <- list(rpart_mccv = rp_error_mccv)
    } else ledt <- list()

    # nnet
    int_nt <- intersect("nnet", model)
    if (length(int_nt) == 0) int_nt <- "NoValue"
    if ("nnet" == int_nt){
      lennet <- list(nnet_mccv = nt_error_mccv)
    } else lennet <- list()

    # knn
    int_knn <- intersect("knn", model)
    if (length(int_knn) == 0) int_knn <- "NoValue"
    if ("knn" == int_knn){
      leknn <- list(knn_mccv = kn_error_mccv)
    } else leknn <- list()

    resulFinal <- c(lesvm, lerf, lenb, ledt, lennet, leknn)

    return(resulFinal)

  } else {

    stop("Unsupported calibration approach.", call. = TRUE)
  }

}
