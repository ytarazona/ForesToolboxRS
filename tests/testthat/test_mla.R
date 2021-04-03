library(testthat)
library(ForesToolboxRS)
library(raster)
library(e1071)
library(sf)
data(endm)
data(img_l8)

context("ForesToolboxRS mla")

vegt <- extract(img_l8, endm)
df <- as.data.frame(endm)
names(df[1]) <- c("class")
df <- data.frame(vegt, class = df[,1])
df$class <- as.factor(df$class)

Muestra <- sample(1:60, 12)
testing <- df[Muestra,]
training <- df[-Muestra,]

model <- svm(class~., data = training, type = "C-classification")
prediccion <- predict(model, testing)
MC <- table(testing[,7], prediccion)
Acierto <- sum(diag(MC))/sum(MC)

test_that("try mla", {
  classMap <- mla(img = img_l8, endm = endm, model = "svm", training_split = 80)
  expect_equal(as.vector(classMap$Overall_accuracy[1]), Acierto,
               tolerance=1)
})
