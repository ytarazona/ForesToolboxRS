library(ForesToolboxRS)
library(raster)
data(img_optical)
data(img_radar)

context("ForesToolboxRS fusionRS")

# Variance test
img <- raster::stack(img_optical, img_radar)
df <- as.data.frame(img)
df <- na.omit(df)
acp <- prcomp(df, center = TRUE, scale = TRUE)
var <- summary(acp)$importance[1, ]^2 # Variance

test_that("try fusionRS", {
  fusion <- fusionRS(x = img_optical, y = img_radar, na.rm = TRUE)
  expect_equal(as.vector(fusion$Variance), as.vector(var))
})

# Variance Proportion test
pov <- summary(acp)$importance[2, ] # Proportion of variance

test_that("try fusionRS", {
  fusion <- fusionRS(x = img_optical, y = img_radar, na.rm = TRUE)
  expect_equal(as.vector(fusion$Proportion_of_variance), as.vector(pov))
})

# Cumulative variance test
varAcum <- summary(acp)$importance[3, ] # Cumulative variance

test_that("try fusionRS", {
  fusion <- fusionRS(x = img_optical, y = img_radar, na.rm = TRUE)
  expect_equal(as.vector(fusion$Cumulative_variance), as.vector(varAcum))
})

# Correlation test
corr <- get_pca_var(acp)$cor # Correlation

test_that("try fusionRS", {
  fusion <- fusionRS(x = img_optical, y = img_radar, na.rm = TRUE)
  expect_equal(as.vector(fusion$Correlation), as.vector(corr))
})

# Contribution test
contri <- get_pca_var(acp)$contrib # Contribution in %

test_that("try fusionRS", {
  fusion <- fusionRS(x = img_optical, y = img_radar, na.rm = TRUE)
  expect_equal(as.vector(fusion$Contribution_in_pct), as.vector(contri))
})
