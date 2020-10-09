<img src="docs/figures/logo.png" align="right" width = 15%/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/csaybar/ForesToolboxRS/branch/master/graph/badge.svg)](https://codecov.io/gh/csaybar/ForesToolboxRS?branch=dev)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://paypal.me/APROGIS?locale.x=es_XC)

# ForesToolboxRS

**ForesToolboxRS** is an initiative that is inspired by the work of [Tarazona, Y., Mantas, V.M., Pereira, A.J.S.C. (2018). Improving tropical deforestation detection through using photosynthetic vegetation time series (PVts-Beta). Ecological Indicators, 94, 367-379.](https://doi.org/10.1016/j.ecolind.2018.07.012).

<img src="docs/figures/Readme-image.png">

# Funding

The development of this package was funded by [American Program in GIS and Remote Sensing (APROGIS)](https://www.apgis-rs.com/). ARROGIS was established in 2018 as a leading scientific institution and pioneer in the field of Remote Sensing and Geographic Information Systems (GIS). APROGIS promotes the use of state-of-the-art space technology and earth observation for the sustainable development of states. It is an institution capable of generating new knowledge through publications in different journals in the field of Remote Sensing. More about APROGIS [here](https://www.apgis-rs.com/acerca-de-nosotros/mision-y-vision).

# Introduction

**ForesToolboxRS** is a R package that was created to provide a variety of tools and algorithms for the processing and analysis of satallite images for the different applications of Remote Sensing for Earth Observation. All implemented algorithms are based on scientific publications. **The PVts-Beta approach**, a non-seasonal detection approach, is implemented in this package and is capable of reading time series, vector, matrix and raster data. The various functions of this package are intended to show, on the one hand, some progress in methods for mapping deforestation, forest degradation, and on the other hand, to provide some tools (not yet available) for routine analysis of remotely detected data. Tools for calibration of unsupervised and supervised algorithms through various calibration approaches are some of the functions embedded in this package. Much of the authors' experience in the field of Remote Sensing is collected by this package, therefore we sincerely hope that **ForesToolboxRS** can facilitate different analyses and simple and robust processes in satellite images.

Availables functions:

| Name of functions   | Description                                               | 
| --------------      | ----------------                                          | 
| **`pvts`**                | This algorithm will allow to detect disturbances in the forests using all the available Landsat set. In fact, it can also be run with sensors such as MODIS.
| **`pvtsRaster`**          | This algorithm will allow to detect disturbances in the forests using all the available Landsat set. In fact, it can also be run with sensors such as MODIS.     |
| **`smootH`**              | In order to eliminate outliers in the time series, a  temporary smoothing is used.                  |
| **`mla`**                 | This developed function allows to execute supervised and unsupervised classification in satellite images through various algorithms.                        |
| **`calmla`**              | This function allows to calibrate supervised classification in satellite images through various algorithms and using approches such as Set-Approach, Leave-One-Out Cross-Validation (LOOCV), Cross-Validation (k-fold) and Monte Carlo Cross-Validation (MCCV). |
| **`calkmeans`**           | This function allows to calibrate the kmeans algorithm. It is possible to obtain the best k value and the best embedded algorithm in kmeans.      |
| **`coverChange`**         | This algorithm is able to obtain gain and loss in land cover classification.                                      |
| **`linearTrend`**         | Linear trend is useful for mapping forest degradation, land degradation, among others. This algorithm is capable of obtaining the slope of an ordinary least-squares linear regression and its reliability (p-value). |
| **`fusionRS`**           | This algorithm allows to fusion images coming from different spectral sensors (e.g., optical-optical, optical and SAR or SAR-SAR). Among many of the qualities of this function, it is possible to obtain the contribution (%) of each variable in the fused image. |
| **`sma`**                 | The SMA assumes that the energy received, within the field of vision of the remote sensor, can be considered as the sum of the energies received from each dominant endmember. This function addresses a Linear Mixing Model.  |
| **`ndfiSMA`**             | The NDFI it is sensitive to the state of the canopy cover, and has been successfully applied to monitor forest degradation and deforestation in Peru and Brazil. This index comes from the endmembers Green Vegetation (GV), non-photosynthetic vegetation (NPV), Soil (S) and the reminder is the shade component.          |
| **`tct`**                | The Tasseled-Cap Transformation is a linear transformation method for various remote sensing data. Not only can it perform volume data compression, but it can also provide parametersassociated with the physical characteristics, such as brightness, greenness and wetness indices.                                           |
| **`gevi`**                | Greenness Vegetation Index is obtained from the Tasseled Cap Transformation.                                        |
| **`MosaicFreeCloud`**     | A three-step algorithm for creating a mosaic from satellite imagery (see notes).                             |
                        
# Installation

To install the latest development version directly from the GitHub repository. Before running **ForesToolboxRS**, it is necessary to intall the **devtools** package. Then, let's do this:

```R
library(devtools)
install_github("ytarazona/ForesToolboxRS")
```

# Examples

## 1. Breakpoint in an NDFI series

Here an Normalized Difference Fraction Index (NDFI) between 2000 and 2019, one NDFI for each year. The NDFI value ranges from -1 to 1.

```R
library(ForesToolboxRS)

# NDFI series
ndfi <- c(0.86,0.93,0.97,0.91,0.95,0.96,0.91,0.88,0.92,0.89,0.90,0.89,0.91,
          0.92,0.89,0.90,0.92,0.84,0.46,0.20, 0.27,0.22,0.52,0.63,0.61,0.67,0.64,0.86)
          
# Plot
plot (ndfi, pch = 20, xlab = "Index", ylab = "NDFI value")
lines (ndfi, col = "gray45")
```

### 1.1 Applying a smoothing

Before detecting a breakpoint, it is necessary to apply a smoothing to remove outliers. So, we'll use the **smootH** function from the **ForesToolboxRS** package. This function accepts vector or matrix. 

```R
library(ForesToolboxRS)

smth <- smootH(x)
plot(x, type="o", ylab="Reflectance %", xlab="Time")
lines(smth, col="blue", type="o")
```

To detect changes, either we can have a vector (using a specific index) as input or a time series. Let's first detect changes with a vector, a then with a time series. 

### 1.1 Breakpoint using a specific Index (vector)

```R
# Detect changes in 2008 (position 19)
cd <- pvts(x = ndfi, startm = 19, endm = 19, threshold = 5)
plot(cd)
```
The output:

<img src="docs/figures/breakpoint_index.jpg" width = 70%/>

### Breakpoint using Time Series

```R
# Detect changes in 2008 (time serie)
ndfi_ts <- ts(ndfi, start = 1990, end = 2017, frequency = 1)

cd <- pvts(x = ndfi_ts, startm = 2008, endm = 2008,  threshold = 5)

plot(cd)
```
The output:

<img src="docs/figures/breakpoint_ts.jpg" width = 70%/>

### Classification in Remote Sensing

For this tutorial, Landsat 8 OLI was used. To download the image [Here](https://drive.google.com/file/d/1Xf1A84fJN20eC578GwwOsVSuoxLuCRGJ/view?usp=sharing). 

```R
library(ForesToolboxRS)
library(raster)
load("FTdata.RData")

image_path <- ".../your/path/LC08_232066_20190727.tif"
image <- stack(img_path)

class_svm <- mla(img = image, endm = endme, training_split=80, testing_split=20)

plot(cd)
```



