
# ForesToolboxRS 0.0.1
=====================================

Initial release with the following functions:

* **pvts** This algorithm will allow to detect disturbances in the forests using all the available Landsat set. In fact, it can also be run with sensors such as MODIS.
* **pvtsRaster** This algorithm will allow to detect disturbances in the forests using all the available Landsat set. In fact, it can also be run with sensors such as MODIS.
* **smootH** In order to eliminate outliers in the time series, a  temporary smoothing is used.
* **mla** This developed function allows to execute supervised and unsupervised classification in satellite images through various algorithms.
* **calmla** This function allows to calibrate supervised classification in satellite images through various algorithms and using approches such as Set-Approach, Leave-One-Out Cross-Validation (LOOCV), Cross-Validation (k-fold) and Monte Carlo Cross-Validation (MCCV).
* **calkmeans** This function allows to calibrate the kmeans algorithm. It is possible to obtain the best k value and the best embedded algorithm in kmeans.
* **coverChange** This algorithm is able to obtain gain and loss in land cover classification.
* **linearTrend** Linear trend is useful for mapping forest degradation, land degradation, among others. This algorithm is capable of obtaining the slope of an ordinary least-squares linear regression and its reliability (p-value).
* **fusionRS** This algorithm allows to fusion images coming from different spectral sensors (e.g., optical-optical, optical and SAR or SAR-SAR). Among many of the qualities of this function, it is possible to obtain the contribution (%) of each variable in the fused image.
* **sma** The SMA assumes that the energy received, within the field of vision of the remote sensor, can be considered as the sum of the energies received from each dominant endmember. This function addresses a Linear Mixing Model.
* **ndfiSMA** The NDFI it is sensitive to the state of the canopy cover, and has been successfully applied to monitor forest degradation and deforestation in Peru and Brazil. This index comes from the endmembers Green Vegetation (GV), non-photosynthetic vegetation (NPV), Soil (S) and the reminder is the shade component.
* **tct** The Tasseled-Cap Transformation is a linear transformation method for various remote sensing data. Not only can it perform volume data compression, but it can also provide parametersassociated with the physical characteristics, such as brightness, greenness and wetness indices.
* **gevi** Greenness Vegetation Index is obtained from the Tasseled Cap Transformation.

 
Included example data sets: 
* data(FTdata)


