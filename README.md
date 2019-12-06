# cuyem

The culture of the Nez Perce Tribe and it's people, the Nimiip\'uu, revolve around fish and water.  As such, its only fitting to name an R package, centered solely on monitoring and evaluating anadromous fish population status, after the Nimiip\'uu word for fish: __cuyem__. The __cuyem__ package serves as a tool for the Nez Perce Tribe's Department of Fisheries Resources Management's Research division staff to calculate population indicators and metrics using standard methods across multiple species and populations.  __cuyem__ and the embedded functions can then be shared with co-managers, regulatory and funding agencies to achieve full reproducibility and transparency of popultion estimates and methodology.

The __cuyem__ package has multiple companion manuscripts that document Nez Perce Tribe field data collection methods, data management procedures and calculation methods for population indicators and metrics. Indicator and metric calculation methods used within each __cuyem__ function is documented and further explained in the Research division's "Standardized Performance Measures" document (Kinzer et al. 2017). All documents pertaining to the Research Division's field and analysis methods can be found on the Nez Perce Tribe Department of Fisheries Resources Management website; \url{www.nptfisheries.org}. 

The __cuyem__ package is in a development phase and currently under going significant changes. Included functions are being improved and others are being added weekly. Because the package is changing constantly we recommend downloading a newer version into the local R library before each use.  We welcome package suggestions, critiques or contributions by contacting Ryan Kinzer by email, \email{ryank@nezperce.org}.

## Load Package

The newest version of __cuyem__ is stored in a GitHub repository and freely accessible. To obtain the package first install Hadley Wickam's __devtools__ from R CRAN. Then, the function *install_github()* will retrieve the latest version of __cuyem__ from the GitHub repository, \url{https://github.com/ryankinzer/cuyem}, and store it in a local R library.

```
install.packages("devtools")
devtools::install_github("ryankinzer/cuyem")
```
After installing __cuyem__ it is necessary to load the package into the current R session using *library('cuyem')*. 

```
library('cuyem')
```
