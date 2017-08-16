## Correlation matrix in R using Hmisc package

# setwd("~/R")

install.packages("Hmisc")
library(Hmisc)


dat <- read.table("CoEx_soilCorr.txt", header = TRUE)
head(dat)
# here's what my input data looked like:
# first 5 variables are the data i aquired and I wanted to see if any
# correlation with soil physiochem properties.

# DNA  RNA X280_260 X260_230 Protein Clay Silt Sand  pH   SOM    TC  TN
# Soil2   7.99 4.27     1.69     0.82   96.63 28.1 45.3 26.6 5.2 201.0 100.0 7.8
# Soil8  13.80 3.56     1.58     0.79   54.68 11.1 20.6 68.2 5.1  93.5  43.6 4.3
# Soil14 11.37 2.85     1.74     1.11   96.00 30.7 37.4 31.9 6.2 136.5  57.2 5.1
# Soil16 28.60 7.07     1.68     0.98   82.36 16.7 38.7 44.6 6.0  83.0  40.5 3.9
# Soil21  5.27 2.56     1.87     1.32   67.01  9.5 15.3 75.2 5.2  61.0  24.5 2.4
# Soil28 14.17 3.47     1.79     1.23   51.50 19.4 33.5 47.2 6.6  94.5  40.4 3.9


rcorr(dat, type = c("pearson"))
# can choose spearman instead. 


res2 <- rcorr(as.matrix(dat))

res2$r # look at correlations
res2$P # look at p vals. 


## use function flattenCorrMatrix in order to write out the restuls

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


res3 <- flattenCorrMatrix(res2$r, res2$P)

# write out to csv

write.csv(res3, "Co-ex_CorrOutput.csv")

## taken from this tutorial:

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

