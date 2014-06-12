Assessment R Markdown
========================================================

## This is a R markdown for the Assessment (Peer Reviewed) for the Practical Machine Learning Course from the John Hopkinson University

### **Author** - Mohit Sewak - *mohitsewak@gmail.com*

#### Built on R version 2.15.2, using R Studio Version 0.98.507, Posted on GitHub Repository [mohitsewak/RMachineLearning] (http://mohitsewak.github.io/RMachineLearinng/) using gw-pages.


Loading required libraries (Results Hidden, Warnings OFF, Messages OFF)


```r
library(lattice)
library(ggplot2)
library(graphics)
library(Hmisc)
library(e1071)
library(caret)
```


Reading (& caching) Files, and making training and verification data sets.


```r
trainingCSV = read.csv("pml-training.csv")
testingCSV = read.csv("pml-testing.csv")
inTrain <- createDataPartition(trainingCSV$classe, p = 0.75, list = FALSE)
training <- trainingCSV[inTrain, ]
verification <- trainingCSV[-inTrain, ]
```



Creating summary and viewing the top few records, and seeing paired scatter plots in the training file (Results Hidden, Output Cached).



```r
summary(training)
head(training)
```


Trying to explore the data, esp the relations between variables. 

<p></p>

<p> Since the number of features are huge, so most of the comprehensive plot based visualizations will break.</p>

<p></p>

<p>There we may try to see multiple subsetted plot (as some examples given), or better still, see the tabular correlation matrix of the ones which are highly correlated. We will do these later as for this we will have to segregate the numeric variables and conduct this analysis on those variables alone.</p>


```r
plot(training)
```

```
## Error: figure margins too large
```

```r
pairs(training[1:10000, 1:10])
```

![plot of chunk pair_plots](figure/pair_plots1.png) 

```r
pairs(training[1:10000, 11:20])
```

![plot of chunk pair_plots](figure/pair_plots2.png) 

```r
pairs(training[1:10000, 21:30])
```

![plot of chunk pair_plots](figure/pair_plots3.png) 


Since most of the columns have no data, or predictive power, it might not be conducive to use them as-is.
<p> Conducting the following prepocessing operations:</p>
- Removing Near Zero Value Predictors
- Principal Component Analysis (PCA) orthogonal rotation to optimize/ reduce dimensions in the data set 
- (Nt: Variable 160 = target variable named "classe"")


```r
nzv <- nearZeroVar(training[, -160])
dim(training)
```

```
## [1] 19622   160
```

```r
nzv_training <- training[, -nzv]
dim(nzv_training)
```

```
## [1] 19622   100
```


Finding numric variables, as PCA can be applied only on numeric variables


```r
numericvars <- NULL
numericvarsid <- NULL

non_numericvars <- NULL
non_numericvarsid <- NULL

id <- 0L

for (Var in names(nzv_training)) {
    
    id <- id + 1
    
    if (class(nzv_training[, Var]) == "integer" | class(nzv_training[, Var]) == 
        "numeric") {
        numericvars <- c(numericvars, Var)
        numericvarsid <- c(numericvarsid, id)
    } else {
        non_numericvars <- c(non_numericvars, Var)
        non_numericvarsid <- c(non_numericvarsid, id)
    }
}

summary(numericvars)
```

```
##    Length     Class      Mode 
##        97 character character
```

```r
numericvarsid
```

```
##  [1]  1  3  4  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## [24] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
## [47] 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71
## [70] 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94
## [93] 95 96 97 98 99
```


Seeing the correlation amongst the numeric variables, with less than 60% null values and Conducting PCA on them.


```r
numericTraining <- nzv_training[, numericvarsid]
numericNANTraining <- numericTraining[, colSums(is.na(numericTraining)) >= 0.4 * 
    nrow(numericTraining)]
m <- abs(cor(numericNANTraining))
diag(m) <- 0
which(m > 0.75, arr.ind = T)
```

```
##      row col
```

```r

numPCATrain <- preProcess(numericNANTraining, method = "pca", thresh = 0.8)
numPCATrain
```

```
## 
## Call:
## preProcess.default(x = numericNANTraining, method = "pca", thresh = 0.8)
## 
## Created from 406 samples and 41 variables
## Pre-processing: principal component signal extraction, scaled, centered 
## 
## PCA needed 8 components to capture 80 percent of the variance
```

```r

numericTrainingData <- predict(numPCATrain, numericNANTraining)
summary(numericTrainingData)
```

```
##       PC1             PC2             PC3             PC4       
##  Min.   :-9      Min.   :-4      Min.   :-8      Min.   :-6     
##  1st Qu.:-2      1st Qu.:-2      1st Qu.:-2      1st Qu.:-2     
##  Median : 0      Median : 0      Median : 0      Median : 0     
##  Mean   : 0      Mean   : 0      Mean   : 0      Mean   : 0     
##  3rd Qu.: 2      3rd Qu.: 1      3rd Qu.: 2      3rd Qu.: 1     
##  Max.   : 5      Max.   : 9      Max.   : 4      Max.   : 8     
##  NA's   :19216   NA's   :19216   NA's   :19216   NA's   :19216  
##       PC5             PC6             PC7             PC8       
##  Min.   :-2      Min.   :-4      Min.   :-4      Min.   :-4     
##  1st Qu.: 0      1st Qu.:-1      1st Qu.:-1      1st Qu.:-1     
##  Median : 0      Median : 0      Median : 0      Median : 0     
##  Mean   : 0      Mean   : 0      Mean   : 0      Mean   : 0     
##  3rd Qu.: 0      3rd Qu.: 1      3rd Qu.: 1      3rd Qu.: 1     
##  Max.   :28      Max.   : 5      Max.   : 4      Max.   : 3     
##  NA's   :19216   NA's   :19216   NA's   :19216   NA's   :19216
```


