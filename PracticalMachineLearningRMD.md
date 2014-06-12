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


Reading (& caching) Files.


```r
training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")
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


Seeing the correlation amongst the numeric variables and Conducting PCA on them.


```r
m <- abs(cor(nzv_training[, numericvarsid]))
diag(m) <- 0
which(m > 0.75, arr.ind = T)
```

```
##                      row col
## magnet_dumbbell_z     80   2
## yaw_belt               7   5
## total_accel_belt       8   5
## accel_belt_y          29   5
## accel_belt_z          30   5
## accel_arm_y           43   5
## accel_belt_x          28   6
## magnet_belt_x         31   6
## roll_belt              5   7
## total_accel_belt       8   7
## accel_belt_z          30   7
## roll_belt              5   8
## yaw_belt               7   8
## accel_belt_y          29   8
## accel_belt_z          30   8
## accel_arm_y           43   8
## magnet_dumbbell_y     79  25
## pitch_belt             6  28
## magnet_belt_x         31  28
## roll_belt              5  29
## total_accel_belt       8  29
## accel_belt_z          30  29
## roll_belt              5  30
## yaw_belt               7  30
## total_accel_belt       8  30
## accel_belt_y          29  30
## accel_arm_y           43  30
## pitch_belt             6  31
## accel_belt_x          28  31
## magnet_belt_z         33  32
## magnet_belt_y         32  33
## gyros_arm_y           40  39
## gyros_arm_x           39  40
## magnet_arm_x          45  42
## roll_belt              5  43
## total_accel_belt       8  43
## accel_belt_z          30  43
## magnet_arm_z          47  44
## accel_arm_x           42  45
## magnet_arm_y          46  45
## magnet_arm_x          45  46
## magnet_arm_z          47  46
## accel_arm_z           44  47
## magnet_arm_y          46  47
## accel_dumbbell_x      75  53
## accel_dumbbell_z      77  54
## accel_dumbbell_y      76  61
## gyros_dumbbell_z      74  72
## gyros_forearm_z       91  72
## gyros_dumbbell_x      72  74
## gyros_forearm_z       91  74
## pitch_dumbbell        53  75
## total_accel_dumbbell  61  76
## yaw_dumbbell          54  77
## magnet_dumbbell_y     79  78
## gyros_belt_x          25  79
## magnet_dumbbell_x     78  79
## raw_timestamp_part_1   2  80
## gyros_forearm_z       91  90
## gyros_dumbbell_x      72  91
## gyros_dumbbell_z      74  91
## gyros_forearm_y       90  91
## magnet_forearm_y      96  93
## accel_forearm_y       93  96
```

```r

numPCATrain <- preProcess(nzv_training[, numericvarsid], method = "pca", thresh = 0.8, 
    na.remove = TRUE)
numPCATrain
```

```
## 
## Call:
## preProcess.default(x = nzv_training[, numericvarsid], method =
##  "pca", thresh = 0.8, na.remove = TRUE)
## 
## Created from 406 samples and 97 variables
## Pre-processing: principal component signal extraction, scaled, centered 
## 
## PCA needed 17 components to capture 80 percent of the variance
```


