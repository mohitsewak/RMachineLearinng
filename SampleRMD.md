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


Since the data size, and number of feature vectors are huge, so reducing both the data size for plots and also the number of columns in a set of paired visualization


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
- Principal Component Axis (PCA) rotation to optimize/ reduce dimensions in the data set 
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

```r
preProcess(nzv_training[, -160], method = "pca", thresh = 0.8)
```

```
## Error: all columns of x must be numeric
```





