---
  title: "Final Data Analysis"
output:
  pdf_document: default
html_document: default
date: "2022-12-09"
---
  ##1 Preliminary exploration and Data cleaning
  First we read in the data and look at the summaries of the variables.
```{r}
obsidian = read.table(file = "/Users/minseoksong/Downloads/obsidian_data.txt", header=TRUE, sep= ",")
obsidian
summary(obsidian)
```
We first notice that there seems to be some missing value. Let us look into that.
```{r}
missing_ind = which(apply(is.na(obsidian),1,any) == TRUE)
missing_ind
```
Data points in row 171 and 178 have missing values. For the time being, instead of imputing these missing values, we omit these in order to increase the accuracy of the regression. 
```{r}
obsidian = obsidian[-missing_ind,]
```
This is becasue we have enough number of the data sample, which is
```{r}
dim(obsidian)[1]
```
Second thing we notice is abnormally high value of maximum of mass. We suspect that this data is corrupted.
```{r}
obsidian[which(obsidian$mass>30),]
```
Indeed, comparing with summary, we notice that the amount of four elements does not seem to deviate too much from other data. By a common sense, this indeed looks corrupted, so we delete this data point as well.
```{r}
obsidian = obsidian[-which(obsidian$mass>30),]
```
Let us now check the histogram of continuous covariates.
```{r}
par(mfrow=c(1,4))
hist(obsidian$element_Rb)
hist(obsidian$element_Sr)
hist(obsidian$element_Y)
hist(obsidian$element_Zr)
```
There are some data points that deviate from other data, but the deviation is not huge and so we do not suspect that they are corrupted. We do not observe any noticeable patterns either.

If we look through obsidian data, which we omit due to space concern, there are data that signifies uncertainty for type covariate. The easypattern we observe is that the length of those is greater than 6, so we use "which" function..
```{r}
uncertaintypes = which(nchar(obsidian$type)>6)
length(uncertaintypes)
```
The number of these data points are not too large given the dimension of the data set around 600, so for the accuracy of our regression, we delete these as well. One potential downside is that this can cause some bias, so we do need to keep this in mind.

```{r}
obsidian = obsidian[-uncertaintypes, ]
```

We also want to use consistent capital/small letter and singular/plural noun throughout the data in order to treat these as same when we do the regression.
```{r}
for (i in 1:dim(obsidian)[1]){
  i = as.double(i)
  if (obsidian$type[i] == "Blades" || obsidian$type[i] == "blade"){
    obsidian$type[i] = "Blade"
  }
  else if (obsidian$type[i] == "Flakes" || obsidian$type[i] == "flake"){
    obsidian$type[i] = "Flake"
  }
  else if (obsidian$type[i] == "core"){
    obsidian$type[i] = "Core"
  }
}
```
By looking through the site covariate, we also notice that there is some uncertainty/abnormality in data point 215 (Ali Kosh/Chaga Sefid) and data point 229 (Hulailan Tepe Guran). We need to be careful with the indices when we delete data points because some row numbers are already deleted. We delete these data points for the accuracy/simplicity of our regression. Again, the counterpoiknt is that this can introduce bias.
```{r}
obsidian = obsidian[-which(nchar(obsidian$site)>13),]
```
We are not ready to do the regression.

```{r}
obsidian[446,]
```

##Model Fitting
Let us first divide the data into a training set and validation set. We use training set to do the model selection and use validation set to avoid multiple testing issues and selective inference.
```{r}
set.seed(123)
n=dim(obsidian)[1]
ntrain = 410; nval = 203
validation = sample(n, nval, replace=FALSE)
train = setdiff(1:n, validation)
```
First, we decide not to include ID as a covariate in our regression. Other than the obvious reason that each data point has a different ID, we do not see any visible pattern. For example, if we restrict to the first three digits, the number will be exactly collinear with the site covariate. If we were to expand it to more than first three digits, again model gets too large, hence increasing variance too much.
```{r}
obsidian$type = as.factor(obsidian$type)
obsidian$site = as.factor(obsidian$site)
lmod1 <- lm(mass ~ type + site + element_Rb + element_Sr + element_Y + element_Zr, obsidian[train,])
summary(lmod1)
```
typeCore and element_Rb covariates are highly significant, so we keep any categorical covariate that includes these. typeFlake is mildly  significant. Remember that intercept term implicitly considers Ali Kosh as a reference level, so even though t value of "siteChangha Sefid" covariate is high, we do not want to delete it. However, we do want to delete "element_Sr," "element_Y," and "element_Zr" since their t values are high and the size of estimated values are not so large anyway.
```{r}
lmod2 <- lm(formula = mass ~ type + site + element_Rb, data = obsidian[train, ])
summary(lmod2)
```
It is natural to ask if there is an interaction term between site and type.
```{r}
lmod3 <- lm(formula = mass ~ (type + site + element_Rb)**2, data = obsidian[train, ])
anova(lmod3)
```
We do observe that we might need to consider both type:site and site:element_Rb.
```{r}
lmod4 <- lm(formula = mass ~ type + site + element_Rb + type:site + site:element_Rb, data = obsidian[train, ])
summary(lmod4)
```
While typeCore:siteChagha Sefid is highly significant, there is caveat here. Namely, there are only a small number of datapoints corresponding to the type core in the training dataset.
```{r}
length(which(obsidian$type[train]=="Core"))
```
Low t value means that these ten data points are mostly Chagha Sefid, but it may be just by random chance. Let us keep this in mind.
Let us check for leverage points.
```{r}
X = model.matrix(lmod4)
leverage = diag(X%*%solve(t(X)%*%X,t(X)))
plot(lmod4$fit,lmod4$resid,cex=10*leverage)
```
We do need to be aware of the three biggest leverage points on the right, since they actually have high size of residuals simultaneously; they are influential and greatly affect the slope of the regression line.
```{r}
lst <- sort(leverage, index.return=TRUE, decreasing=TRUE)
lapply(lst, `[`, lst$x %in% head(unique(lst$x),3))
```

Those three data points are...
```{r}
obsidian[c(87,137,106),]
```

Let us try regressing without these data points.

```{r}
deleteinf = obsidian[train[-c(87, 137, 106)],]
lmod5 <- lm(formula = mass ~ type + site + element_Rb + type:site + site:element_Rb, data = deleteinf)
summary(lmod5)
```
It is a judgement of the domain expert whether we include these three influential points.
Note that there is NA for typeCore:siteChagha Sefid because they are exactly collinear. Since typeFlake:siteChagha Sefid, which may be more meaningful due to their large corresponding sample size, does not have a significance level, we wonder if we have to delete type:site.

```{r}
lmod6 <- lm(formula = mass ~ type + site + element_Rb + site:element_Rb, data = obsidian[train,])
summary(lmod6)
```

```{r}
anova(lmod4, lmod6)
```
As F test exhibits above, we do not delete type:site.

Given that we do not delete three influential points, let us do the diagnosis for lmod4.

```{r}
par(mfrow=c(2,2))
plot(lmod4)
```
```{r}

```

