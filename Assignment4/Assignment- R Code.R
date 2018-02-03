####PROBLEM1

### Part 1

install.packages("freqparcoord")
install.packages("regtools")
library (freqparcoord)
library(regtools)
data(mlb)

xvalpart <- function(data, p) { 
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain ,replace = FALSE)
  list ( train = data[trainidxs,], valid = data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE) { 
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  # fit model to training data
  trainy <- train [,ycol]
  trainpreds <- train [,predvars]
  # using matrix form in lm() call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm( trainy ~ trainpreds )
  # apply fitted model to validation data; note
  # that %â*% works only on matrices , not data frames
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid [,ycol]
  if (meanabs)
    return(mean(abs(predy - realy )))
  list (predy = predy , realy = realy )
}

xvalknn <-
  function(data,ycol , predvars, k, p, meanabs=TRUE){
    # cull out just Y and the Xs
    data <- data[, c(predvars, ycol)]
    tmp <- xvalpart(data,p)
    train <- tmp$train
    valid <- tmp$valid
    valid <- as.matrix(valid)
    xd <- preprocessx(train[,-ycol],k)
    kout <- knnest(train[,ycol],xd,k)
    predy <- predict(kout , valid[,-ycol ], TRUE)
    realy <- valid [, col]
    if (meanabs)
      return(mean(abs(predy - realy )))
    list (predy = predy , realy = realy )
  }

lm_values <- matrix(NA, nrow= 5, ncol = 1)
knn_values <-  matrix(NA, nrow= 5, ncol = 1)

for (i in 1:10) {
  lm_values[i] <- xvallm(mlb, 5 , c(4,6), 2/3)
  knn_values[i] <- xvallm(mlb, 5 , c(4,6), 2/3)
}

output = data.frame(cbind(lm_values, knn_values))
colnames(output) = c("LM Validation", "KNN Validation")
print(output)



### Part 2

data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1, 12, 9 , 13, 14, 15, 8)]
pe <- as.matrix(pe)
mod_lm <- lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem + age:fem + age2:fem, data=prgeng)
pred_lm <- predict(mod_lm,  data.frame(age = 32, age2 = 32^2, wkswrkd = mean(prgeng$wkswrkd), ms = 1, phd = 0, fem = 1))
pred_lm



### Part 3

bodyfat = read.csv("bodyfat.csv")

n <- nrow(bodyfat)
p <- 0.2
ntrain <- round(p*n)
trainidxs <- sample(1:n, ntrain ,replace = FALSE)
train = bodyfat[trainidxs,]
valid = bodyfat[-trainidxs,]
head(train)
mod_lm <- lm(density ~ age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist, data=bodyfat)
mod_lm
valid$prediction <- predict(mod_lm,  valid)
mae <- mean(abs(valid$prediction - valid$density))
mae  # The extremely low value of mean absolute error implies indirect method can be utilized


### Part 4

## Sub-part a)

#Write English prose that relates the overall mean height of people and the gender-specific mean heights.
# The national mean height is the weighted average of gender-specific mean heights
# E(V) = EW = E[E(V | U)]
# where V = height
#       U = gender
# This may be computed as the sum of product of proportion of gender population and mean height of each gender


## Sub-part b)

#Write English prose that relates the overall proportion of people taller than 70 inches to the gender-specific proportions.
# The mean height of people taller than 70" is the weighted average of gender-specific mean heights of this subset
# E [E(V|U1,U2) | U1] = E(V | U1)
# where V = height
#       U1 = taller than 70 inches
#       U2 = gender



#### PROBLEM2

### Part 1

## Sub-part a)

data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(8,1,12,9,13,14,15)]
model_lm <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = pe)
summary(model_lm)
UCL <- -11176.74 + 1.96 * 912.206
LCL <- -11176.74 - 1.96 * 912.206
cat("Confidence interval is (", LCL, ",", UCL, ")")

## Sub-part b)

tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(8,1,12,9,13,14,15)]
model_lm <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + ms:fem + phd:fem, data = pe)
summary(model_lm)
UCL <- -5088.779 + 1.96 * 1975.841 
LCL <- -5088.779 - 1.96 * 1975.841
cat("Confidence interval is (", LCL, ",", UCL, ")")


### Part 2

day <- read.csv('day.csv')
day$temp2 <- (day$temp)^2
day$clearday <- as.integer(day$weathersit == 1)
model_lm <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
summary(model_lm)
LCL <- 1716.25 - 1.96 * 56.58
UCL <- 1716.25 + 1.96 * 56.58
head(day)



### Part 4

# rho^2 = 1 - (var(errors) / var(Y))
# var(errors) = p --------> given
# var(Xi) = 1 --------> given
# var(X1) + var(X2) + ..... + var(Xp) = p
# Y = X1 + X2 + ..... + Xp + errors
# var(Y) = p + p = 2p
# rho^2 = p / 2p 
#       = 1 / 2 
#       = 0.5









