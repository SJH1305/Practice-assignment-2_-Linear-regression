
## 10. This question should be answered using the Carseats data set.
install.packages('ISLR2')
library(ISLR2)
Carseats


## (a) Fit a multiple regression model to predict Sales using Price,
## Urban, and US.

summary(Carseats)

cor(Carseats$Price, Carseats$Sales)

## Fitting the model:
lm.fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)


## (e) On the basis of your response to the previous question, fit a
## smaller model that only uses the predictors for which there is
## evidence of association with the outcome.

lm.fit_small <- lm(Sales ~ Price + US, data = Carseats)
summary(lm.fit_small)


## (g) Using the model from (e), obtain 95 % confidence intervals for
## the coeﬀicient(s).

confint(lm.fit_small)

## (h) Is there evidence of outliers or high leverage observations in the
## model from (e)?

png("my_plot.png", width = 800, height = 600)
plot(predict(lm.fit_small), rstudent (lm.fit_small))
dev.off()

## Q15

Boston
head(Boston)

### Fitting the model
lm.fit_crim <- lm(medv ~ crim, data = Boston)
lm.fit_zn<- lm(medv ~ zn, data = Boston)
lm.fit_lstat <- lm(medv ~ lstat, data = Boston)
lm.fit_ptratio <- lm(medv ~ ptratio, data = Boston)
lm.fit_rad <- lm(medv ~ rad, data = Boston)
lm.fit_tax <- lm(medv ~ tax, data = Boston)
lm.fit_indus <- lm(medv ~ indus, data = Boston)
lm.fit_chas <- lm(medv ~ chas, data = Boston)
lm.fit_dis <- lm(medv ~ dis, data = Boston)
lm.fit_nox <- lm(medv ~ nox, data = Boston)
lm.fit_rm <- lm(medv ~ rm, data = Boston)
lm.fit_age <- lm(medv ~ age, data = Boston)

summary(lm.fit_crim)
summary(lm.fit_zn)
summary(lm.fit_indus)
summary(lm.fit_chas)
summary(lm.fit_nox)
summary(lm.fit_rm)
summary(lm.fit_age)
summary(lm.fit_dis)
summary(lm.fit_rad)
summary(lm.fit_tax)
summary(lm.fit_ptratio)
summary(lm.fit_lstat)

# 1. Setup the grid (3 rows, 4 columns)
par(mfrow = c(3, 4))

# 2. List the variable names exactly as they appear in the dataset
# (Excluding 'medv' because it's our Y-axis)
predictors <- c("crim", "zn", "indus", "chas", "nox", "rm", 
                "age", "dis", "rad", "tax", "ptratio", "lstat")

# 3. The Loop
for (var in predictors) {
  # Plot the raw data
  plot(Boston[[var]], Boston$medv, 
       main = var, 
       xlab = var, 
       ylab = "medv", 
       pch = 20, 
       col = "darkgrey")
  
  # Construct the name of your model (e.g., "lm.fit_crim")
  # and use get() to fetch the actual model object
  model_name <- paste0("lm.fit_", var)
  model_obj <- get(model_name)
  
  # Draw the regression line from that specific model
  abline(model_obj, col = "red", lwd = 2)
}

## b 

lm.fit_Boston <- lm(medv ~ ., data = Boston)
summary(lm.fit_Boston)

## c
### Manual plotting:
simple_coeffs <- c(-0.415, 0.142, -0.648, 6.346, -33.916, 9.102, -0.123, 1.091, -0.403, -0.025, -2.157, -0.950)
multiple_coeffs <- c(-0.121, 0.046, 0.013, 2.839, -18.758, 3.658, 0.003, -1.490, 0.289, -0.012, -0.937, -0.552)

# 2. Plot the points
png("coeff_manual.png", width=800, height=600)
plot(simple_coeffs, multiple_coeffs, 
     main="Simple vs. Multiple Regression Coefficients",
     xlab="Univariate (Simple) Coefficients", 
     ylab="Multiple Regression Coefficients",
     pch=19, col="blue")

# 3. Add a 45-degree reference line (where x = y)
abline(0, 1, col="red", lty=2)
dev.off()

## Suggested method
# Collect the 12 simple coefficients (slopes)
png("coeff_suggested.png", width=800, height=600)
simple_coeffs <- c(coef(lm.fit_crim)[2], coef(lm.fit_zn)[2], 
                   coef(lm.fit_indus)[2], coef(lm.fit_chas)[2], 
                   coef(lm.fit_nox)[2], coef(lm.fit_rm)[2], 
                   coef(lm.fit_age)[2], coef(lm.fit_dis)[2], 
                   coef(lm.fit_rad)[2], coef(lm.fit_tax)[2], 
                   coef(lm.fit_ptratio)[2], coef(lm.fit_lstat)[2])

# Collect the 13 multiple coefficients (excluding the intercept)
multiple_coeffs <- coef(lm.fit_Boston)[-1]

plot(simple_coeffs, multiple_coeffs, 
     pch = 19, col = "blue",
     xlab = "Univariate (Simple) Coefficients",
     ylab = "Multiple Regression Coefficients",
     main = "Simple vs. Multiple Regression Coefficients")

# Add a 45-degree line to see which ones changed most
abline(0, 1, col = "red", lty = 2)
dev.off()

## d

## Manual method:
### Derived R^2 readings from previous summary functions
### Polynomial models:
lm.fit_polycrim <- lm(medv ~ poly(crim, 3), data = Boston)
lm.fit_polyzn      <- lm(medv ~ poly(zn, 3), data = Boston)
lm.fit_polyindus   <- lm(medv ~ poly(indus, 3), data = Boston)
lm.fit_polynox     <- lm(medv ~ poly(nox, 3), data = Boston)
lm.fit_polyrm      <- lm(medv ~ poly(rm, 3), data = Boston)
lm.fit_polyage     <- lm(medv ~ poly(age, 3), data = Boston)
lm.fit_polydis     <- lm(medv ~ poly(dis, 3), data = Boston)
lm.fit_polyrad     <- lm(medv ~ poly(rad, 3), data = Boston)
lm.fit_polytax     <- lm(medv ~ poly(tax, 3), data = Boston)
lm.fit_polyptratio <- lm(medv ~ poly(ptratio, 3), data = Boston)
lm.fit_polylstat   <- lm(medv ~ poly(lstat, 3), data = Boston)

summary(lm.fit_polycrim)
summary(lm.fit_polycrim)
summary(lm.fit_polyzn)
summary(lm.fit_polyindus)
summary(lm.fit_polynox)
summary(lm.fit_polyrm)
summary(lm.fit_polyage)
summary(lm.fit_polydis)
summary(lm.fit_polyrad)
summary(lm.fit_polytax)
summary(lm.fit_polyptratio)
summary(lm.fit_polylstat)

## Suggested model:
# Get names of all predictors, predictors but specifically exclude 'medv' AND 'chas'(dummy categorical)
predictors <- names(Boston)[!names(Boston) %in% c("medv", "chas")]

# Fit a cubic model for each predictor and store in a list
cubic_models <- lapply(predictors, function(x) {
  formula <- as.formula(paste("medv ~ poly(", x, ", 3)"))
  return(lm(formula, data = Boston))
})

# Name the list elements for easy access
names(cubic_models) <- predictors

summary(cubic_models$crim)
summary(cubic_models$zn)
summary(cubic_models$indus)
summary(cubic_models$nox)
summary(cubic_models$rm)
summary(cubic_models$age)
summary(cubic_models$dis)
summary(cubic_models$rad)
summary(cubic_models$tax)
summary(cubic_models$ptratio)
summary(cubic_models$lstat)



