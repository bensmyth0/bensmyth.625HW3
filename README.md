# bensmyth.625HW3
Submission for BIOSTAT 625 Homework 3

This package seeks to reimplement the command for linear models in base R (lm) as a new command linear.
* linear() runs a linear regression model given the specified response variable and predictor variables.

## Installation
Using the package `devtools`, the package may be installed directly from github in the following manner:

```{r}
install.packages("devtools")
devtools::install_github(bensmyth0/bensmyth.625HW3)
```
## Usage
```{r}
# Using the built-in dataset swiss

head(swiss)
#>              Fertility Agriculture Examination Education Catholic Infant.Mortality
#> Courtelary        80.2        17.0          15        12     9.96             22.2
#> Delemont          83.1        45.1           6         9    84.84             22.2
#> Franches-Mnt      92.5        39.7           5         5    93.40             20.2
#> Moutier           85.8        36.5          12         7    33.77             20.3
#> Neuveville        76.9        43.5          17        15     5.16             20.6
#> Porrentruy        76.1        35.3           9         7    90.57             26.6

# Predicting Infant Mortality based on Fertility

model1 <- linear(y = "Infant.Mortality", x = "Fertility", data = swiss)
model1$beta
#>      Intercept  Fertility
#> [1,]   13.1297 0.09712863

```
