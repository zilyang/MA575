Bike rental
================
Ziling Yang
9/11/2020

``` r
bikedata <- read.csv("day.csv",header=T)
names(bikedata)
```

    ##  [1] "instant"    "dteday"     "season"     "yr"         "mnth"      
    ##  [6] "holiday"    "weekday"    "workingday" "weathersit" "temp"      
    ## [11] "atemp"      "hum"        "windspeed"  "casual"     "registered"
    ## [16] "cnt"

# How temperature affects bike count

``` r
#Transform temp and atemp to o.temp and o.atemp without division
bikedata <- 
  bikedata %>% mutate(actual.temp = temp*41) %>% 
  mutate(actual.atemp = atemp*50) %>%
  mutate(actual.windspeed = windspeed*67) %>%
  mutate(actual.hum = hum*100)
```

# Regressing bike count with actual temperature

``` r
#Perform a simple linear regression 
actual_temp.simple.regress = (lm(bikedata$cnt ~ bikedata$actual.temp))
summary(actual_temp.simple.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$cnt ~ bikedata$actual.temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4615.3 -1134.9  -104.4  1044.3  3737.8 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          1214.642    161.164   7.537 1.43e-13 ***
    ## bikedata$actual.temp  161.969      7.444  21.759  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1509 on 729 degrees of freedom
    ## Multiple R-squared:  0.3937, Adjusted R-squared:  0.3929 
    ## F-statistic: 473.5 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
ggplot(bikedata, aes(actual.temp, cnt)) + geom_point(shape=1) + geom_abline(intercept = coef(actual_temp.simple.regress)[1], slope = coef(actual_temp.simple.regress)[2], colour = "red") + ylab('Total bike rental') + xlab('actual temperature') + labs(title = 'temperature against rental count (model imposed)')
```

![](Bike-rentals_files/figure-gfm/scatter%20for%20temperature%20against%20rental%20count-1.png)<!-- -->

``` r
plot(actual_temp.simple.regress)
```

![](Bike-rentals_files/figure-gfm/plot-1.png)<!-- -->![](Bike-rentals_files/figure-gfm/plot-2.png)<!-- -->![](Bike-rentals_files/figure-gfm/plot-3.png)<!-- -->![](Bike-rentals_files/figure-gfm/plot-4.png)<!-- -->
\# How temperature affects casual and registered bikers

``` r
casual_temp.regress = (lm(bikedata$casual ~ bikedata$actual.temp))
summary(casual_temp.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$casual ~ bikedata$actual.temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1005.4  -343.4  -142.5   131.1  2521.8 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -161.346     61.592   -2.62  0.00899 ** 
    ## bikedata$actual.temp   49.704      2.845   17.47  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 576.8 on 729 degrees of freedom
    ## Multiple R-squared:  0.2952, Adjusted R-squared:  0.2942 
    ## F-statistic: 305.3 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
registered_temp.regress = (lm(bikedata$registered ~ bikedata$actual.temp))
summary(registered_temp.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$registered ~ bikedata$actual.temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3616.9  -988.3  -116.1  1006.3  3357.8 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          1375.988    140.312   9.807   <2e-16 ***
    ## bikedata$actual.temp  112.265      6.481  17.323   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1314 on 729 degrees of freedom
    ## Multiple R-squared:  0.2916, Adjusted R-squared:  0.2906 
    ## F-statistic: 300.1 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
ggplot(bikedata, aes(x = actual.temp)) + geom_point(aes(y = registered,  color = "registered"), shape = 1) + geom_point(aes(y = casual,  color = "casual"), shape = 1) + geom_abline(intercept = coef(registered_temp.regress)[1], slope = coef(registered_temp.regress)[2], colour = "blue") +  geom_abline(intercept = coef(casual_temp.regress)[1], slope = coef(casual_temp.regress)[2], colour = "red") + ylab('bike rental count') + xlab('actual temperature') + labs(title = 'temperature against rental count (model imposed)', labels=c("registered", "casual"))
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Feel temperature and bike rental count

``` r
#Perform a simple linear regression 
feeltemp.simple.regress = (lm(bikedata$cnt ~ bikedata$actual.atemp))
summary(feeltemp.simple.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$cnt ~ bikedata$actual.atemp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4598.7 -1091.6   -91.8  1072.0  4383.7 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            945.824    171.291   5.522 4.67e-08 ***
    ## bikedata$actual.atemp  150.037      6.831  21.965  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1504 on 729 degrees of freedom
    ## Multiple R-squared:  0.3982, Adjusted R-squared:  0.3974 
    ## F-statistic: 482.5 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
ggplot(bikedata, aes(actual.temp, cnt)) + geom_point(shape=1) + geom_abline(intercept = coef(feeltemp.simple.regress)[1], slope = coef(feeltemp.simple.regress)[2], colour = "red") + ylab('Total bike rental') + xlab('feel temperature') + labs(title = 'feel temperature against rental count (model imposed)')
```

![](Bike-rentals_files/figure-gfm/scatter%20for%20feel%20temp%20against%20rental%20count-1.png)<!-- -->
\# How feel temperature affects casual and registered bikers

``` r
casual_feeltemp.regress = (lm(bikedata$casual ~ bikedata$actual.atemp))
summary(casual_feeltemp.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$casual ~ bikedata$actual.atemp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1126.1  -343.9  -142.9   148.3  2514.3 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -238.816     65.678  -3.636 0.000296 ***
    ## bikedata$actual.atemp   45.830      2.619  17.499  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 576.6 on 729 degrees of freedom
    ## Multiple R-squared:  0.2958, Adjusted R-squared:  0.2948 
    ## F-statistic: 306.2 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
registered_feeltemp.regress = (lm(bikedata$registered ~ bikedata$actual.atemp))
summary(registered_feeltemp.regress)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$registered ~ bikedata$actual.atemp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3607.1  -959.2  -153.8   998.2  3304.8 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            1184.64     149.21    7.94 7.67e-15 ***
    ## bikedata$actual.atemp   104.21       5.95   17.51  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1310 on 729 degrees of freedom
    ## Multiple R-squared:  0.2961, Adjusted R-squared:  0.2952 
    ## F-statistic: 306.7 on 1 and 729 DF,  p-value: < 2.2e-16

``` r
ggplot(bikedata, aes(x = actual.atemp)) + geom_point(aes(y = registered,  color = "registered"), shape = 1) + geom_point(aes(y = casual,  color = "casual"), shape = 1) + geom_abline(intercept = coef(registered_feeltemp.regress)[1], slope = coef(registered_feeltemp.regress)[2], colour = "blue") +  geom_abline(intercept = coef(casual_feeltemp.regress)[1], slope = coef(casual_feeltemp.regress)[2], colour = "red") + ylab('bike rental count') + xlab('feel temperature') + labs(title = 'feel temperature against rental count (model imposed)', labels=c("registered", "casual")) 
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Correlation between variables

``` r
cor(bikedata$actual.atemp, bikedata$cnt)
```

    ## [1] 0.6310657

``` r
cor(bikedata$actual.atemp, bikedata$casual)
```

    ## [1] 0.5438637

``` r
cor(bikedata$actual.atemp, bikedata$registered)
```

    ## [1] 0.5441918

``` r
cor(bikedata$actual.temp, bikedata$cnt)
```

    ## [1] 0.627494

``` r
cor(bikedata$actual.temp, bikedata$casual)
```

    ## [1] 0.5432847

``` r
cor(bikedata$actual.temp, bikedata$registered)
```

    ## [1] 0.540012

``` r
cor(bikedata$actual.atemp, bikedata$casual)
```

    ## [1] 0.5438637

``` r
cor(bikedata$actual.atemp, bikedata$actual.temp)
```

    ## [1] 0.9917016

# Multiple linear Regression

Use covariates holiday, weekday, weathersit, temp, hum ,windspeed to
regress total rental bike counts.

First we regress with temperature^2.

``` r
m.quadls_casual <- lm(bikedata$casual ~ bikedata$actual.temp + I(bikedata$actual.temp^2))
m.quadls_registered <- lm(bikedata$registered ~ bikedata$actual.temp + I(bikedata$actual.temp^2))
```

``` r
ggplot(bikedata, aes(x = actual.temp)) + geom_point(aes(y = registered,  color = "registered"), shape = 1) + geom_point(aes(y = casual,  color = "casual"), shape = 1) +   geom_line(data = fortify(m.quadls_casual), aes(x = bikedata$actual.temp, y = .fitted), color = "red") + geom_line(data = fortify(m.quadls_registered), aes(x = bikedata$actual.temp, y = .fitted), color = "blue") + labs(title = "Scatter plot with fitted quadratic model")
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
StanResQLS_casual <- rstandard(m.quadls_casual)
ggplot(bikedata, aes(x = casual, y=StanResQLS_casual)) + geom_point(shape = 1) + geom_hline(yintercept = -2) + geom_hline(yintercept = 2) + ggtitle("Standarized Residuals")
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
StanResQLS_registered <- rstandard(m.quadls_registered)
ggplot(bikedata, aes(x = registered, y=StanResQLS_registered)) + geom_point(shape = 1) + geom_hline(yintercept = -2) + geom_hline(yintercept = 2)+ ggtitle("Standarized Residuals")
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Most data points are within two standard deviations within the
prediction. This means that the model is a good fit.

``` r
summary(m.quadls_casual)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$casual ~ bikedata$actual.temp + I(bikedata$actual.temp^2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1020.0  -368.4  -116.3   179.1  2420.5 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               -844.3089   151.3762  -5.578 3.44e-08 ***
    ## bikedata$actual.temp       128.6265    16.2712   7.905 9.91e-15 ***
    ## I(bikedata$actual.temp^2)   -1.9626     0.3986  -4.924 1.05e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 567.9 on 728 degrees of freedom
    ## Multiple R-squared:  0.3179, Adjusted R-squared:  0.316 
    ## F-statistic: 169.6 on 2 and 728 DF,  p-value: < 2.2e-16

``` r
summary(m.quadls_registered)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$registered ~ bikedata$actual.temp + I(bikedata$actual.temp^2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3745.0  -947.5  -117.2   971.2  3000.2 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               -1057.6468   336.4358  -3.144  0.00174 ** 
    ## bikedata$actual.temp        393.4938    36.1629  10.881  < 2e-16 ***
    ## I(bikedata$actual.temp^2)    -6.9934     0.8859  -7.895 1.07e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1262 on 728 degrees of freedom
    ## Multiple R-squared:  0.3475, Adjusted R-squared:  0.3457 
    ## F-statistic: 193.8 on 2 and 728 DF,  p-value: < 2.2e-16

``` r
bikedata$season <- as.factor(bikedata$season)
bikedata$holiday <- as.factor(bikedata$holiday)
bikedata$weathersit <- as.factor(bikedata$weathersit)
bikedata$workingday <- as.factor(bikedata$workingday)
bikedata$yr <- as.factor(bikedata$yr)
```

``` r
m.mls_casual <- lm(bikedata$casual ~ bikedata$holiday + bikedata$weathersit + bikedata$workingday +  bikedata$actual.hum + bikedata$actual.temp+ bikedata$actual.windspeed)

summary(m.mls_casual)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$casual ~ bikedata$holiday + bikedata$weathersit + 
    ##     bikedata$workingday + bikedata$actual.hum + bikedata$actual.temp + 
    ##     bikedata$actual.windspeed)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1344.11  -217.48   -17.62   162.03  1782.49 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                974.483    110.161   8.846  < 2e-16 ***
    ## bikedata$holiday1         -312.884     94.911  -3.297  0.00103 ** 
    ## bikedata$weathersit2       -56.701     40.663  -1.394  0.16362    
    ## bikedata$weathersit3      -321.274    104.135  -3.085  0.00211 ** 
    ## bikedata$workingday1      -829.014     34.206 -24.236  < 2e-16 ***
    ## bikedata$actual.hum         -5.951      1.452  -4.099 4.62e-05 ***
    ## bikedata$actual.temp        51.258      2.137  23.985  < 2e-16 ***
    ## bikedata$actual.windspeed  -14.832      3.169  -4.681 3.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 414.5 on 723 degrees of freedom
    ## Multiple R-squared:  0.6391, Adjusted R-squared:  0.6356 
    ## F-statistic: 182.9 on 7 and 723 DF,  p-value: < 2.2e-16

``` r
m.mls_registered <- lm(bikedata$registered ~ bikedata$holiday + bikedata$weathersit + bikedata$workingday + bikedata$yr + bikedata$actual.hum + bikedata$actual.temp+ bikedata$actual.windspeed)

summary(m.mls_registered)
```

    ## 
    ## Call:
    ## lm(formula = bikedata$registered ~ bikedata$holiday + bikedata$weathersit + 
    ##     bikedata$workingday + bikedata$yr + bikedata$actual.hum + 
    ##     bikedata$actual.temp + bikedata$actual.windspeed)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3068.28  -514.58    10.66   569.09  1928.73 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1029.794    213.323   4.827 1.69e-06 ***
    ## bikedata$holiday1          -332.676    179.378  -1.855   0.0641 .  
    ## bikedata$weathersit2       -399.385     77.020  -5.185 2.80e-07 ***
    ## bikedata$weathersit3      -1492.762    196.813  -7.585 1.03e-13 ***
    ## bikedata$workingday1        941.423     64.646  14.563  < 2e-16 ***
    ## bikedata$yr1               1736.294     58.617  29.621  < 2e-16 ***
    ## bikedata$actual.hum          -2.618      2.765  -0.947   0.3441    
    ## bikedata$actual.temp         95.618      4.048  23.619  < 2e-16 ***
    ## bikedata$actual.windspeed   -37.389      5.992  -6.239 7.48e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 783.3 on 722 degrees of freedom
    ## Multiple R-squared:  0.7507, Adjusted R-squared:  0.7479 
    ## F-statistic: 271.8 on 8 and 722 DF,  p-value: < 2.2e-16

``` r
data <- data.frame(bikedata$casual, bikedata$holiday, bikedata$weathersit, bikedata$workingday, bikedata$actual.hum, bikedata$actual.temp, bikedata$actual.windspeed)

ggpairs(data, lower = list(continuous = wrap("points", alpha = 0.3, size= 0.1)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Bike-rentals_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
data <- data.frame(bikedata$registered, bikedata$holiday, bikedata$weathersit, bikedata$workingday, bikedata$actual.hum, bikedata$actual.temp, bikedata$actual.windspeed)

ggpairs(data, lower = list(continuous = wrap("points", alpha = 0.3, size= 0.1)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Bike-rentals_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
StanResMLS_registered <- rstandard(m.mls_registered)
ggplot(bikedata, aes(x = registered, y=StanResMLS_registered)) + geom_point(shape = 1) + geom_hline(yintercept = -2) + geom_hline(yintercept = 2)+ ggtitle("Standarized Residuals")
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
StanResMLS_casual <- rstandard(m.mls_casual)
ggplot(bikedata, aes(x = casual, y=StanResMLS_casual)) + geom_point(shape = 1) + geom_hline(yintercept = -2) + geom_hline(yintercept = 2)+ ggtitle("Standarized Residuals")
```

![](Bike-rentals_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
