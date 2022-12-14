BST 210 Project
================
2022-12-05

# Data Cleaning

``` r
data <-
  data %>%
  dplyr::select(AGE, SEX, RACENEW, INCFAM07ON, HEALTH, MOD10DMIN, ANXIETYEV, DEPFREQ, EMOSUPPORT, CVDSYMP, REGION) %>% 
  filter(MOD10DMIN != 000) %>% 
  filter(ANXIETYEV != 0) %>% 
  filter(DEPFREQ != 0) %>% 
  filter(EMOSUPPORT != 0) %>% 
  filter(CVDSYMP != 0)
```

``` r
data$AGE[data$AGE == 997] = NA
data$AGE[data$AGE == 999] = NA
```

``` r
data$sexcat <- NA
data$sexcat[data$SEX == 1] = "male"
data$sexcat[data$SEX == 2] = "female"
data$sexcat <- as.factor(data$sexcat)
```

``` r
data$racecat <- NA
data$racecat[data$RACENEW == 100] = "white"
data$racecat[data$RACENEW == 200] = "non-white"
data$racecat[data$RACENEW == 300] = "non-white"
data$racecat[data$RACENEW == 400] = "non-white"
data$racecat[data$RACENEW > 400 & data$RACENEW < 900] = "non-white"
data$racecat <- as.factor(data$racecat)
```

``` r
data$incomecat <- NA
data$incomecat[data$INCFAM07ON == 11] = "under $49,999"
data$incomecat[data$INCFAM07ON == 12] = "under $49,999"
data$incomecat[data$INCFAM07ON == 22] = "$50,000-$99,999"
data$incomecat[data$INCFAM07ON == 23] = "$50,000-$99,999"
data$incomecat[data$INCFAM07ON == 24] = "over $100,000"
data$incomecat <- factor(data$incomecat, levels = c("under $49,999", "$50,000-$99,999", "over $100,000"))
```

``` r
data$healthcat <- NA
data$healthcat[data$HEALTH == 1] = "good"
data$healthcat[data$HEALTH == 2] = "good"
data$healthcat[data$HEALTH == 3] = "good"
data$healthcat[data$HEALTH == 4] = "fair"
data$healthcat[data$HEALTH == 5] = "poor"
data$healthcat <- factor(data$healthcat, levels = c("good", "fair", "poor"))
```

``` r
data$MOD10DMIN[data$MOD10DMIN %in% c(996, 997, 998, 999)] = NA
```

``` r
data$anxiety <- NA
data$anxiety[data$ANXIETYEV == 1] = "No"
data$anxiety[data$ANXIETYEV == 2] = "Yes"
data$anxiety <- factor(data$anxiety, levels = c("No", "Yes"))
```

``` r
data$depress <- NA
data$depress[data$DEPFREQ == 1] = "daily"
data$depress[data$DEPFREQ == 2] = "weekly or monthly"
data$depress[data$DEPFREQ == 3] = "weekly or monthly"
data$depress[data$DEPFREQ == 4] = "yearly"
data$depress[data$DEPFREQ == 5] = "never"
data$depress <- factor(data$depress, levels = c("never", "daily", "weekly or monthly", "yearly"))
```

``` r
data$esupport <- NA
data$esupport[data$EMOSUPPORT == 1] = "always"
data$esupport[data$EMOSUPPORT == 2] = "usually"
data$esupport[data$EMOSUPPORT == 3] = "sometimes-never"
data$esupport[data$EMOSUPPORT == 4] = "sometimes-never"
data$esupport[data$EMOSUPPORT == 5] = "sometimes-never"
data$esupport <- factor(data$esupport, levels = c("always", "usually", "sometimes-never"))
```

``` r
data$sympt <- NA
data$sympt[data$CVDSYMP == 1] = "no symptoms"
data$sympt[data$CVDSYMP == 2] = "mild symptoms"
data$sympt[data$CVDSYMP == 3] = "moderate symptoms"
data$sympt[data$CVDSYMP == 4] = "severe symptoms"
data$sympt <- factor(data$sympt, levels = c("no symptoms", "mild symptoms", "moderate symptoms", "severe symptoms"))
```

``` r
data$reg <- NA
data$reg[data$REGION == 1] = "northeast"
data$reg[data$REGION == 2] = "midwest"
data$reg[data$REGION == 3] = "south"
data$reg[data$REGION == 4] = "west"
data$reg <- factor(data$reg, levels = c("northeast", "midwest", "south", "west"))
```

``` r
data <- data %>% 
  dplyr::select(12:20, AGE, MOD10DMIN)

data %>% 
  summary()
```

    ##     sexcat         racecat              incomecat   healthcat  anxiety  
    ##  female:241   non-white: 74   under $49,999  :140   good:403   No :368  
    ##  male  :205   white    :334   $50,000-$99,999:126   fair: 39   Yes: 78  
    ##               NA's     : 38   over $100,000  :180   poor:  4            
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##               depress               esupport                 sympt    
    ##  never            :239   always         :239   no symptoms      : 60  
    ##  daily            : 16   usually        :115   mild symptoms    :144  
    ##  weekly or monthly: 65   sometimes-never: 82   moderate symptoms:162  
    ##  yearly           :125   NA's           : 10   severe symptoms  : 79  
    ##  NA's             :  1                         NA's             :  1  
    ##                                                                       
    ##                                                                       
    ##         reg           AGE          MOD10DMIN     
    ##  northeast: 70   Min.   :18.00   Min.   :  1.00  
    ##  midwest  :109   1st Qu.:31.00   1st Qu.: 30.00  
    ##  south    :172   Median :44.00   Median : 45.00  
    ##  west     : 95   Mean   :45.12   Mean   : 58.75  
    ##                  3rd Qu.:58.00   3rd Qu.: 60.00  
    ##                  Max.   :85.00   Max.   :480.00  
    ##                  NA's   :1       NA's   :3

``` r
# dim(data)[1]*0.05 # 5% missing 22.3

data <- data %>% drop_na()

# dim(data) # 397*11
```

# Data Visualization

``` r
data %>% 
  ggplot(aes(x=sexcat, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=sexcat, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

> Female seems to be positively associated with anxiety and depression.

``` r
data %>% 
  ggplot(aes(x=racecat, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=racecat, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

> White and other seem to be at higher risk of anxiety and depression.

``` r
data %>% 
  ggplot(aes(x=incomecat, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=incomecat, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

> Higher income seems to be a protective factor.

``` r
data %>% 
  ggplot(aes(x=healthcat, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=healthcat, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

> Poor health status is highly associated with anxiety and especially
> depression.

``` r
data %>% 
  ggplot(aes(x=esupport, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=esupport, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

> Participants who rarely or sometimes receive emotional support seem to
> be under higher risk of anxiety and depression.

``` r
data %>% 
  ggplot(aes(x=sympt, fill=anxiety)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=sympt, fill=depress)) +
  geom_bar(position="fill")
```

![](README_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

> Severity of covid symptoms seems to be positively associated with
> anxiety and depression.

``` r
data %>% 
  ggplot(aes(x=anxiety,y=AGE)) +
  geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=depress,y=AGE)) +
  geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

> Participants with anxiety are on average younger, while participants
> who feel depressed daily are older between 40 and 60.

``` r
data %>% 
  ggplot(aes(x=anxiety,y=MOD10DMIN)) +
  geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x=depress,y=MOD10DMIN)) +
  geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

> Exercise seems to be a protective factor.

``` r
data %>% 
  ggplot(aes(x = AGE)) +
  geom_density()
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(x = MOD10DMIN)) +
  geom_density()
```

![](README_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

# Modeling

## Anxiety - logistic

``` r
data_logis <- data %>% dplyr::select(-depress)


logis1 <- glm(anxiety ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt + reg + esupport + incomecat,
              family = binomial, data = data_logis)
logis2 <- glm(anxiety ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis3 <- glm(anxiety ~ AGE + sexcat + racecat + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis4 <- glm(anxiety ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt,
              family = binomial, data = data_logis)
logis5 <- glm(anxiety ~ AGE + MOD10DMIN + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis6 <- glm(anxiety ~ AGE + sexcat + racecat + healthcat + sympt,
              family = binomial, data = data_logis)
logis7 <- glm(anxiety ~ AGE + healthcat + sympt,
              family = binomial, data = data_logis)
logis8 <- glm(anxiety ~ sexcat + racecat + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis9 <- glm(anxiety ~ AGE + racecat + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis10 <- glm(anxiety ~ AGE + sexcat + healthcat + sympt + esupport,
              family = binomial, data = data_logis)
logis11 <- glm(anxiety ~ AGE + sexcat + racecat + sympt + esupport,
              family = binomial, data = data_logis)
```

``` r
logis6$coefficients[7:9]
```

    ##     symptmild symptoms symptmoderate symptoms   symptsevere symptoms 
    ##            -0.25501804            -0.07268736             0.16699790

``` r
# logis1$coefficients[8:10]
# logis2$coefficients[8:10]
# logis3$coefficients[7:9]
# logis4$coefficients[8:10]
# logis5$coefficients[6:8]
# logis6$coefficients[7:9]
# logis7$coefficients[5:7]
# logis8$coefficients[6:8]
# logis9$coefficients[6:8]
# logis10$coefficients[6:8]
# logis11$coefficients[5:7]

# # test MOD10DMIN
# (logis3$coefficients[7]-logis2$coefficients[8])/logis2$coefficients[8]*100
# (logis3$coefficients[8]-logis2$coefficients[9])/logis2$coefficients[9]*100
# (logis3$coefficients[9]-logis2$coefficients[10])/logis2$coefficients[10]*100

# # test esupport - >10%
# (logis6$coefficients[7]-logis3$coefficients[7])/logis3$coefficients[7]*100
# (logis6$coefficients[8]-logis3$coefficients[8])/logis3$coefficients[8]*100
# (logis6$coefficients[9]-logis3$coefficients[9])/logis3$coefficients[9]*100

# test AGE - >10%
(logis8$coefficients[6]-logis3$coefficients[7])/logis3$coefficients[7]*100
```

    ## symptmild symptoms 
    ##          -7.247651

``` r
(logis8$coefficients[7]-logis3$coefficients[8])/logis3$coefficients[8]*100
```

    ## symptmoderate symptoms 
    ##             -0.5919602

``` r
(logis8$coefficients[8]-logis3$coefficients[9])/logis3$coefficients[9]*100
```

    ## symptsevere symptoms 
    ##            -18.81389

``` r
# test sexcat - >10%
(logis9$coefficients[6]-logis3$coefficients[7])/logis3$coefficients[7]*100
```

    ## symptmild symptoms 
    ##           2.654435

``` r
(logis9$coefficients[7]-logis3$coefficients[8])/logis3$coefficients[8]*100
```

    ## symptmoderate symptoms 
    ##              -40.22001

``` r
(logis9$coefficients[8]-logis3$coefficients[9])/logis3$coefficients[9]*100
```

    ## symptsevere symptoms 
    ##            -3.248635

``` r
# test racecat - >10%
(logis10$coefficients[6]-logis3$coefficients[7])/logis3$coefficients[7]*100
```

    ## symptmild symptoms 
    ##          -46.63908

``` r
(logis10$coefficients[7]-logis3$coefficients[8])/logis3$coefficients[8]*100
```

    ## symptmoderate symptoms 
    ##               -170.492

``` r
(logis10$coefficients[8]-logis3$coefficients[9])/logis3$coefficients[9]*100
```

    ## symptsevere symptoms 
    ##             42.57754

``` r
# test healthcat - >10%
(logis11$coefficients[5]-logis3$coefficients[7])/logis3$coefficients[7]*100
```

    ## symptmild symptoms 
    ##          -8.735373

``` r
(logis11$coefficients[6]-logis3$coefficients[8])/logis3$coefficients[8]*100
```

    ## symptmoderate symptoms 
    ##               60.36972

``` r
(logis11$coefficients[7]-logis3$coefficients[9])/logis3$coefficients[9]*100
```

    ## symptsevere symptoms 
    ##             43.66862

``` r
anova(logis2,logis1,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.5350914

``` r
anova(logis3,logis2,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.9396426

``` r
anova(logis4,logis2,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.1151778

``` r
anova(logis5,logis2,test="Chisq")$`Pr(>Chi)` # p<<0.05
```

    ## [1]         NA 0.01761369

``` r
anova(logis6,logis3,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.1154385

``` r
anova(logis6,logis4,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.9722419

``` r
anova(logis7,logis6,test="Chisq")$`Pr(>Chi)` # p<<0.05
```

    ## [1]         NA 0.01862658

``` r
# logis6 based on LRT
```

``` r
# no significant interaction

logis6_h <- glm(anxiety ~ AGE + sexcat + racecat + healthcat:sympt,
              family = binomial, data = data_logis)
logis6_r <- glm(anxiety ~ AGE + sexcat + racecat:sympt + healthcat,
              family = binomial, data = data_logis)
logis6_s <- glm(anxiety ~ AGE + sexcat:sympt + racecat + healthcat,
              family = binomial, data = data_logis)

anova(logis6,logis6_h,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.2869032

``` r
anova(logis6,logis6_r,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.6422804

``` r
anova(logis6,logis6_s,test="Chisq")$`Pr(>Chi)` # p>0.05
```

    ## [1]        NA 0.3955457

``` r
# options(scipen = 999)
tlogis1 = tidy(logis1)
tlogis2 = tidy(logis2)
tlogis3 = tidy(logis3)
tlogis4 = tidy(logis4)
tlogis5 = tidy(logis5)
tlogis6 = tidy(logis6)
tlogis7 = tidy(logis7)

tlogis1$term[tlogis1$p.value < 0.05]
```

    ## [1] "(Intercept)"   "racecatwhite"  "healthcatfair"

``` r
tlogis2$term[tlogis2$p.value < 0.05]
```

    ## [1] "(Intercept)"             "sexcatmale"             
    ## [3] "healthcatfair"           "esupportsometimes-never"

``` r
tlogis3$term[tlogis3$p.value < 0.05]
```

    ## [1] "(Intercept)"             "sexcatmale"             
    ## [3] "healthcatfair"           "esupportsometimes-never"

``` r
tlogis4$term[tlogis4$p.value < 0.05]
```

    ## [1] "(Intercept)"   "healthcatfair"

``` r
tlogis5$term[tlogis5$p.value < 0.05]
```

    ## [1] "(Intercept)"             "healthcatfair"          
    ## [3] "esupportsometimes-never"

``` r
tlogis6$term[tlogis6$p.value < 0.05]
```

    ## [1] "(Intercept)"   "healthcatfair"

``` r
tlogis7$term[tlogis7$p.value < 0.05]
```

    ## [1] "(Intercept)"   "healthcatfair"

``` r
# logis2, logis3 based on number of significant covariates
```

``` r
# hist(logis$fitted, main="p-hats")
# hist(hatvalues(logis), main="hat-values")

# H0: fit of the model is adequate
options(digits=7)
hoslem.test(logis1$y,fitted(logis1),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis1$y, fitted(logis1)
    ## X-squared = 4.6925, df = 8, p-value = 0.7899

``` r
hoslem.test(logis2$y,fitted(logis2),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis2$y, fitted(logis2)
    ## X-squared = 13.236, df = 8, p-value = 0.104

``` r
hoslem.test(logis3$y,fitted(logis3),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis3$y, fitted(logis3)
    ## X-squared = 9.9871, df = 8, p-value = 0.2659

``` r
hoslem.test(logis4$y,fitted(logis4),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis4$y, fitted(logis4)
    ## X-squared = 16.188, df = 8, p-value = 0.03977

``` r
hoslem.test(logis5$y,fitted(logis5),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis5$y, fitted(logis5)
    ## X-squared = 5.6385, df = 8, p-value = 0.6876

``` r
hoslem.test(logis6$y,fitted(logis6),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis6$y, fitted(logis6)
    ## X-squared = 13.009, df = 8, p-value = 0.1115

``` r
hoslem.test(logis7$y,fitted(logis7),g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  logis7$y, fitted(logis7)
    ## X-squared = 8.6109, df = 8, p-value = 0.3762

``` r
# logis1, logis5, logis7, logis3, logis6, logis2 based on H-L GOF
```

``` r
predprob1 <- predict(logis1,type=c("response"))
roccurve1 <- roc(data_logis$anxiety ~ predprob1)
predprob2 <- predict(logis2,type=c("response"))
roccurve2 <- roc(data_logis$anxiety ~ predprob2)
predprob3 <- predict(logis3,type=c("response"))
roccurve3 <- roc(data_logis$anxiety ~ predprob3)
predprob4 <- predict(logis4,type=c("response"))
roccurve4 <- roc(data_logis$anxiety ~ predprob4)
predprob5 <- predict(logis5,type=c("response"))
roccurve5 <- roc(data_logis$anxiety ~ predprob5)
predprob6 <- predict(logis6,type=c("response"))
roccurve6 <- roc(data_logis$anxiety ~ predprob6)
predprob7 <- predict(logis7,type=c("response"))
roccurve7 <- roc(data_logis$anxiety ~ predprob7)
```

``` r
data.frame(MODEL=c("logis1","logis2","logis3","logis4","logis5","logis6","logis7"),
           AIC=c(AIC(logis1),AIC(logis2),AIC(logis3),AIC(logis4),AIC(logis5),AIC(logis6),AIC(logis7)),
           BIC=c(BIC(logis1),BIC(logis2),BIC(logis3),BIC(logis4),BIC(logis5),BIC(logis6),BIC(logis7)),
           AUC=c(auc(roccurve1),auc(roccurve2),auc(roccurve3),auc(roccurve4),auc(roccurve5),auc(roccurve6),auc(roccurve7)))
```

    ##    MODEL      AIC      BIC       AUC
    ## 1 logis1 375.5568 443.2837 0.6869157
    ## 2 logis2 369.6569 417.4641 0.6741372
    ## 3 logis3 367.6626 411.4859 0.6742901
    ## 4 logis4 369.9795 409.8188 0.6674967
    ## 5 logis5 373.7351 413.5744 0.6338139
    ## 6 logis6 367.9807 403.8361 0.6683705
    ## 7 logis7 371.9470 399.8346 0.6055920

``` r
# logis3, logis6 based on AIC
# logis7, logis6 based on BIC
# logis1, logis3, logis2, logis6 based on AUC

# stepModel <- step(logis1, direction=c("both"))
# sexcat + racecat + healthcat + esupport based on stepwise selection

pred<-ifelse(predict(logis3,type="response")>0.5,"Yes","No")
actu<-data_logis$anxiety
confusionMatrix(table(pred,actu))
```

    ## Confusion Matrix and Statistics
    ## 
    ##      actu
    ## pred   No Yes
    ##   No  324  67
    ##   Yes   3   3
    ##                                           
    ##                Accuracy : 0.8237          
    ##                  95% CI : (0.7826, 0.8599)
    ##     No Information Rate : 0.8237          
    ##     P-Value [Acc > NIR] : 0.5319          
    ##                                           
    ##                   Kappa : 0.0526          
    ##                                           
    ##  Mcnemar's Test P-Value : 5.076e-14       
    ##                                           
    ##             Sensitivity : 0.99083         
    ##             Specificity : 0.04286         
    ##          Pos Pred Value : 0.82864         
    ##          Neg Pred Value : 0.50000         
    ##              Prevalence : 0.82368         
    ##          Detection Rate : 0.81612         
    ##    Detection Prevalence : 0.98489         
    ##       Balanced Accuracy : 0.51684         
    ##                                           
    ##        'Positive' Class : No              
    ## 

``` r
pred<-ifelse(predict(logis6,type="response")>0.5,"Yes","No")
actu<-data_logis$anxiety
confusionMatrix(table(pred,actu))
```

    ## Confusion Matrix and Statistics
    ## 
    ##      actu
    ## pred   No Yes
    ##   No  325  67
    ##   Yes   2   3
    ##                                           
    ##                Accuracy : 0.8262          
    ##                  95% CI : (0.7853, 0.8622)
    ##     No Information Rate : 0.8237          
    ##     P-Value [Acc > NIR] : 0.4794          
    ##                                           
    ##                   Kappa : 0.0579          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.312e-14       
    ##                                           
    ##             Sensitivity : 0.99388         
    ##             Specificity : 0.04286         
    ##          Pos Pred Value : 0.82908         
    ##          Neg Pred Value : 0.60000         
    ##              Prevalence : 0.82368         
    ##          Detection Rate : 0.81864         
    ##    Detection Prevalence : 0.98741         
    ##       Balanced Accuracy : 0.51837         
    ##                                           
    ##        'Positive' Class : No              
    ## 

``` r
VIF(logis1) # no collinearity
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## AGE       1.086843  1        1.042518
    ## sexcat    1.026409  1        1.013119
    ## racecat   1.080902  1        1.039664
    ## MOD10DMIN 1.058572  1        1.028869
    ## healthcat 1.101702  2        1.024510
    ## sympt     1.176108  3        1.027404
    ## reg       1.141802  3        1.022347
    ## esupport  1.101648  2        1.024497
    ## incomecat 1.148030  2        1.035114

``` r
VIF(logis3) # no collinearity
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## AGE       1.063723  1        1.031370
    ## sexcat    1.018727  1        1.009320
    ## racecat   1.052703  1        1.026013
    ## healthcat 1.052728  2        1.012929
    ## sympt     1.121260  3        1.019259
    ## esupport  1.038605  2        1.009514

``` r
VIF(logis6) # no collinearity
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## AGE       1.064500  1        1.031746
    ## sexcat    1.017052  1        1.008490
    ## racecat   1.053983  1        1.026637
    ## healthcat 1.041530  2        1.010225
    ## sympt     1.102608  3        1.016413

## Depression - multinomial & ordinal

``` r
data_multi <- data %>% dplyr::select(-anxiety)

multi1 <- multinom(depress ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt + reg + esupport + incomecat, data = data_multi)
multi2 <- multinom(depress ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt + esupport, data = data_multi)
multi3 <- multinom(depress ~ AGE + sexcat + racecat + healthcat + sympt + esupport, data = data_multi)
multi4 <- multinom(depress ~ AGE + sexcat + racecat + MOD10DMIN + healthcat + sympt, data = data_multi)
multi5 <- multinom(depress ~ AGE + MOD10DMIN + healthcat + sympt + esupport, data = data_multi)
multi6 <- multinom(depress ~ AGE + sexcat + racecat + healthcat + sympt, data = data_multi)
multi7 <- multinom(depress ~ AGE + healthcat + sympt, data = data_multi)
multi8 <- multinom(depress ~ sexcat + racecat + healthcat + sympt, data = data_multi)
multi9 <- multinom(depress ~ AGE + racecat + healthcat + sympt, data = data_multi)
multi10 <- multinom(depress ~ AGE + sexcat + healthcat + sympt, data = data_multi)
multi11 <- multinom(depress ~ AGE + sexcat + racecat + sympt, data = data_multi)
```

``` r
coef(multi1)[,8:10]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -2.0535995             -0.5519945
    ## weekly or monthly         -0.3050451              0.3222653
    ## yearly                     0.3562810              1.0385223
    ##                   symptsevere symptoms
    ## daily                        1.1558441
    ## weekly or monthly            0.4233404
    ## yearly                       1.0054904

``` r
coef(multi2)[,8:10]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -2.2524725             -0.6565946
    ## weekly or monthly         -0.3457732              0.3054771
    ## yearly                     0.2719140              1.0075965
    ##                   symptsevere symptoms
    ## daily                        0.9393633
    ## weekly or monthly            0.4667145
    ## yearly                       1.0528752

``` r
coef(multi3)[,7:9]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -2.2510663             -0.6787723
    ## weekly or monthly         -0.3251514              0.3382121
    ## yearly                     0.2658774              1.0031350
    ##                   symptsevere symptoms
    ## daily                        0.9339796
    ## weekly or monthly            0.5064456
    ## yearly                       1.0427955

``` r
coef(multi4)[,8:10]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.8718049             -0.3192431
    ## weekly or monthly         -0.2491003              0.4317267
    ## yearly                     0.2993094              1.0585815
    ##                   symptsevere symptoms
    ## daily                        1.0688915
    ## weekly or monthly            0.4289015
    ## yearly                       1.0290449

``` r
coef(multi5)[,6:8]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -2.0273424             -0.3756657
    ## weekly or monthly         -0.2905221              0.3822645
    ## yearly                     0.2793311              1.0294561
    ##                   symptsevere symptoms
    ## daily                        1.0955201
    ## weekly or monthly            0.4878121
    ## yearly                       1.0493774

``` r
coef(multi6)[,7:9]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.8742328             -0.3322938
    ## weekly or monthly         -0.2367581              0.4512837
    ## yearly                     0.2900526              1.0503638
    ##                   symptsevere symptoms
    ## daily                         1.064912
    ## weekly or monthly             0.454630
    ## yearly                        1.011867

``` r
coef(multi7)[,5:7]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.7706372             -0.1299562
    ## weekly or monthly         -0.1805483              0.5371027
    ## yearly                     0.2993619              1.0757372
    ##                   symptsevere symptoms
    ## daily                        1.1243872
    ## weekly or monthly            0.4804613
    ## yearly                       1.0113661

``` r
coef(multi8)[,6:8]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.8476615             -0.3362301
    ## weekly or monthly         -0.1705624              0.4410487
    ## yearly                     0.3466814              1.0479237
    ##                   symptsevere symptoms
    ## daily                        1.0351213
    ## weekly or monthly            0.3613692
    ## yearly                       0.9516154

``` r
coef(multi9)[,6:8]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.9205865             -0.3060473
    ## weekly or monthly         -0.2455915              0.4593797
    ## yearly                     0.2800716              1.0530914
    ##                   symptsevere symptoms
    ## daily                        1.0501802
    ## weekly or monthly            0.4456849
    ## yearly                       0.9996263

``` r
coef(multi10)[,6:8]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.7099026             -0.1238559
    ## weekly or monthly         -0.1683309              0.5331414
    ## yearly                     0.3090951              1.0723951
    ##                   symptsevere symptoms
    ## daily                        1.1521669
    ## weekly or monthly            0.4918571
    ## yearly                       1.0231320

``` r
coef(multi11)[,5:7]
```

    ##                   symptmild symptoms symptmoderate symptoms
    ## daily                     -1.7795110             -0.4365974
    ## weekly or monthly         -0.1996737              0.4003010
    ## yearly                     0.2808907              1.0510576
    ##                   symptsevere symptoms
    ## daily                        1.1863228
    ## weekly or monthly            0.4998809
    ## yearly                       0.9991090

``` r
# # test MOD10DMIN
# (coef(multi3)[1,7]-coef(multi2)[1,8])/coef(multi2)[1,8]*100
# (coef(multi3)[1,8]-coef(multi2)[1,9])/coef(multi2)[1,9]*100
# (coef(multi3)[1,9]-coef(multi2)[1,10])/coef(multi2)[1,10]*100
# 
# (coef(multi3)[2,7]-coef(multi2)[2,8])/coef(multi2)[2,8]*100
# (coef(multi3)[2,8]-coef(multi2)[2,9])/coef(multi2)[2,9]*100
# (coef(multi3)[2,9]-coef(multi2)[2,10])/coef(multi2)[2,10]*100
# 
# (coef(multi3)[3,7]-coef(multi2)[3,8])/coef(multi2)[3,8]*100
# (coef(multi3)[3,8]-coef(multi2)[3,9])/coef(multi2)[3,9]*100
# (coef(multi3)[3,9]-coef(multi2)[3,10])/coef(multi2)[3,10]*100
```

``` r
# test AGE - >10%
(coef(multi8)[1,6]-coef(multi6)[1,7])/coef(multi6)[1,7]*100
```

    ## [1] -1.417718

``` r
(coef(multi8)[1,7]-coef(multi6)[1,8])/coef(multi6)[1,8]*100
```

    ## [1] 1.184578

``` r
(coef(multi8)[1,8]-coef(multi6)[1,9])/coef(multi6)[1,9]*100
```

    ## [1] -2.797465

``` r
(coef(multi8)[2,6]-coef(multi6)[2,7])/coef(multi6)[2,7]*100
```

    ## [1] -27.95922

``` r
(coef(multi8)[2,7]-coef(multi6)[2,8])/coef(multi6)[2,8]*100
```

    ## [1] -2.267965

``` r
(coef(multi8)[2,8]-coef(multi6)[2,9])/coef(multi6)[2,9]*100
```

    ## [1] -20.51355

``` r
(coef(multi8)[3,6]-coef(multi6)[3,7])/coef(multi6)[3,7]*100
```

    ## [1] 19.52364

``` r
(coef(multi8)[3,7]-coef(multi6)[3,8])/coef(multi6)[3,8]*100
```

    ## [1] -0.2323055

``` r
(coef(multi8)[3,8]-coef(multi6)[3,9])/coef(multi6)[3,9]*100
```

    ## [1] -5.954459

``` r
# # test sexcat
# (coef(multi9)[1,6]-coef(multi6)[1,7])/coef(multi6)[1,7]*100
# (coef(multi9)[1,7]-coef(multi6)[1,8])/coef(multi6)[1,8]*100
# (coef(multi9)[1,8]-coef(multi6)[1,9])/coef(multi6)[1,9]*100
# 
# (coef(multi9)[2,6]-coef(multi6)[2,7])/coef(multi6)[2,7]*100
# (coef(multi9)[2,7]-coef(multi6)[2,8])/coef(multi6)[2,8]*100
# (coef(multi9)[2,8]-coef(multi6)[2,9])/coef(multi6)[2,9]*100
# 
# (coef(multi9)[3,6]-coef(multi6)[3,7])/coef(multi6)[3,7]*100
# (coef(multi9)[3,7]-coef(multi6)[3,8])/coef(multi6)[3,8]*100
# (coef(multi9)[3,8]-coef(multi6)[3,9])/coef(multi6)[3,9]*100
```

``` r
# test racecat - >10%
(coef(multi10)[1,6]-coef(multi6)[1,7])/coef(multi6)[1,7]*100
```

    ## [1] -8.767866

``` r
(coef(multi10)[1,7]-coef(multi6)[1,8])/coef(multi6)[1,8]*100
```

    ## [1] -62.72701

``` r
(coef(multi10)[1,8]-coef(multi6)[1,9])/coef(multi6)[1,9]*100
```

    ## [1] 8.19364

``` r
(coef(multi10)[2,6]-coef(multi6)[2,7])/coef(multi6)[2,7]*100
```

    ## [1] -28.90174

``` r
(coef(multi10)[2,7]-coef(multi6)[2,8])/coef(multi6)[2,8]*100
```

    ## [1] 18.13886

``` r
(coef(multi10)[2,8]-coef(multi6)[2,9])/coef(multi6)[2,9]*100
```

    ## [1] 8.188428

``` r
(coef(multi10)[3,6]-coef(multi6)[3,7])/coef(multi6)[3,7]*100
```

    ## [1] 6.565191

``` r
(coef(multi10)[3,7]-coef(multi6)[3,8])/coef(multi6)[3,8]*100
```

    ## [1] 2.097497

``` r
(coef(multi10)[3,8]-coef(multi6)[3,9])/coef(multi6)[3,9]*100
```

    ## [1] 1.113331

``` r
# test healthcat - >10%
(coef(multi11)[1,5]-coef(multi6)[1,7])/coef(multi6)[1,7]*100
```

    ## [1] -5.053899

``` r
(coef(multi11)[1,6]-coef(multi6)[1,8])/coef(multi6)[1,8]*100
```

    ## [1] 31.38894

``` r
(coef(multi11)[1,7]-coef(multi6)[1,9])/coef(multi6)[1,9]*100
```

    ## [1] 11.40103

``` r
(coef(multi11)[2,5]-coef(multi6)[2,7])/coef(multi6)[2,7]*100
```

    ## [1] -15.66345

``` r
(coef(multi11)[2,6]-coef(multi6)[2,8])/coef(multi6)[2,8]*100
```

    ## [1] -11.29726

``` r
(coef(multi11)[2,7]-coef(multi6)[2,9])/coef(multi6)[2,9]*100
```

    ## [1] 9.95335

``` r
(coef(multi11)[3,5]-coef(multi6)[3,7])/coef(multi6)[3,7]*100
```

    ## [1] -3.158703

``` r
(coef(multi11)[3,6]-coef(multi6)[3,8])/coef(multi6)[3,8]*100
```

    ## [1] 0.06605735

``` r
(coef(multi11)[3,7]-coef(multi6)[3,9])/coef(multi6)[3,9]*100
```

    ## [1] -1.260797

``` r
anova(multi2,multi1,test="Chisq")$`Pr(Chi)` # p>0.05
```

    ## [1]        NA 0.2376931

``` r
anova(multi3,multi2,test="Chisq")$`Pr(Chi)` # p>0.05
```

    ## [1]        NA 0.4835993

``` r
anova(multi4,multi2,test="Chisq")$`Pr(Chi)` # p<<0.05
```

    ## [1]           NA 3.682396e-06

``` r
anova(multi5,multi3,test="Chisq")$`Pr(Chi)` # p>0.05
```

    ## [1]        NA 0.6642791

``` r
anova(multi6,multi5,test="Chisq")$`Pr(Chi)` # p<<0.05
```

    ## [1]           NA 3.008774e-07

``` r
anova(multi7,multi5,test="Chisq")$`Pr(Chi)` # p<<0.05
```

    ## [1]           NA 2.551386e-05

``` r
anova(multi9,multi5,test="Chisq")$`Pr(Chi)` # p<<0.05
```

    ## [1]           NA 4.615877e-06

``` r
# multi5 based on LRT
```

``` r
# no significant interaction

multi6_r <- multinom(depress ~ AGE + sexcat + racecat:sympt + healthcat, data = data_multi)
multi6_h <- multinom(depress ~ AGE + sexcat + racecat + healthcat:sympt, data = data_multi)

anova(multi6,multi6_h,test="Chisq")$`Pr(Chi)` # p>0.05
anova(multi6,multi6_r,test="Chisq")$`Pr(Chi)` # p>0.05
```

``` r
data.frame(MODEL=c("multi1","multi2","multi3","multi4","multi5","multi6","multi7"),
           AIC=c(AIC(multi1),AIC(multi2),AIC(multi3),AIC(multi4),AIC(multi5),AIC(multi6),AIC(multi7)),
           BIC=c(BIC(multi1),BIC(multi2),BIC(multi3),BIC(multi4),BIC(multi5),BIC(multi6),BIC(multi7)))
```

    ##    MODEL      AIC       BIC
    ## 1 multi1 861.1691 1064.3498
    ## 2 multi2 849.6609  993.0826
    ## 3 multi3 846.1152  977.5851
    ## 4 multi4 873.0132  992.5313
    ## 5 multi5 841.6937  961.2118
    ## 6 multi6 868.8338  976.4001
    ## 7 multi7 860.7692  944.4319

``` r
# multi5, multi3 based on AIC
# multi7, multi5 based on BIC
```

``` r
logitgof(data_multi$depress, fitted(multi1))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi1)
    ## X-squared = 22.277, df = 24, p-value = 0.5627

``` r
logitgof(data_multi$depress, fitted(multi2))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi2)
    ## X-squared = 31.245, df = 24, p-value = 0.1468

``` r
logitgof(data_multi$depress, fitted(multi3))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi3)
    ## X-squared = 30.27, df = 24, p-value = 0.176

``` r
logitgof(data_multi$depress, fitted(multi4))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi4)
    ## X-squared = 33.341, df = 24, p-value = 0.09707

``` r
logitgof(data_multi$depress, fitted(multi5))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi5)
    ## X-squared = 15.593, df = 24, p-value = 0.9022

``` r
logitgof(data_multi$depress, fitted(multi6))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi6)
    ## X-squared = 32.075, df = 24, p-value = 0.1251

``` r
logitgof(data_multi$depress, fitted(multi7))
```

    ## 
    ##  Hosmer and Lemeshow test (multinomial model)
    ## 
    ## data:  data_multi$depress, fitted(multi7)
    ## X-squared = 17.677, df = 24, p-value = 0.8184

``` r
# all p>0.05
```

``` r
# options(scipen = 999)
tmulti1 = tidy(multi1)
tmulti2 = tidy(multi2)
tmulti3 = tidy(multi3)
tmulti4 = tidy(multi4)
tmulti5 = tidy(multi5)
tmulti6 = tidy(multi6)
tmulti7 = tidy(multi7)

# tmulti1$term[tmulti1$p.value < 0.05]
# tmulti2$term[tmulti2$p.value < 0.05]
# tmulti3$term[tmulti3$p.value < 0.05]
# tmulti4$term[tmulti4$p.value < 0.05]
# tmulti5$term[tmulti5$p.value < 0.05]
# tmulti7$term[tmulti7$p.value < 0.05]
tmulti6$term[tmulti6$p.value < 0.05]
```

    ## [1] "(Intercept)"            "healthcatfair"          "healthcatfair"         
    ## [4] "healthcatpoor"          "symptmoderate symptoms" "symptsevere symptoms"

``` r
# multi1 based on number of significant covariates
```

``` r
# fitted probs close to actual probs

cbind(fitted(multi5)[data_multi$sympt == "no symptoms",][1,],fitted(multi5)[data_multi$sympt == "mild symptoms",][1,],fitted(multi5)[data_multi$sympt == "moderate symptoms",][1,],fitted(multi5)[data_multi$sympt == "severe symptoms",][1,])
```

    ##                          [,1]        [,2]      [,3]       [,4]
    ## never             0.694660282 0.693025616 0.3257930 0.52022777
    ## daily             0.007528724 0.001008465 0.0209333 0.01706802
    ## weekly or monthly 0.107161088 0.062538355 0.2085481 0.09388564
    ## yearly            0.190649906 0.243427563 0.4447256 0.36881856

``` r
prop.table(table(data_multi$depress,data_multi$sympt),2)
```

    ##                    
    ##                     no symptoms mild symptoms moderate symptoms severe symptoms
    ##   never             0.641509434   0.629629630       0.459259259     0.445945946
    ##   daily             0.037735849   0.007407407       0.022222222     0.094594595
    ##   weekly or monthly 0.150943396   0.133333333       0.170370370     0.162162162
    ##   yearly            0.169811321   0.229629630       0.348148148     0.297297297

``` r
# never
plot(na.omit(data_multi$sympt), fitted(multi5)[,1])
```

![](README_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
# daily
plot(na.omit(data_multi$sympt), fitted(multi5)[,2], main="Figure 1. Predicted Probabilities of Feeling Depressed Daily", xlab="COVID-19 Symptom", ylab="Probability")
```

![](README_files/figure-gfm/unnamed-chunk-44-2.png)<!-- -->

``` r
# weekly or monthly
plot(na.omit(data_multi$sympt), fitted(multi5)[,3])
```

![](README_files/figure-gfm/unnamed-chunk-44-3.png)<!-- -->

``` r
# yearly
plot(na.omit(data_multi$sympt), fitted(multi5)[,4])
```

![](README_files/figure-gfm/unnamed-chunk-44-4.png)<!-- -->

``` r
# proportional assumption does not hold

data_multi$depressdaily <- ifelse(data_multi$depress=="daily", 1, 0)
data_multi$depressnever <- ifelse(data_multi$depress=="never", 1, 0)
moddaily <- glm(depressdaily ~ ., family=binomial, data=data_multi %>% dplyr::select(-depress, -depressnever)) 
summary(moddaily)
modnever <- glm(depressnever ~ ., family=binomial, data=data_multi %>% dplyr::select(-depress, -depressdaily)) 
summary(modnever)
coef(moddaily)[10:12]
coef(moddaily)[10] + c(-1, 1)*1.96*sqrt(vcov(moddaily)[10,10])
coef(moddaily)[11] + c(-1, 1)*1.96*sqrt(vcov(moddaily)[11,11])
coef(moddaily)[12] + c(-1, 1)*1.96*sqrt(vcov(moddaily)[12,12])
coef(modnever)[10:12]
coef(modnever)[10] + c(-1, 1)*1.96*sqrt(vcov(modnever)[10,10])
coef(modnever)[11] + c(-1, 1)*1.96*sqrt(vcov(modnever)[11,11])
coef(modnever)[12] + c(-1, 1)*1.96*sqrt(vcov(modnever)[12,12])
```

# Kenny???s EDA

``` r
# univariate
hist(data$MOD10DMIN)
```

![](README_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
# tally(~data$MOD10FWK)

hist(log(data$MOD10DMIN))
```

![](README_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->

``` r
hist(data$AGE)
```

![](README_files/figure-gfm/unnamed-chunk-46-3.png)<!-- -->

``` r
barplot(prop.table(table(data$sympt)))
```

![](README_files/figure-gfm/unnamed-chunk-46-4.png)<!-- -->

``` r
barplot(prop.table(table(data$sexcat)))
```

![](README_files/figure-gfm/unnamed-chunk-46-5.png)<!-- -->

``` r
barplot(prop.table(table(data$racecat)))
```

![](README_files/figure-gfm/unnamed-chunk-46-6.png)<!-- -->

``` r
barplot(prop.table(table(data$incomecat)))
```

![](README_files/figure-gfm/unnamed-chunk-46-7.png)<!-- -->

``` r
barplot(prop.table(table(data$healthcat)))
```

![](README_files/figure-gfm/unnamed-chunk-46-8.png)<!-- -->

``` r
ggplot(data, aes(sympt, MOD10DMIN)) +   
  geom_boxplot() +
  labs(x = "Severity of COVID-19 Symptoms", y = "Duration of Moderate Physical Activity")
```

    ## Don't know how to automatically pick scale for object of type
    ## <haven_labelled/vctrs_vctr/double>. Defaulting to continuous.

![](README_files/figure-gfm/unnamed-chunk-46-9.png)<!-- -->

We see that the duration of moderate physical activity is heavily right
skewed so we log transformed it.

``` r
#bivariate
scatter.smooth(data$AGE, log(data$MOD10DMIN))
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
scatter.smooth(data$AGE, (data$MOD10DMIN))
```

![](README_files/figure-gfm/unnamed-chunk-47-2.png)<!-- -->

``` r
boxplot(data$MOD10DMIN ~ data$sympt)
```

![](README_files/figure-gfm/unnamed-chunk-47-3.png)<!-- -->

``` r
boxplot(data$MOD10DMIN ~ data$racecat)
```

![](README_files/figure-gfm/unnamed-chunk-47-4.png)<!-- -->

``` r
boxplot(data$MOD10DMIN ~ data$incomecat)
```

![](README_files/figure-gfm/unnamed-chunk-47-5.png)<!-- -->

``` r
boxplot(data$MOD10DMIN ~ data$healthcat)
```

![](README_files/figure-gfm/unnamed-chunk-47-6.png)<!-- -->

# Fitting the full model

``` r
lm1 <- lm(log(MOD10DMIN) ~ sexcat + racecat  + healthcat + sympt + AGE
      ,data = data)

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8528 -0.4554  0.0505  0.2810  2.1917 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.978605   0.144534  27.527   <2e-16 ***
    ## sexcatmale              0.041469   0.069951   0.593   0.5536    
    ## racecatwhite            0.002340   0.094338   0.025   0.9802    
    ## healthcatfair          -0.146695   0.125329  -1.170   0.2425    
    ## healthcatpoor          -0.145437   0.401778  -0.362   0.7176    
    ## symptmild symptoms      0.029941   0.113782   0.263   0.7926    
    ## symptmoderate symptoms  0.048412   0.114314   0.423   0.6722    
    ## symptsevere symptoms    0.086427   0.125649   0.688   0.4920    
    ## AGE                    -0.003857   0.002139  -1.803   0.0721 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6916 on 388 degrees of freedom
    ## Multiple R-squared:  0.01445,    Adjusted R-squared:  -0.005874 
    ## F-statistic: 0.7109 on 8 and 388 DF,  p-value: 0.6819

In the full model, we see that after adjusting for the effect of the
other variables, none of the COVID symptom levels have a statistically
significant effect on the duration of moderate physical activity.

# Effect modifier

``` r
# check if anxiety, depression, region or emotional support are effect modifiers of covid symptoms
lm_anxiety <- lm(log(MOD10DMIN) ~ sexcat + racecat  + healthcat + sympt + AGE
          + anxiety*sympt,data = data)
lm_depress <- lm(log(MOD10DMIN) ~ sexcat + racecat  + healthcat + sympt + AGE
          + depress*sympt,data = data)
lm_reg <- lm(log(MOD10DMIN) ~ sexcat + racecat  + healthcat + sympt + AGE
          + reg*sympt,data = data)
lm_esupport <- lm(log(MOD10DMIN) ~ sexcat + racecat + incomecat + healthcat + sympt + AGE
          + esupport*sympt,data = data)
lm_income <- lm(log(MOD10DMIN) ~ sexcat + racecat  + healthcat + sympt + AGE
          + incomecat*sympt,data = data)

summary(lm_anxiety)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE + anxiety * sympt, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8812 -0.4445  0.0583  0.3002  2.1915 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        3.972611   0.153746  25.839   <2e-16 ***
    ## sexcatmale                         0.057663   0.070520   0.818   0.4140    
    ## racecatwhite                       0.004267   0.094779   0.045   0.9641    
    ## healthcatfair                     -0.155687   0.126800  -1.228   0.2203    
    ## healthcatpoor                     -0.083205   0.406876  -0.204   0.8381    
    ## symptmild symptoms                 0.074633   0.124145   0.601   0.5481    
    ## symptmoderate symptoms             0.043322   0.125116   0.346   0.7293    
    ## symptsevere symptoms               0.033188   0.138890   0.239   0.8113    
    ## AGE                               -0.004154   0.002155  -1.928   0.0546 .  
    ## anxietyYes                         0.070098   0.254278   0.276   0.7829    
    ## symptmild symptoms:anxietyYes     -0.298046   0.303079  -0.983   0.3260    
    ## symptmoderate symptoms:anxietyYes  0.021492   0.298784   0.072   0.9427    
    ## symptsevere symptoms:anxietyYes    0.232228   0.322205   0.721   0.4715    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6911 on 384 degrees of freedom
    ## Multiple R-squared:  0.02624,    Adjusted R-squared:  -0.004189 
    ## F-statistic: 0.8623 on 12 and 384 DF,  p-value: 0.5859

``` r
summary(lm_depress)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE + depress * sympt, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8764 -0.4473  0.0463  0.2900  2.1166 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error t value
    ## (Intercept)                                      3.9491175  0.1676639  23.554
    ## sexcatmale                                       0.0501135  0.0712170   0.704
    ## racecatwhite                                     0.0030680  0.0961915   0.032
    ## healthcatfair                                   -0.1207726  0.1318023  -0.916
    ## healthcatpoor                                   -0.2226472  0.4185025  -0.532
    ## symptmild symptoms                               0.0808117  0.1431916   0.564
    ## symptmoderate symptoms                           0.0578911  0.1508354   0.384
    ## symptsevere symptoms                             0.0604058  0.1713523   0.353
    ## AGE                                             -0.0038186  0.0021949  -1.740
    ## depressdaily                                    -0.0595444  0.5101473  -0.117
    ## depressweekly or monthly                         0.0884747  0.2766665   0.320
    ## depressyearly                                    0.0588445  0.2627964   0.224
    ## symptmild symptoms:depressdaily                 -0.5693604  0.8804734  -0.647
    ## symptmoderate symptoms:depressdaily             -0.2135014  0.6588485  -0.324
    ## symptsevere symptoms:depressdaily                0.1493392  0.5915413   0.252
    ## symptmild symptoms:depressweekly or monthly     -0.1894885  0.3313771  -0.572
    ## symptmoderate symptoms:depressweekly or monthly  0.0914607  0.3254830   0.281
    ## symptsevere symptoms:depressweekly or monthly    0.0007314  0.3647894   0.002
    ## symptmild symptoms:depressyearly                -0.1096991  0.3011617  -0.364
    ## symptmoderate symptoms:depressyearly            -0.0908555  0.2954563  -0.308
    ## symptsevere symptoms:depressyearly               0.0202232  0.3255820   0.062
    ##                                                 Pr(>|t|)    
    ## (Intercept)                                       <2e-16 ***
    ## sexcatmale                                        0.4821    
    ## racecatwhite                                      0.9746    
    ## healthcatfair                                     0.3601    
    ## healthcatpoor                                     0.5950    
    ## symptmild symptoms                                0.5728    
    ## symptmoderate symptoms                            0.7013    
    ## symptsevere symptoms                              0.7246    
    ## AGE                                               0.0827 .  
    ## depressdaily                                      0.9071    
    ## depressweekly or monthly                          0.7493    
    ## depressyearly                                     0.8229    
    ## symptmild symptoms:depressdaily                   0.5183    
    ## symptmoderate symptoms:depressdaily               0.7461    
    ## symptsevere symptoms:depressdaily                 0.8008    
    ## symptmild symptoms:depressweekly or monthly       0.5678    
    ## symptmoderate symptoms:depressweekly or monthly   0.7789    
    ## symptsevere symptoms:depressweekly or monthly     0.9984    
    ## symptmild symptoms:depressyearly                  0.7159    
    ## symptmoderate symptoms:depressyearly              0.7586    
    ## symptsevere symptoms:depressyearly                0.9505    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6994 on 376 degrees of freedom
    ## Multiple R-squared:  0.02342,    Adjusted R-squared:  -0.02853 
    ## F-statistic: 0.4509 on 20 and 376 DF,  p-value: 0.9814

``` r
summary(lm_reg)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE + reg * sympt, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8177 -0.4126  0.0491  0.3553  2.0592 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        3.878802   0.284510  13.633   <2e-16 ***
    ## sexcatmale                         0.040986   0.070736   0.579   0.5626    
    ## racecatwhite                      -0.024825   0.095948  -0.259   0.7960    
    ## healthcatfair                     -0.167357   0.128415  -1.303   0.1933    
    ## healthcatpoor                     -0.227152   0.404783  -0.561   0.5750    
    ## symptmild symptoms                 0.232942   0.304983   0.764   0.4455    
    ## symptmoderate symptoms             0.040161   0.297141   0.135   0.8926    
    ## symptsevere symptoms               0.029269   0.320435   0.091   0.9273    
    ## AGE                               -0.003977   0.002138  -1.860   0.0636 .  
    ## regmidwest                        -0.151726   0.328382  -0.462   0.6443    
    ## regsouth                           0.243727   0.295914   0.824   0.4107    
    ## regwest                            0.263354   0.349175   0.754   0.4512    
    ## symptmild symptoms:regmidwest     -0.055524   0.382486  -0.145   0.8847    
    ## symptmoderate symptoms:regmidwest  0.422711   0.375939   1.124   0.2616    
    ## symptsevere symptoms:regmidwest    0.492339   0.408450   1.205   0.2288    
    ## symptmild symptoms:regsouth       -0.349839   0.346566  -1.009   0.3134    
    ## symptmoderate symptoms:regsouth   -0.212598   0.344484  -0.617   0.5375    
    ## symptsevere symptoms:regsouth     -0.172523   0.374705  -0.460   0.6455    
    ## symptmild symptoms:regwest        -0.091021   0.410692  -0.222   0.8247    
    ## symptmoderate symptoms:regwest    -0.025918   0.393226  -0.066   0.9475    
    ## symptsevere symptoms:regwest       0.095360   0.434055   0.220   0.8262    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6889 on 376 degrees of freedom
    ## Multiple R-squared:  0.05238,    Adjusted R-squared:  0.001974 
    ## F-statistic: 1.039 on 20 and 376 DF,  p-value: 0.4144

``` r
summary(lm_esupport)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + incomecat + 
    ##     healthcat + sympt + AGE + esupport * sympt, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8016 -0.4548  0.0530  0.3139  2.0800 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error t value
    ## (Intercept)                                     3.9078584  0.1744173  22.405
    ## sexcatmale                                      0.0266978  0.0707025   0.378
    ## racecatwhite                                   -0.0007515  0.0970387  -0.008
    ## incomecat$50,000-$99,999                        0.1718725  0.0941033   1.826
    ## incomecatover $100,000                          0.1001521  0.0866362   1.156
    ## healthcatfair                                  -0.1258672  0.1285763  -0.979
    ## healthcatpoor                                  -0.0176870  0.4102631  -0.043
    ## symptmild symptoms                              0.0539908  0.1478112   0.365
    ## symptmoderate symptoms                          0.0816966  0.1513619   0.540
    ## symptsevere symptoms                            0.1052557  0.1645169   0.640
    ## AGE                                            -0.0040674  0.0021761  -1.869
    ## esupportusually                                -0.0889611  0.2381717  -0.374
    ## esupportsometimes-never                         0.0995022  0.2650734   0.375
    ## symptmild symptoms:esupportusually              0.0611668  0.2818722   0.217
    ## symptmoderate symptoms:esupportusually          0.0284198  0.2741594   0.104
    ## symptsevere symptoms:esupportusually           -0.0916738  0.3120133  -0.294
    ## symptmild symptoms:esupportsometimes-never     -0.1923464  0.3074942  -0.626
    ## symptmoderate symptoms:esupportsometimes-never -0.2132703  0.3119431  -0.684
    ## symptsevere symptoms:esupportsometimes-never    0.0748275  0.3463074   0.216
    ##                                                Pr(>|t|)    
    ## (Intercept)                                      <2e-16 ***
    ## sexcatmale                                       0.7059    
    ## racecatwhite                                     0.9938    
    ## incomecat$50,000-$99,999                         0.0686 .  
    ## incomecatover $100,000                           0.2484    
    ## healthcatfair                                    0.3282    
    ## healthcatpoor                                    0.9656    
    ## symptmild symptoms                               0.7151    
    ## symptmoderate symptoms                           0.5897    
    ## symptsevere symptoms                             0.5227    
    ## AGE                                              0.0624 .  
    ## esupportusually                                  0.7090    
    ## esupportsometimes-never                          0.7076    
    ## symptmild symptoms:esupportusually               0.8283    
    ## symptmoderate symptoms:esupportusually           0.9175    
    ## symptsevere symptoms:esupportusually             0.7691    
    ## symptmild symptoms:esupportsometimes-never       0.5320    
    ## symptmoderate symptoms:esupportsometimes-never   0.4946    
    ## symptsevere symptoms:esupportsometimes-never     0.8290    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6947 on 378 degrees of freedom
    ## Multiple R-squared:  0.03148,    Adjusted R-squared:  -0.01464 
    ## F-statistic: 0.6826 on 18 and 378 DF,  p-value: 0.8289

``` r
summary(lm_income)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE + incomecat * sympt, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8812 -0.4472  0.0349  0.2832  2.1789 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error t value
    ## (Intercept)                                      4.010641   0.190718  21.029
    ## sexcatmale                                       0.032295   0.070460   0.458
    ## racecatwhite                                    -0.015513   0.095410  -0.163
    ## healthcatfair                                   -0.120835   0.127075  -0.951
    ## healthcatpoor                                   -0.109382   0.406688  -0.269
    ## symptmild symptoms                              -0.052416   0.193460  -0.271
    ## symptmoderate symptoms                          -0.003188   0.200095  -0.016
    ## symptsevere symptoms                            -0.155890   0.204980  -0.761
    ## AGE                                             -0.003931   0.002161  -1.819
    ## incomecat$50,000-$99,999                         0.034442   0.225324   0.153
    ## incomecatover $100,000                          -0.097886   0.239447  -0.409
    ## symptmild symptoms:incomecat$50,000-$99,999      0.068463   0.277153   0.247
    ## symptmoderate symptoms:incomecat$50,000-$99,999  0.026620   0.279468   0.095
    ## symptsevere symptoms:incomecat$50,000-$99,999    0.466837   0.296292   1.576
    ## symptmild symptoms:incomecatover $100,000        0.197551   0.277207   0.713
    ## symptmoderate symptoms:incomecatover $100,000    0.149603   0.281145   0.532
    ## symptsevere symptoms:incomecatover $100,000      0.342819   0.310989   1.102
    ##                                                 Pr(>|t|)    
    ## (Intercept)                                       <2e-16 ***
    ## sexcatmale                                        0.6470    
    ## racecatwhite                                      0.8709    
    ## healthcatfair                                     0.3423    
    ## healthcatpoor                                     0.7881    
    ## symptmild symptoms                                0.7866    
    ## symptmoderate symptoms                            0.9873    
    ## symptsevere symptoms                              0.4474    
    ## AGE                                               0.0697 .  
    ## incomecat$50,000-$99,999                          0.8786    
    ## incomecatover $100,000                            0.6829    
    ## symptmild symptoms:incomecat$50,000-$99,999       0.8050    
    ## symptmoderate symptoms:incomecat$50,000-$99,999   0.9242    
    ## symptsevere symptoms:incomecat$50,000-$99,999     0.1160    
    ## symptmild symptoms:incomecatover $100,000         0.4765    
    ## symptmoderate symptoms:incomecatover $100,000     0.5950    
    ## symptsevere symptoms:incomecatover $100,000       0.2710    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6919 on 380 degrees of freedom
    ## Multiple R-squared:  0.03412,    Adjusted R-squared:  -0.006547 
    ## F-statistic: 0.839 on 16 and 380 DF,  p-value: 0.6408

``` r
# None are effect modifiers so we will stick with
lm1 <- lm(log(MOD10DMIN) ~ sexcat + racecat + healthcat + sympt + AGE, data = data)

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8528 -0.4554  0.0505  0.2810  2.1917 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.978605   0.144534  27.527   <2e-16 ***
    ## sexcatmale              0.041469   0.069951   0.593   0.5536    
    ## racecatwhite            0.002340   0.094338   0.025   0.9802    
    ## healthcatfair          -0.146695   0.125329  -1.170   0.2425    
    ## healthcatpoor          -0.145437   0.401778  -0.362   0.7176    
    ## symptmild symptoms      0.029941   0.113782   0.263   0.7926    
    ## symptmoderate symptoms  0.048412   0.114314   0.423   0.6722    
    ## symptsevere symptoms    0.086427   0.125649   0.688   0.4920    
    ## AGE                    -0.003857   0.002139  -1.803   0.0721 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6916 on 388 degrees of freedom
    ## Multiple R-squared:  0.01445,    Adjusted R-squared:  -0.005874 
    ## F-statistic: 0.7109 on 8 and 388 DF,  p-value: 0.6819

# Diagnosis and Model Evaluation

``` r
plot(lm1, 1:6)
```

![](README_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-50-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-50-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-50-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-50-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-50-6.png)<!-- -->

``` r
hist(lm1$residuals)
```

![](README_files/figure-gfm/unnamed-chunk-50-7.png)<!-- -->

``` r
# 265, 46, 383

# 12/397
# 0.0302267

# modelling dropping those 
lm1_outliers <- lm(log(MOD10DMIN) ~ sexcat + racecat + healthcat + sympt + AGE, data = 
data[-c(46,265, 383),])

summary(lm1_outliers)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE, data = data[-c(46, 265, 383), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8705 -0.4665  0.0849  0.2656  2.2153 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.911275   0.141298  27.681   <2e-16 ***
    ## sexcatmale              0.064804   0.068355   0.948    0.344    
    ## racecatwhite            0.002294   0.091773   0.025    0.980    
    ## healthcatfair          -0.165610   0.121973  -1.358    0.175    
    ## healthcatpoor          -0.468764   0.678626  -0.691    0.490    
    ## symptmild symptoms      0.061304   0.110903   0.553    0.581    
    ## symptmoderate symptoms  0.038661   0.111278   0.347    0.728    
    ## symptsevere symptoms    0.083618   0.122466   0.683    0.495    
    ## AGE                    -0.002545   0.002098  -1.213    0.226    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6728 on 385 degrees of freedom
    ## Multiple R-squared:  0.01381,    Adjusted R-squared:  -0.006679 
    ## F-statistic: 0.6741 on 8 and 385 DF,  p-value: 0.7144

``` r
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = log(MOD10DMIN) ~ sexcat + racecat + healthcat + 
    ##     sympt + AGE, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8528 -0.4554  0.0505  0.2810  2.1917 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.978605   0.144534  27.527   <2e-16 ***
    ## sexcatmale              0.041469   0.069951   0.593   0.5536    
    ## racecatwhite            0.002340   0.094338   0.025   0.9802    
    ## healthcatfair          -0.146695   0.125329  -1.170   0.2425    
    ## healthcatpoor          -0.145437   0.401778  -0.362   0.7176    
    ## symptmild symptoms      0.029941   0.113782   0.263   0.7926    
    ## symptmoderate symptoms  0.048412   0.114314   0.423   0.6722    
    ## symptsevere symptoms    0.086427   0.125649   0.688   0.4920    
    ## AGE                    -0.003857   0.002139  -1.803   0.0721 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6916 on 388 degrees of freedom
    ## Multiple R-squared:  0.01445,    Adjusted R-squared:  -0.005874 
    ## F-statistic: 0.7109 on 8 and 388 DF,  p-value: 0.6819

``` r
# none are above 5 :)
VIF(lm1) 
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## sexcat    1.007309  1        1.003648
    ## racecat   1.072669  1        1.035697
    ## healthcat 1.024043  2        1.005957
    ## sympt     1.094357  3        1.015141
    ## AGE       1.069634  1        1.034231

From the residual plots, it seems linearity, normality, and equal
variance holds.
