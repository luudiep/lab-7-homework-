summary(Household_Pulse_data)
Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 
sum(is.na(Household_Pulse_data$RECVDVACC))
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
summary(Household_Pulse_data$vaxx)

use_varb <- (Household_Pulse_data$TBIRTH_YEAR >= 1950) & (Household_Pulse_data$TBIRTH_YEAR <= 1990)
dat_use1 <- subset(Household_Pulse_data,use_varb) 
vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE + REGION
d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)
#I chose the subset of people between the ages 31 and 71
  
d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))

d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx)) 

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  dat_use1$TBIRTH_YEAR[!is.na(dat_use1$vaxx)],
  d_educ[!is.na(dat_use1$vaxx),2:7],
  d_marstat[!is.na(dat_use1$vaxx),2:6],
  d_race[!is.na(dat_use1$vaxx),2:4],
  d_hispanic[!is.na(dat_use1$vaxx),2],
  d_gender[!is.na(dat_use1$vaxx),2:5],
  d_region[!is.na(dat_use1$vaxx),2:4])

names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
names(dat_for_analysis_sub)[17] <- "Hispanic"

require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.1) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(vaxx ~ TBIRTH_YEAR + EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest
                    , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)

#linear
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)

#logit
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)

#when looking at the different types of models and their outputs
#the logit model gave values that were higher than the linear model

#attempting using different values of pred_vals to see different results
pred_model_logit1 <- (pred_vals > 0.3)
table(pred = pred_model_logit1, true = dat_test$vaxx)

pred_model_logit1 <- (pred_vals > 0.9)
table(pred = pred_model_logit1, true = dat_test$vaxx)

pred_model_logit1 <- (pred_vals > 0.6)
table(pred = pred_model_logit1, true = dat_test$vaxx)
#the higher value of the pred_vals the more numbers are shown for the given info

#while doing my code, figured out why EEDUC was not showing
#after redoing the different vaxx coding, it finally worked
#not sure what the warnings meant, got it twice for the two models i was trying to run

> summary(Household_Pulse_data)
RHISPANIC       RRACE                EEDUC               MS        EGENID_BIRTH  
Not Hispanic:62660   White:56938   less than hs:  411   NA       :  881   male  :27592  
Hispanic    : 6454   Black: 5412   some hs     :  936   married  :40036   female:41522  
Asian: 3561   HS diploma  : 7857   widowed  : 3872                 
Other: 3203   some coll   :14596   divorced :10310                 
assoc deg   : 7508   separated: 1214                 
bach deg    :20075   never    :12801                 
adv deg     :17731                                   
GENID_DESCRIBE       SEXUAL_ORIENTATION                      KIDS_LT5Y    
NA         : 1131   NA            : 1506    NA                        :62342  
male       :26796   gay or lesbian: 2343    Yes children under 5 in HH: 6772  
female     :40263   straight      :61238                                      
transgender:  202   bisexual      : 2288                                      
other      :  722   something else:  871                                      
dont know     :  868                                      

KIDS_5_11Y                        KIDS_12_17Y   
NA                       :58467   NA                        :58046  
Yes children 5 - 11 in HH:10647   Yes children 12 - 17 in HH:11068  





ENROLLNONE                  RECVDVACC    
NA                                :64285   NA                 :  851  
children not in any type of school: 4829   yes got vaxx       :60326  
no did not get vaxx: 7937  




DOSESRV                          GETVACRV    
NA                       : 9105   NA                      :61159  
yes got all doses        :57762   definitely will get vaxx:  609  
yes plan to get all doses: 1993   probably will get vaxx  :  731  
no will not get all doses:  254   unsure about vaxx       : 1584  
probably not            : 1599  
definitely not          : 3432  

KIDDOSES                        KIDGETVAC    
NA                                :58318   NA                      :65384  
Yes kids got or will get all doses: 7135   definitely will get vaxx:  487  
no kids did not or will not       : 3661   probably will get vaxx  :  439  
unsure about vaxx       :  736  
probably not            :  593  
definitely not          : 1036  
dont know yet           :  439  
HADCOVID                      WRKLOSSRV    
NA                       : 1363   NA                    : 1961  
yes doctor told had covid: 9122   yes recent HH job loss: 8058  
no did not               :58221   no recent HH job loss :59095  
not sure                 :  408                                 



ANYWORK                     KINDWORK                RSNNOWRKRV   
NA                           : 2135   NA                 :30540   NA              :42659  
yes employment in last 7 days:39237   work for govt      : 6378   retired         :15024  
no employment in last 7 days :27742   work for private co:21370   other           : 4027  
work for nonprofit : 5055   sick or disabled: 1451  
self employed      : 4966   caring for kids : 1329  
work in family biz :  805   laid off        : 1164  
(Other)         : 3460  
CHLDCARE                              CURFOODSUF   
NA                                       :58419   NA                            : 6770  
yes impacts to childcare because pandemic: 2566   had enough food               :49234  
no                                       : 8129   had enough but not what wanted: 9947  
sometimes not enough food     : 2486  
often not enough food         :  677  


CHILDFOOD    
NA                                                 :64258  
often kids not eating enough because couldnt afford:  271  
sometimes kids not eating enough                   : 1191  
kids got enough food                               : 3394  



ANXIOUS     
NA                                             : 7946  
no anxiety over past 2 wks                     :26611  
several days anxiety over past 2 wks           :19794  
more than half the days anxiety over past 2 wks: 6140  
nearly every day anxiety                       : 8623  


WORRY      
NA                                             : 8016  
no worry over past 2 wks                       :31876  
several days worried over past 2 wks           :17936  
more than half the days worried over past 2 wks: 4979  
nearly every day worry                         : 6307  


TENURE                                     LIVQTRRV    
NA                           :11103   live in detached 1 family          :41348  
housing owned free and clear :16738   NA                                 :11336  
housing owned with mortgage  :28016   live in bldg w 5+ apts             : 6731  
housing rented               :12579   live in 1 family attached to others: 4628  
housing occupied without rent:  678   live in mobile home                : 1781  
live in building with 3-4 apts     : 1737  
(Other)                            : 1553  
RENTCUR                     MORTCUR     
NA             :56572   NA                 :41200  
current on rent:11239   current on mortgage:26462  
behind on rent : 1303   behind on mortgage : 1452  




EVICT      
NA                                        :67859  
very likely evicted in next 2 months      :  207  
somewhat likely evicted in next 2 months  :  377  
not very likely evicted in next 2 months  :  345  
not at all likely evicted in next 2 months:  326  


FORCLOSE               EST_ST     
NA                                           :67695   California   : 5359  
very likely forclosed in next 2 months       :   65   Texas        : 3766  
somewhat likely forclosed in next 2 months   :  218   Florida      : 2728  
not very likely forclosed in next 2 months   :  474   Washington   : 2634  
not at all forclosed evicted in next 2 months:  662   Massachusetts: 1965  
Oregon       : 1934  
(Other)      :50728  
PRIVHLTH                      PUBHLTH            REGION     
has private health ins:46869   has public health ins:23346   Northeast:10478  
no private health ins :11275   no public health ins :33381   South    :22680  
NA                    :10970   NA                   :12387   Midwest  :13651  
West     :22305  



INCOME       TBIRTH_YEAR   Num_kids_Pub_School Num_kids_Priv_School
NA                   :14637   Min.   :1933   Min.   :0.00        Min.   :0.00        
HH income $100k - 149:10117   1st Qu.:1955   1st Qu.:1.00        1st Qu.:0.00        
HH income $50k - 74.9: 9330   Median :1967   Median :2.00        Median :1.00        
HH income $75 - 99.9 : 7830   Mean   :1968   Mean   :1.71        Mean   :1.03        
HH income $200k +    : 6117   3rd Qu.:1981   3rd Qu.:2.00        3rd Qu.:2.00        
HH income $35k - 49.9: 5805   Max.   :2003   Max.   :4.00        Max.   :2.00        
(Other)              :15278                  NA's   :55108       NA's   :66430       
Num_kids_homeschool        Works_onsite            works_remote            Shop_in_store  
Min.   :0.00        NA           : 6350   NA             : 8022   NA              : 6873  
1st Qu.:0.00        worked onsite:34918   worked remotely:22863   shopped in store:53576  
Median :1.00        no           :27846   no             :38229   no              : 8665  
Mean   :0.87                                                                              
3rd Qu.:2.00                                                                              
Max.   :2.00                                                                              
NA's   :67421                                                                             
                 eat_in_restaurant    vaxx        
 NA                       : 7217   Mode :logical  
 eat at restaurant indoors:32405   FALSE:7937     
 no                       :29492   TRUE :60326    
                                   NA's :851      



> Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
> is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 
> sum(is.na(Household_Pulse_data$RECVDVACC))
[1] 0
> table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

less than hs some hs HS diploma some coll assoc deg bach deg adv deg
FALSE          115     269       1647      2396      1132     1565     813
TRUE           290     652       6097     12022      6266    18272   16727
> summary(Household_Pulse_data$vaxx)
Mode   FALSE    TRUE    NA's 
logical    7937   60326     851 
> 
> use_varb <- (Household_Pulse_data$TBIRTH_YEAR >= 1950) & (Household_Pulse_data$TBIRTH_YEAR <= 1990)
> dat_use1 <- subset(Household_Pulse_data,use_varb) 
> vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE + REGION
vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE + 
    REGION
> d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
> summary(d_educ)
  X.Intercept. dat_use1.EEDUCsome.hs dat_use1.EEDUCHS.diploma dat_use1.EEDUCsome.coll
 Min.   :1     Min.   :0.00000       Min.   :0.0000           Min.   :0.000          
 1st Qu.:1     1st Qu.:0.00000       1st Qu.:0.0000           1st Qu.:0.000          
 Median :1     Median :0.00000       Median :0.0000           Median :0.000          
 Mean   :1     Mean   :0.01339       Mean   :0.1115           Mean   :0.201          
 3rd Qu.:1     3rd Qu.:0.00000       3rd Qu.:0.0000           3rd Qu.:0.000          
 Max.   :1     Max.   :1.00000       Max.   :1.0000           Max.   :1.000          
 dat_use1.EEDUCassoc.deg dat_use1.EEDUCbach.deg dat_use1.EEDUCadv.deg
 Min.   :0.0000          Min.   :0.0000         Min.   :0.0000       
 1st Qu.:0.0000          1st Qu.:0.0000         1st Qu.:0.0000       
 Median :0.0000          Median :0.0000         Median :0.0000       
 Mean   :0.1153          Mean   :0.2918         Mean   :0.2607       
 3rd Qu.:0.0000          3rd Qu.:1.0000         3rd Qu.:1.0000       
 Max.   :1.0000          Max.   :1.0000         Max.   :1.0000       
> levels(dat_use1$EEDUC)
[1] "less than hs" "some hs"      "HS diploma"   "some coll"    "assoc deg"    "bach deg"    
[7] "adv deg"     
> #I chose the subset of people between the ages 31 and 71
>   
> d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
> d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
> d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
> d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
> d_region <- data.frame(model.matrix(~ dat_use1$REGION))
> 
> d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx)) 
> 
> dat_for_analysis_sub <- data.frame(
+   d_vaxx[,2],
+   dat_use1$TBIRTH_YEAR[!is.na(dat_use1$vaxx)],
+   d_educ[!is.na(dat_use1$vaxx),2:7],
+   d_marstat[!is.na(dat_use1$vaxx),2:6],
+   d_race[!is.na(dat_use1$vaxx),2:4],
+   d_hispanic[!is.na(dat_use1$vaxx),2],
+   d_gender[!is.na(dat_use1$vaxx),2:5],
+   d_region[!is.na(dat_use1$vaxx),2:4])
> 
> names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
> names(dat_for_analysis_sub)[1] <- "vaxx"
> names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
> names(dat_for_analysis_sub)[17] <- "Hispanic"
> 
> require("standardize")
> set.seed(654321)
> NN <- length(dat_for_analysis_sub$vaxx)
> restrict_1 <- (runif(NN) < 0.1) 
> summary(restrict_1)
   Mode   FALSE    TRUE 
logical   47750    5371 
> dat_train <- subset(dat_for_analysis_sub, restrict_1)
> dat_test <- subset(dat_for_analysis_sub, !restrict_1)
> 
> sobj <- standardize(vaxx ~ TBIRTH_YEAR + EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
+                       MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
+                       Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
+                       REGIONSouth + REGIONMidwest + REGIONWest
+                     , dat_train, family = binomial)
> 
> s_dat_test <- predict(sobj, dat_test)
> 
> #linear
> model_lpm1 <- lm(sobj$formula, data = sobj$data)
> summary(model_lpm1)

Call:
lm(formula = sobj$formula, data = sobj$data)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.08443  0.01386  0.09487  0.16111  0.45401 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 1.153239   0.149317   7.723 1.34e-14 ***
TBIRTH_YEAR                -0.056369   0.004554 -12.378  < 2e-16 ***
EEDUCsome.hs1               0.021531   0.032741   0.658 0.510808    
EEDUCHS.diploma1            0.035440   0.028359   1.250 0.211469    
EEDUCsome.coll1             0.051776   0.028079   1.844 0.065253 .  
EEDUCassoc.deg1             0.072440   0.028491   2.543 0.011031 *  
EEDUCbach.deg1              0.111259   0.028001   3.973 7.18e-05 ***
EEDUCadv.deg1               0.115182   0.028030   4.109 4.03e-05 ***
MSmarried1                 -0.003004   0.025086  -0.120 0.904685    
MSwidowed1                 -0.005211   0.027401  -0.190 0.849190    
MSdivorced1                -0.020786   0.025494  -0.815 0.414901    
MSseparated1               -0.016109   0.029730  -0.542 0.587956    
MSnever1                   -0.004760   0.025616  -0.186 0.852575    
RRACEBlack1                 0.008785   0.008056   1.090 0.275555    
RRACEAsian1                 0.033715   0.009359   3.602 0.000318 ***
RRACEOther1                -0.042955   0.009932  -4.325 1.55e-05 ***
Hispanic1                   0.037560   0.007448   5.043 4.73e-07 ***
GENID_DESCRIBEmale1         0.024626   0.024623   1.000 0.317304    
GENID_DESCRIBEfemale1       0.024800   0.024563   1.010 0.312710    
GENID_DESCRIBEtransgender1  0.066553   0.048696   1.367 0.171780    
GENID_DESCRIBEother1        0.011386   0.032904   0.346 0.729324    
REGIONSouth1               -0.024143   0.006703  -3.602 0.000319 ***
REGIONMidwest1             -0.025228   0.007419  -3.400 0.000678 ***
REGIONWest1                -0.021182   0.006755  -3.136 0.001725 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3147 on 5347 degrees of freedom
Multiple R-squared:  0.07951,	Adjusted R-squared:  0.07555 
F-statistic: 20.08 on 23 and 5347 DF,  p-value: < 2.2e-16

> pred_vals_lpm <- predict(model_lpm1, s_dat_test)
There were 22 warnings (use warnings() to see them)
> pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
> table(pred = pred_model_lpm1, true = dat_test$vaxx)
       true
pred        0     1
  FALSE  4320 19200
  TRUE   1366 22864
> 
> #logit
> model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
> summary(model_logit1)

Call:
glm(formula = sobj$formula, family = binomial, data = sobj$data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0980   0.2600   0.3934   0.5372   1.5205  

Coefficients:
                            Estimate Std. Error z value Pr(>|z|)    
(Intercept)                 4.977403   1.384708   3.595 0.000325 ***
TBIRTH_YEAR                -0.580065   0.048615 -11.932  < 2e-16 ***
EEDUCsome.hs1               0.174038   0.247178   0.704 0.481370    
EEDUCHS.diploma1            0.238643   0.214130   1.114 0.265074    
EEDUCsome.coll1             0.358565   0.212337   1.689 0.091284 .  
EEDUCassoc.deg1             0.509965   0.217754   2.342 0.019184 *  
EEDUCbach.deg1              0.930540   0.214824   4.332 1.48e-05 ***
EEDUCadv.deg1               1.013449   0.216680   4.677 2.91e-06 ***
MSmarried1                  0.008279   0.276911   0.030 0.976148    
MSwidowed1                 -0.016563   0.302989  -0.055 0.956405    
MSdivorced1                -0.163405   0.279871  -0.584 0.559316    
MSseparated1               -0.112009   0.310966  -0.360 0.718701    
MSnever1                    0.006981   0.281251   0.025 0.980198    
RRACEBlack1                 0.075462   0.080722   0.935 0.349872    
RRACEAsian1                 0.515971   0.146723   3.517 0.000437 ***
RRACEOther1                -0.307909   0.080565  -3.822 0.000132 ***
Hispanic1                   0.371466   0.080503   4.614 3.94e-06 ***
GENID_DESCRIBEmale1         0.264463   0.224518   1.178 0.238831    
GENID_DESCRIBEfemale1       0.256532   0.223598   1.147 0.251261    
GENID_DESCRIBEtransgender1  0.741516   0.578202   1.282 0.199684    
GENID_DESCRIBEother1        0.178993   0.303576   0.590 0.555449    
REGIONSouth1               -0.275065   0.078279  -3.514 0.000442 ***
REGIONMidwest1             -0.299359   0.083678  -3.578 0.000347 ***
REGIONWest1                -0.256970   0.079169  -3.246 0.001171 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 3983.1  on 5370  degrees of freedom
Residual deviance: 3547.0  on 5347  degrees of freedom
AIC: 3595

Number of Fisher Scoring iterations: 5

> pred_vals <- predict(model_logit1, s_dat_test, type = "response")
There were 22 warnings (use warnings() to see them)
> pred_model_logit1 <- (pred_vals > 0.5)
> table(pred = pred_model_logit1, true = dat_test$vaxx)
       true
pred        0     1
  FALSE    57    86
  TRUE   5629 41978
> 
> #when looking at the different types of models and their outputs
> #the logit model gave values that were higher than the linear model
> 
> #attempting using different values of pred_vals to see different results
> pred_model_logit1 <- (pred_vals > 0.3)
> table(pred = pred_model_logit1, true = dat_test$vaxx)
       true
pred        0     1
  FALSE     1     0
  TRUE   5685 42064
> 
> pred_model_logit1 <- (pred_vals > 0.9)
> table(pred = pred_model_logit1, true = dat_test$vaxx)
       true
pred        0     1
  FALSE  4256 18406
  TRUE   1430 23658
> 
> pred_model_logit1 <- (pred_vals > 0.6)
> table(pred = pred_model_logit1, true = dat_test$vaxx)
       true
pred        0     1
  FALSE   322   534
  TRUE   5364 41530
> #the higher value of the pred_vals the more numbers are shown for the given info
> 
> #while doing my code, figured out why EEDUC was not showing
> #after redoing the different vaxx coding, it finally worked

