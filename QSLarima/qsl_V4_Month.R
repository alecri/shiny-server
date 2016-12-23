library(shiny)
library(timevis)
library(dygraphs)
library(tidyverse)
library(Epi)
library(lubridate)
library(tsModel)
library(rms)
library(magrittr)
library(dplyr) 
library(forecast)
library(TSA)
library(lmtest)





## data for pipeline
data_milestone <- data.frame(
  id      = 1:11,
  content = c("Quit Smoking Line Initiation", "Campaign on passive smoking",
              "Larger Warnings on Cigarette Packs + Removal of Terms such as 'Light'", 
              "Banning of Indirect Tobacco Advertisements", "Smoke-Free Restaurants",
              "Prohibition of Cigarettes Selling to Minor", "Tax Increase (Mainly Snuff)",
              "Tax Increase", "Tax Increase (10%)", "Tax Decrease", "Tax Increase"),
  start   = c("1998-09-14", "2001-01-01", "2002-09-30", "2003-01-01", "2005-06-01",
              "2005-07-01", "2007-01-01", "2011-01-01", "2012-01-01", "2014-01-01",
              "2015-01-01"),
  end     = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
data_milestone$start <- as.Date(data_milestone$start)


load("www/qsl_data_week.rda")
load("www/pop_smk.rda")

## pre-prepare the data
qsl_ts <- qsl_ts %>% mutate(year_val = year(month),
                            month_val = month(month)) %>%
  merge(pop_smk, by.x = "year_val", by.y = "year", all.x = TRUE) %>%
  arrange(month)
qsl_ts$rate <- with(qsl_ts, n/total*10^5)


#---------------------------------#
#Step 0, Description, monthly data during 2009 to 2014
#---------------------------------#

qsl_ts_All <- filter(qsl_ts, month >= "1999-01-01" & month <  "2015-01-01")
qsl_ts_All$time <- seq_along(qsl_ts_All$month) - 1

par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot(( ts (qsl_ts_All$rate, start = c(1999,01), frequency = 12) ), ylim = c(25, 300),
     ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
     main = " " , log = 'y', col = "grey",
     frame.plot=FALSE, lwd = 2, cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)
points (ts (qsl_ts_All$rate, start = c(1999,01), frequency = 12), pch = 19)
axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(25, 50, 100, 200, 300), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Calling rates per 100,000 smokers', cex.lab = 1.5, line = 4) 

#----------------------------------------------------#
# ARIMA model, considering the interventions on by one seperately
#----------------------------------------------------#

# Remove or not remove the data of January 2012
qsl_ts_All_rate <- ts ( log(qsl_ts_All$rate), frequency = 12, start = c(1999,01)) 
# pos_max <- which ( qsl_ts_All_rate == max ( qsl_ts_All_rate )) 
# qsl_ts_All_rate [ pos_max ]<- NA    

Inter1 <- window (qsl_ts_All_rate, start = c(1999, 01), end = c(2002, 08) )
Inter2 <- window (qsl_ts_All_rate, start = c(2001, 01), end = c(2005, 05) )
Inter3 <- window (qsl_ts_All_rate, start = c(2002, 09), end = c(2008, 12) )
Inter4 <- window (qsl_ts_All_rate, start = c(2009, 01), end = c(2014, 12) )

# which ( Inter4 == max ( Inter4) ) 
# arima_pre1 <- auto.arima (  window (qsl_ts_All_rate, start = c(1999, 01), end = c(2001, 03) ) )  #27 #ARIMA(0,1,0)  
arima_pre1 <- auto.arima (  window (qsl_ts_All_rate, start = c(1999, 01), end = c(2001, 03) ), ic = "aicc" )  #24 #ARIMA(0,1,0)  
arima_pre2 <- auto.arima (  window (qsl_ts_All_rate, start = c(2001, 01), end = c(2002, 08) ), ic = "aicc" )  #20 #ARIMA(0,0,1) with non-zero mean
arima_pre3 <- auto.arima (  window (qsl_ts_All_rate, start = c(2002, 09), end = c(2005, 05) ), ic = "aicc" )  #33 #ARIMA(0,0,0)(0,1,0)[12] with drift 
arima_pre4 <- auto.arima (  window (qsl_ts_All_rate, start = c(2009, 01), end = c(2011, 12) ), ic = "aicc" )  

# plot ( forecast(arima_pre1, h = 20 ), main = "prediction for model in period 1"  ); points ( Inter1, col = "red")
# # pdf (pdf ( file = "../../Plots/MonNoLag/log_rate_I2_Forecast.pdf")    ) 
# plot ( forecast(arima_pre2, h = 20 ), main = "prediction for model in period 2"   ); points ( Inter2, col = "red")
# # dev.off () 
# plot ( forecast(arima_pre3, h = 20 ), main = "prediction for model in period 3"   ); points ( Inter3, col = "red")
# plot ( forecast(arima_pre4, h = 20 ), main = "prediction for model in period 4"   ); points ( Inter4, col = "red")

#-----------------------------------------------------------#
## Period 1
I1_pre <- window (qsl_ts_All_rate, start = c(1999, 01), end = c(2000, 12) ) 

arima_pre1
detach("package:dplyr")
qts.m1_rate_1=arimax( Inter1, 
                      order=c(0,1,1),seasonal=list(order=c(0,0,0), period=12),
                      xtransf = data.frame( Inter_I1 =  1*(seq(Inter1)>=25 )
                      ),
                      fixed = c(0, NA, NA ),
                      transfer=list(  c(1, 0 )
                      ),
                      method="ML")
lmtest::coeftest (qts.m1_rate_1)  #AIC 7.8 vs 13.46

omega0 <- 0.76659; 
delta <- -0.24430; 

Effect <- 1*(seq(Inter1)>=25)
Effect_I1 <- filter (Effect, filter= delta ,  method = "recursive")* omega0

## Calculate the confidence interval of the the intervention effects 
library ( car )
library ( aod )

obj.mean <- coef(qts.m1_rate_1) ; 
obj.cov <- vcov (qts.m1_rate_1 ); 

names( obj.mean ) <- c("b0", "b1", "b2"); 
colnames (obj.cov)  <- c("b1", "b2")
rownames (obj.cov ) <- c("b1", "b2")
exp ( deltaMethod (obj.mean[c(2,3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov )  ) 
deltaMethod (obj.mean[c(2,3)], "exp (b2*(1-b1^7)/(1-b1) )", vcov. = obj.cov )  

wald.test ( b = obj.mean[c(2,3)], Sigma = obj.cov, Terms = 1:2 ) 

# dev.new ( width=4.875, height=2.5,pointsize=8 )
# pdf ( file = "../../Plots/MonNoLag/Log_rate_Effect_Int1_01.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot ( exp ( ts(Effect_I1, frequency = 12, start = c(1999,01)) ) , type = "h", 
       ylim = c(0.4, 2.5),  ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
       col = "lightblue", log = 'y', 
       main = "B", cex.main = 1.5, frame.plot=FALSE, lwd = 1.5) 
axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(0.4, 0.55, 0.75, 1, 1.35,1.8, 2.5), cex.axis = 1.5, las = 2) 

title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 
points (exp ( ts(Effect_I1, frequency = 12, start = c(1999,01)) ) , type = "l")
# dev.off () 

#--------------------------#
# pdf ( file = "../../Plots/MonNoLag/Mod_Fit_Inter_1.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot   ( exp (Inter1), type = "n" , ylim = c(10, 120),
         main = "A", log = 'y',
         cex.main=1.5, cex.lab=1.5,cex.axis=1.5, 
         ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
         frame.plot=FALSE, lwd = 2)
# points ( exp (Inter1),  type = "l")
points ( exp (I1_pre),  type = "l", lwd = 2)
points ( exp (I1_pre), pch = 1, lwd = 2)
points ( exp ( window (Inter1, start = c(2001,01))), col = "grey", pch = 2, lwd = 2)
points ( exp ( window (Inter1, start = c(2001,01))), col = "grey", type = "l", lty = "dashed", lwd = 2)
abline (  v = c(2001), lty = "dashed", lwd = 1, col  ="grey60")

# points ( exp(fitted (qts.m1_rate_1)), col = "red", type = "l" )

axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(10, 20, 35, 60, 100), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Calling rates per 100,000 smokers', cex.lab = 1.5, line = 4) 

NewInter1 <- Inter1 - ts( Effect_I1, frequency =  frequency (Inter1), start = start (Inter1) ) 
points ( exp( window (NewInter1, start = c(2001,01) ) ) , col = "blue", pch = 10, lwd = 2 )
points ( exp( window (NewInter1, start = c(2001,01) ) ) , col = "blue", type = "l", lwd = 2, lty = 'dotted' )

legend("bottomleft", legend = c("Observed before the intervention", 
                                "Observed after the intervention",
                                "Predicted in the absence of intervention"),
       lty = c("solid","dashed","dotted"), lwd = rep(2,3),
       col = c("black", "grey", "blue"), 
       pch = c(1:2,10), bty = "n")
# dev.off () 

#-----------------------------------------------------------#
## Period 2

I2_pre <- window (qsl_ts_All_rate, start = c(2001, 01), end = c(2002, 08) )  

coeftest ( arima_pre2 ) 
qts.m1_rate_2=arimax( Inter2, 
                      order=c(0,0,1),seasonal=list(order=c(0,0,0), period=12),
                      xtransf = data.frame( Inter_I2 =  1*(seq(Inter2)>=21 )
                      ),
                      # fixed = c(NA, NA, NA),
                      transfer=list(  c(1, 0 )
                      ),
                      method="ML")
coeftest (qts.m1_rate_2)
arima ( Inter2, order = c(0,0,1 ) ) 

omega0 <- 0.0819
delta  <- 0.8980

# Effect_I2 <- omega0* ( 1 - delta^ seq ( 1:50)) / ( 1 - delta ) 
Effect <- 1*(seq(Inter2)>=21)
Effect_I2 <- filter (Effect, filter= delta ,  method = "recursive")* omega0

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_2) ; 
obj.cov <-  vcov (qts.m1_rate_2 ); 

names( obj.mean ) <- c("b0", "b3", "b1", "b2"); 
colnames (obj.cov)  <-c("b0", "b3", "b1", "b2")
rownames (obj.cov ) <-c("b0", "b3", "b1", "b2")
exp ( deltaMethod (obj.mean[c(3,4)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov[c(3,4),c(3,4)] )  ) 

deltaMethod (obj.mean[c(3,4)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(3,4),c(3,4)] )

library ( aod)
wald.test ( b = obj.mean[c(3,4)], Sigma = obj.cov[c(3,4),c(3,4)] , Terms = 1:2 ) 


# Plot 
#-------------#
# pdf ( file = "../../Plots/MonNoLag/Log_rate_Effect_Int2_01.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot.ts ( exp ( ts(Effect_I2, frequency = 12, start = c(2001,01)) ) , type = "h", 
          ylim = c(0.5, 2.5),  ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
          col  = "lightblue", log = 'y',
          main = "B", cex.main = 1.5, frame.plot=FALSE, lwd = 1.5  ) 
axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(0.5, 0.7, 1.0, 1.50, 2.0), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 
points (exp ( ts(Effect_I2, frequency = 12, start = c(2001,01)) ) , type = "l")

# dev.off () 

# pdf ( file = "../../Plots/MonNoLag/Mod_Fit_Inter_2.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot   ( exp (Inter2), type = "n" , ylim = c(20, 250), 
         main = "A", log = 'y',
         ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
         frame.plot=FALSE, lwd = 2,
         cex.main=1.5, cex.lab=1.5,cex.axis=1.5)
axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(25, 45, 80, 150, 250), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Calling rates per 100,000 smokers', cex.lab = 1.5, line = 4) 

points ( exp (I2_pre),  type = "l", lwd = 2)
points ( exp (I2_pre), pch = 1, lwd = 2)

points ( exp ( window (Inter2, start = c(2002,09))), col = "grey", pch = 2, lwd = 2 )
points ( exp ( window (Inter2, start = c(2002,09))), col = "grey", type = "l", lwd = 2, lty = "dashed")
abline (  v = c(2002 +  8/12), lty = "dashed", col  ="grey60" )

NewInter2 <- Inter2 -  ts( Effect_I2, frequency =  frequency (Inter2), start = start (Inter2) ) 
points ( exp( window (NewInter2, start = c(2002,09) ) ) , col = "blue", pch = 10, lwd = 2 )
points ( exp( window (NewInter2, start = c(2002,09) ) ) , col = "blue", type = "l", lwd = 2, lty = "dotted")

# points ( exp(fitted (qts.m1_rate_2)), col = "red", type = "l" )

legend("topleft", legend = c("Observed before the intervention", 
                             "Observed after the intervention",
                             "Predicted in the absence of intervention"),
       lty = c("solid","dashed","dotted"), lwd = rep(2,3),
       col = c("black", "grey", "blue"), 
       pch = c(1:2,10), bty = "n")
# dev.off () 

#-----------------------------------------------------------#
## Period 3

I3_pre <- window (qsl_ts_All_rate, start = c(2002, 09), end = c(2005, 05) ) 

arima_pre3
qts.m1_rate_3 = arimax( Inter3, 
                        order=c(0,0,1),seasonal=list(order=c(0,1,0), period=12),
                        xtransf = data.frame( Inter_I3=  1*(seq(Inter3)>=34 )
                        ),
                        # fixed = c (0, NA, NA),
                        transfer=list(  c(1, 0 )
                        ),
                        method="ML")
coeftest (qts.m1_rate_3)

omega0 <-  0.164882
delta  <- -0.735497
Effect <- 1*(seq(Inter3)>=34)
Effect_I3 <- filter (Effect, filter= delta ,  method = "recursive")* omega0
# dev.new ( width=4.875, height=2.5,pointsize=8 )

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_3) ; 
obj.cov <- vcov (qts.m1_rate_3 ); 

names( obj.mean ) <- c("b0", "b1", "b2"); 
colnames (obj.cov)  <- c("b0","b1", "b2")
rownames (obj.cov ) <- c("b0", "b1", "b2")

library ( car)
exp ( deltaMethod (obj.mean[c(2,3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov[c(2,3),c(2,3)] )  ) 
deltaMethod (obj.mean[c(2,3)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(2,3),c(2,3)] )  

wald.test ( b = obj.mean[c(2,3)], Sigma = obj.cov[c(2,3),c(2,3)] , Terms = 1:2 ) 

# pdf ( file = "../../Plots/MonNoLag/Log_rate_Effect_Int3_01.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot.ts ( exp ( ts(Effect_I3, frequency = 12, start = c(2002,09)) ) , type = "h", 
          ylim = c(0.9, 1.2),  ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
          col  = "lightblue",  log = 'y',
          main = "B", cex.main = 1.5, frame.plot=FALSE, lwd = 1.5 )
axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 

points (exp ( ts(Effect_I3, frequency = 12, start = c(2002,09)) ) , type = "l")
# dev.off () 

# Print ------------------------------------------
# pdf ( file = "../../Plots/MonNoLag/Mod_Fit_Inter_3.pdf") 
par(mar = c(5,5,4,2) + c(1.5, 1.5, 0.1,0.1) )
plot   (exp (Inter3), type = "n" , ylim = c(50, 220), 
        main = "A", log = 'y',
        ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
        frame.plot=FALSE, lwd = 2,
        cex.main=1.5, cex.lab=1.5,cex.axis=1.5
)

axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(50, 70, 100, 140, 200), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Calling rates per 100,000 smokers', cex.lab = 1.5, line = 4) 

points (exp (I3_pre),  type = "l", lwd = 2)
points (exp (I3_pre), lwd = 2 )
points (exp ( window (Inter3, start = c(2005,06))), col = "grey", pch = 2, lwd = 2 )
points (exp ( window (Inter3, start = c(2005,06))), col = "grey", type = "l", lwd = 2, lty = "dashed" )
abline (v = c(2005 +  5/12), lty = "dashed", col  ="grey60" )

NewInter3 <- Inter3 -  ts( Effect_I3, frequency =  frequency (Inter3), start = start (Inter3) ) 
points (exp (window (NewInter3, start = c(2005,06) ) ) , col = "blue" , type = "l", lwd = 2, lty = "dotted")
points (exp (window (NewInter3, start = c(2005,06) ) ) , col = "blue" , pch = 10, lwd = 2 )
legend("topleft", legend = c("Observed before the intervention", 
                             "Observed after the intervention",
                             "Predicted in the absence of intervention"),
       lty = c("solid","dashed","dotted"), lwd = rep(2,3),
       col = c("black", "grey", "blue"), 
       pch = c(1:2,10), bty = "n")
# dev.off () 

#-----------------------------------------------------------#
#  Period 4

I4_pre <- window (qsl_ts_All_rate, start = c(2009, 01), end = c(2011, 12) )

arima_pre4

Jan_2012 <- window (qsl_ts_All_rate, start = c(2012, 01), end = c(2012, 01) )
Inter4[37] <- NA

qts.m1_rate_4=arimax( Inter4, 
                      order=c(0,1,1),seasonal=list(order=c(0,1,0), period=12),
                      xtransf = data.frame(Inter_I4 =  1*(seq(Inter4)>=38 )
                                           # , T = 1*(seq(Inter4)==37 )
                      ),
                      fixed = c(NA, NA, NA),
                      transfer=list( c(1, 0 ) 
                                     # , c(0, 0 )
                      ),
                      # xreg=data.frame (outlier = 1*(seq(Inter4)==37 ) ),
                      method="ML")
## not significant for period 4, but outlier is identified
coeftest (qts.m1_rate_4)

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_4) ; 
obj.cov <- vcov (qts.m1_rate_4 ); 

names( obj.mean )   <- c("b0", "b1", "b2"); 
colnames (obj.cov)  <- c("b0", "b1", "b2"); 
rownames (obj.cov ) <- c("b0", "b1", "b2"); 
exp ( deltaMethod (obj.mean[c(2,3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov[c(2,3),c(2,3)])  ) 
deltaMethod (obj.mean[c(2,3)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(2,3),c(2,3)])  
wald.test ( b = obj.mean[c(2,3)], Sigma = obj.cov[c(2,3),c(2,3)] , Terms = 1:2 ) 

# Print ------------------------------------------
# pdf ( file = "../../Plots/MonNoLag/Mod_Fit_Inter_4.pdf") 
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1, 0.1) )
plot   (exp (Inter4), type = "n" , ylim = c(10, 300),
        main = "A", log = 'y',
        ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
        frame.plot=FALSE, lwd = 2,
        cex.main=1.5, cex.lab=1.5,cex.axis=1.5)

axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(10, 25, 50, 100, 180, 300), cex.axis = 1.5, las = 2) 
title(xlab = 'Time', ylab = 'Calling rates per 100,000 smokers', cex.lab = 1.5, line = 4) 

# points ( exp(Inter3), type = "l")
points (exp (I4_pre),  type = "l", lwd = 2)
points (exp (I4_pre), pch = 1, lwd = 2 )
points (exp ( window (Inter4, start = c(2012,01))), col = "grey", pch = 2, lwd = 2)
points (exp ( window (Inter4, start = c(2012,01))), col = "grey", type = "l", lwd = 2 ,lty = "dashed")

points ( exp(window (qsl_ts_All_rate, start = c(2012, 01), end = c(2012, 01) ) ) , 
         col = "red", type = "l", lty = "dotted", lwd = 2, cex =1 ) 

points ( exp (Jan_2012), col = "red", lwd = 2, pch = 19 )

abline (v = c(2012 +  0/12), lty = "dashed", col  ="grey60" )

omega0 <- -0.015961; 
delta <-  0.319897; 

Effect <- 1*(seq(Inter4)>=38)
Effect_I4 <- filter (Effect, filter= delta ,  method = "recursive")* omega0
Effect_I4[37] <- NA

NewInter4 <- Inter4 - ts( Effect_I4, frequency =  frequency (Inter4), start = start (Inter4) ) 
# -  ts( Effect_I4_IO, frequency =  frequency (Inter4), start = start (Inter4) ) 


points (exp (window (NewInter4, start = c(2012,01), end = c(2014,12) ) ) , 
        col = "blue", pch = 10, lwd = 2, cex =1 ) #outlier effect
points (exp (window (NewInter4, start = c(2012,01), end = c(2014,12) ) ) , 
        col = "blue", type = "l", lty = "dotted", lwd = 2, cex =1 ) #outlier effect

legend("bottomleft", legend = c("Observed before the intervention", 
                                "Observed after the intervention",
                                "Predicted in the absence of intervention", 
                                "Outlier"),
       lty = c("solid","dashed","dotted", "F1"), lwd = rep(2,4),
       col = c("black", "grey", "blue", "red"), 
       pch = c(1:2,10, 19), bty = "n")
# dev.off () 

# plot the effect of intervention 4
# pdf ( file = "../../Plots/MonNoLag/Log_rate_Effect_Int4_01.pdf")
par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
plot.ts ( exp ( ts(Effect_I4 , frequency = 12, start = c(2009,01)) ) , type = "h", 
          ylab = "", xlab = "",  xaxt = 'n', yaxt = 'n',
          ylim = c(0.5, 1.5), log ='y',
          col  = "lightblue", 
          main = "B", cex.main = 1.5, frame.plot=FALSE, lwd = 1.5)

axis(1, at = c(1999:2015), cex.axis = 1.5) 
axis(2, at = c(0.5, 0.65, 0.8, 1, 1.2), cex.axis = 1.5, las = 2 ) 

title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 
points ( exp ( ts(Effect_I4, frequency = 12, start = c(2009,01)) ) , type = "l")
# dev.off ()