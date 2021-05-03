library(lubridate)
library(magrittr)
library(forecast)
library(TSA)
library(lmtest)
library(car)
library(aod)
library(dplyr)

library(tsModel)

# for public health interventions considered
data_milestone <- data.frame(
  id      = 1:4,
  content = c("Campaign on Passive Smoking",
              "Larger Warnings on Cigarette Packs",
              "Smoke-Free Restaurants",
              "10% Tax Increase"),
  start   = as.Date(c("2001-01-01", "2002-09-30", "2005-06-01", "2012-01-01")),
  end     = c(NA, NA, NA, NA),
  wlow    = as.Date(c("1999-01-01", "2001-01-01", "2002-09-01", "2009-01-01")),
  wupp    = as.Date(c("2002-08-01", "2005-05-01", "2008-12-01", "2014-12-01"))
)

load("www/qsl_data_week.rda")
load("www/pop_smk.rda")

qsl_ts <- qsl_ts %>% transform(year_val = year(month),
                            month_val = month(month)) %>%
  merge(pop_smk, by.x = "year_val", by.y = "year", all.x = TRUE) %>%
  transform(rate = n/total*10^5) %>% 
  subset(month >= "1999-01-01" & month <  "2015-01-01") %>%
   arrange(month) %>%
  transform(time = seq_along(month) - 1)
detach("package:dplyr")

#----------------------------------------------------#
# ARIMA model, considering the interventions on by one seperately
#----------------------------------------------------#

qsl_ts_rate <- ts(log(qsl_ts$rate), frequency = 12, start = c(1999, 01)) 

Inter1 <- window(qsl_ts_rate, start = c(1999, 01), end = c(2002, 08))
Inter2 <- window(qsl_ts_rate, start = c(2001, 01), end = c(2005, 05))
Inter3 <- window(qsl_ts_rate, start = c(2002, 09), end = c(2008, 12))
Inter4 <- window(qsl_ts_rate, start = c(2009, 01), end = c(2014, 12))

arima_pre1 <- auto.arima(window(qsl_ts_rate, start = c(1999, 01), end = c(2001, 03)), ic = "aicc" )  #24 #ARIMA(0,1,0)  
arima_pre2 <- auto.arima(window(qsl_ts_rate, start = c(2001, 01), end = c(2002, 08)), ic = "aicc" )  #20 #ARIMA(0,0,1) with non-zero mean
arima_pre3 <- auto.arima(window(qsl_ts_rate, start = c(2002, 09), end = c(2005, 05)), ic = "aicc" )  #33 #ARIMA(0,0,0)(0,1,0)[12] with drift 
arima_pre4 <- auto.arima(window(qsl_ts_rate, start = c(2009, 01), end = c(2011, 12)), ic = "aicc" )  

#plot(forecast(arima_pre1, h = 20), main = "prediction for model in period 1"); points(Inter1, col = "red")
#plot(forecast(arima_pre2, h = 20), main = "prediction for model in period 2"); points(Inter2, col = "red")
#plot(forecast(arima_pre3, h = 20), main = "prediction for model in period 3"); points(Inter3, col = "red")
#plot(forecast(arima_pre4, h = 20), main = "prediction for model in period 4"); points(Inter4, col = "red")

#-----------------------------------------------------------#
## Period 1

I1_pre <- window(qsl_ts_rate, start = c(1999, 01), end = c(2000, 12)) 
#arima_pre1
library(plotly)
detach("package:plotly")
qts.m1_rate_1 <- TSA::arimax(Inter1, order = c(0, 1, 1), 
                             seasonal = list(order = c(0, 0, 0), period = 12),
                             xtransf = data.frame(Inter_I1 = 1*(seq(Inter1) >= 25)),
                             fixed = c(0, NA, NA), transfer = list(c(1, 0 )), 
                             method = "ML")
lmtest::coeftest (qts.m1_rate_1) # AIC 7.8 vs 13.46

omega0 <- 0.76659
delta <- -0.24430
Effect <- as.numeric(seq(Inter1) >= 25)
Effect_I1 <- filter(Effect, filter = delta, method = "recursive") * omega0

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_1)
obj.cov <- vcov(qts.m1_rate_1)
names(obj.mean) <- c("b0", "b1", "b2")
colnames(obj.cov)  <- c("b1", "b2")
rownames(obj.cov) <- c("b1", "b2")
exp(deltaMethod(obj.mean[c(2,3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov)) 
deltaMethod(obj.mean[c(2,3)], "exp (b2*(1-b1^7)/(1-b1) )", vcov. = obj.cov)  
wald.test(b = obj.mean[c(2,3)], Sigma = obj.cov, Terms = 1:2) 

rr_1Int <- c(exp(ts(Effect_I1, frequency = 12, start = c(1999, 01))))
time_1Int <- seq(as.Date("1999-01-01"), as.Date("2002-08-01"), by = "month")
month_1Int <- seq_along(time_1Int[time_1Int > as.Date("2000-12-01")]) - 1
rr_1Intci <- data.frame()
for (i in seq_along(month_1Int)){
   a <- month_1Int[i]
   rr_1Intci <-  rbind(
      rr_1Intci,
      exp(deltaMethod(obj.mean[c(2,3)], "b2*(1-b1^(a+1))/(1-b1)", vcov. = obj.cov))[, c(1, 3:4)]
   )
}
rr_tab1 <- data.frame(
   Date = time_1Int[time_1Int > as.Date("2000-12-01")],
   t = month_1Int,
   RR = round(rr_1Intci[, 1], 2),
   RR_low = round(rr_1Intci[, 2], 2),
   RR_upp = round(rr_1Intci[, 3], 2)
)


ggplot(data = NULL, 
       aes(time_1Int, rr_1Int
           #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
       )) + 
  geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
  theme_classic() +
  geom_ribbon(aes(ymin = .5, ymax = rr_1Int), fill = "lightblue")

NewInter1 <- Inter1 - ts(Effect_I1, frequency = frequency (Inter1), start = start(Inter1)) 
pl_int1 <- qsl_ts %>%
  subset(month >= "1999-01-01" & month <= "2002-08-01") %>%
  transform(preint1 = rate, 
            obs = rate,
            pred = exp(c(NewInter1)))
pl_int1$preint1[pl_int1$month >= "2000-12-31"] <- NA
pl_int1$obs[pl_int1$month < "2000-12-31"] <- NA
pl_int1$pred[pl_int1$month < "2000-12-31"] <- NA

#pl_int1_long <- tidyr::gather(pl_int1, serie_type, val, preint1:pred)
pl_int1_long <- reshape(pl_int1, direction = "long", varying = list(names(pl_int1)[8:10]), 
                        v.names = "val", idvar = "month", timevar = "serie_type")
pl_int1_long$serie_type <- factor(pl_int1_long$serie_type, 
                                  labels = c("Observed before the intervention", 
                                             "Observed after the intervention", 
                                             "Predicted in the absence of intervention"))


#-----------------------------------------------------------#
## Period 2


I2_pre <- window(qsl_ts_rate, start = c(2001, 01), end = c(2002, 08))  
#coeftest(arima_pre2) 

library(plotly)
detach("package:plotly")
qts.m1_rate_2 <- arimax(Inter2, order = c(0, 0, 1), seasonal = list(order = c(0, 0, 0), period = 12),
                        xtransf = data.frame(Inter_I2 = 1*(seq(Inter2) >= 21)),
                        # fixed = c(NA, NA, NA),
                        transfer=list(c(1, 0)), method = "ML")
#coeftest(qts.m1_rate_2)
#arima(Inter2, order = c(0, 0, 1)) 

omega0 <- 0.0819
delta  <- 0.8980
# Effect_I2 <- omega0* ( 1 - delta^ seq ( 1:50)) / ( 1 - delta ) 
Effect <- 1*(seq(Inter2) >= 21)
Effect_I2 <- filter(Effect, filter = delta, method = "recursive") * omega0

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_2)
obj.cov <- vcov(qts.m1_rate_2) 
names(obj.mean) <- c("b0", "b3", "b1", "b2")
colnames(obj.cov)  <-c("b0", "b3", "b1", "b2")
rownames (obj.cov ) <-c("b0", "b3", "b1", "b2")
exp(deltaMethod(obj.mean[c(3,4)], "b2*(1-b1^7)/(1-b1)", 
                vcov. = obj.cov[c(3, 4),c(3,4)]))
deltaMethod (obj.mean[c(3,4)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(3,4),c(3,4)] )
wald.test(b = obj.mean[c(3, 4)], Sigma = obj.cov[c(3, 4), c(3, 4)], Terms = 1:2) 

rr_2Int <- c(exp(ts(Effect_I2, frequency = 12, start = c(1999, 01))))
time_2Int <- seq(as.Date("2001-01-01"), as.Date("2005-05-01"), by = "month")
month_2Int <- seq_along(time_2Int[time_2Int > as.Date("2002-08-01")]) - 1
rr_2Intci <- data.frame()
for (i in seq_along(month_2Int)){
   a <- month_2Int[i]
   rr_2Intci <-  rbind(
      rr_2Intci,
      exp(deltaMethod(obj.mean[c(3,4)], "b2*(1-b1^(a+1))/(1-b1)", vcov. = obj.cov[c(3,4),c(3,4)]))[, c(1, 3:4)]
   )
}
rr_tab2 <- data.frame(
   Date = time_2Int[time_2Int > as.Date("2002-08-01")],
   t = month_2Int,
   RR = round(rr_2Intci[, 1], 2),
   RR_low = round(rr_2Intci[, 2], 2),
   RR_upp = round(rr_2Intci[, 3], 2)
)
ggplot(data = NULL, 
       aes(time_2Int, rr_2Int
           #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
       )) + 
  geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
  theme_classic() +
  geom_ribbon(aes(ymin = .5, ymax = rr_2Int), fill = "lightblue")

NewInter2 <- Inter2 - ts(Effect_I2, frequency = frequency(Inter2), 
                         start = start(Inter2)) 
pl_int2 <- qsl_ts %>%
  subset(month >= "2001-01-01" & month <= "2005-05-01") %>%
  transform(preint2 = rate, 
            obs = rate,
            pred = exp(c(NewInter2)))
pl_int2$preint2[pl_int2$month >= "2002-09-30"] <- NA
pl_int2$obs[pl_int2$month < "2002-09-30"] <- NA
pl_int2$pred[pl_int2$month < "2002-09-30"] <- NA

pl_int2_long <- reshape(pl_int2, direction = "long", varying = list(names(pl_int2)[8:10]), 
                        v.names = "val", idvar = "month", timevar = "serie_type")
pl_int2_long$serie_type <- factor(pl_int2_long$serie_type, 
                                  labels = c("Observed before the intervention", 
                                             "Observed after the intervention", 
                                             "Predicted in the absence of intervention"))


#-----------------------------------------------------------#
## Period 3

I3_pre <- window(qsl_ts_rate, start = c(2002, 09), end = c(2005, 05)) 
#arima_pre3
library(plotly)
detach("package:plotly")
qts.m1_rate_3 <- arimax(Inter3, order = c(0, 0, 1), seasonal = list(order = c(0, 1, 0), period = 12),
                        xtransf = data.frame(Inter_I3 = 1*(seq(Inter3) >= 34)),
                        # fixed = c (0, NA, NA),
                        transfer = list(c(1, 0)), method = "ML")
coeftest(qts.m1_rate_3)

omega0 <-  0.164882
delta  <- -0.735497
Effect <- 1*(seq(Inter3) >= 34)
Effect_I3 <- filter(Effect, filter = delta , method = "recursive") * omega0

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_3)
obj.cov <- vcov(qts.m1_rate_3) 
names(obj.mean) <- c("b0", "b1", "b2")
colnames(obj.cov) <- c("b0","b1", "b2")
rownames(obj.cov) <- c("b0", "b1", "b2")

exp(deltaMethod(obj.mean[c(2, 3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov[c(2, 3), c(2, 3)])) 
deltaMethod(obj.mean[c(2, 3)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(2, 3), c(2, 3)])  
wald.test(b = obj.mean[c(2, 3)], Sigma = obj.cov[c(2, 3), c(2, 3)], Terms = 1:2) 

# par(mar = c(5,5,4,2) + c(0.5, 1.0, 0.1,0.1) )
# plot.ts(exp(ts(Effect_I3, frequency = 12, start = c(2002, 09))), type = "h", 
#           ylim = c(0.9, 1.2), ylab = "", xlab = "", xaxt = 'n', yaxt = 'n',
#           col = "lightblue",  log = 'y',
#           main = "B", cex.main = 1.5, frame.plot = FALSE, lwd = 1.5)
# axis(1, at = c(1999:2015), cex.axis = 1.5) 
# axis(2, cex.axis = 1.5, las = 2) 
# title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 
# points(exp(ts(Effect_I3, frequency = 12, start = c(2002, 09))), type = "l")

rr_3Int <- c(exp(ts(Effect_I3, frequency = 12, start = c(1999, 01))))
time_3Int <- seq(as.Date("2002-09-01"), as.Date("2008-12-01"), by = "month")
month_3Int <- seq_along(time_3Int[time_3Int > as.Date("2005-05-01")]) - 1
rr_3Intci <- data.frame()
for (i in seq_along(month_3Int)){
   a <- month_3Int[i]
   rr_3Intci <-  rbind(
      rr_3Intci,
      exp(deltaMethod(obj.mean[c(2,3)], "b2*(1-b1^(a+1))/(1-b1)", vcov. = obj.cov[c(2, 3), c(2, 3)]))[, c(1, 3:4)]
   )
}
rr_tab3 <- data.frame(
   Date = time_3Int[time_3Int > as.Date("2005-05-01")],
   t = month_3Int,
   RR = round(rr_3Intci[, 1], 2),
   RR_low = round(rr_3Intci[, 2], 2),
   RR_upp = round(rr_3Intci[, 3], 2)
)
ggplot(data = NULL, 
       aes(time_3Int, rr_3Int
           #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
       )) + 
  geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
  theme_classic() +
  geom_ribbon(aes(ymin = .5, ymax = rr_3Int), fill = "lightblue")

NewInter3 <- Inter3 -  ts(Effect_I3, frequency = frequency(Inter3), start = start (Inter3))
pl_int3 <- qsl_ts %>%
  subset(month >= "2002-09-01" & month <= "2008-12-01") %>%
  transform(preint3 = rate, 
            obs = rate,
            pred = exp(c(NewInter3)))
pl_int3$preint3[pl_int3$month >= "2005-06-01"] <- NA
pl_int3$obs[pl_int3$month < "2005-06-01"] <- NA
pl_int3$pred[pl_int3$month < "2005-06-01"] <- NA

pl_int3_long <- reshape(pl_int3, direction = "long", varying = list(names(pl_int3)[8:10]), 
                        v.names = "val", idvar = "month", timevar = "serie_type")
pl_int3_long$serie_type <- factor(pl_int3_long$serie_type, 
                                  labels = c("Observed before the intervention", 
                                             "Observed after the intervention", 
                                             "Predicted in the absence of intervention"))

#-----------------------------------------------------------#
#  Period 4

I4_pre <- window(qsl_ts_rate, start = c(2009, 01), end = c(2011, 12))
#arima_pre4
Jan_2012 <- window(qsl_ts_rate, start = c(2012, 01), end = c(2012, 01))
Inter4[37] <- NA

library(plotly)
detach("package:plotly")
qts.m1_rate_4 <- arimax(Inter4, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 0), period = 12),
                        xtransf = data.frame(Inter_I4 =  1*(seq(Inter4) >= 38)
                                             # , T = 1*(seq(Inter4)==37 )
                        ), fixed = c(NA, NA, NA), transfer = list(c(1, 0) 
                                                                  # , c(0, 0 )
                        ), # xreg=data.frame(outlier = 1*(seq(Inter4) == 37)),
                        method = "ML")
coeftest(qts.m1_rate_4)

## Calculate the confidence interval of the the intervention effects 
obj.mean <- coef(qts.m1_rate_4)
obj.cov <- vcov(qts.m1_rate_4) 
names(obj.mean) <- c("b0", "b1", "b2")
colnames(obj.cov) <- c("b0", "b1", "b2")
rownames(obj.cov) <- c("b0", "b1", "b2")
exp(deltaMethod(obj.mean[c(2, 3)], "b2*(1-b1^7)/(1-b1)", vcov. = obj.cov[c(2, 3), c(2, 3)]))
deltaMethod(obj.mean[c(2, 3)], "exp(b2*(1-b1^7)/(1-b1))", vcov. = obj.cov[c(2, 3), c(2, 3)])
wald.test(b = obj.mean[c(2, 3)], Sigma = obj.cov[c(2, 3), c(2, 3)], Terms = 1:2) 

omega0 <- -0.015961
delta <- 0.319897 
Effect <- 1*(seq(Inter4) >= 38)
Effect_I4 <- filter(Effect, filter = delta ,  method = "recursive") * omega0
Effect_I4[37] <- NA

NewInter4 <- Inter4 - ts(Effect_I4, frequency = frequency (Inter4), start = start(Inter4)) 
# -  ts( Effect_I4_IO, frequency =  frequency (Inter4), start = start (Inter4) ) 

# par(mar = c(5, 5, 4, 2) + c(0.5, 1.0, 0.1,0.1))
# plot.ts(exp(ts(Effect_I4, frequency = 12, start = c(2009, 01))), type = "h", 
#           ylab = "", xlab = "", xaxt = 'n', yaxt = 'n',
#           ylim = c(0.5, 1.5), log ='y', col  = "lightblue", 
#           main = "B", cex.main = 1.5, frame.plot = FALSE, lwd = 1.5)
# axis(1, at = c(1999:2015), cex.axis = 1.5)
# axis(2, at = c(0.5, 0.65, 0.8, 1, 1.2), cex.axis = 1.5, las = 2 ) 
# title(xlab = 'Time', ylab = 'Rate Ratio', cex.lab = 1.5, line = 4) 
# points(exp(ts(Effect_I4, frequency = 12, start = c(2009,01))), type = "l")
rr_4Int <- c(exp(ts(Effect_I4, frequency = 12, start = c(1999, 01))))
time_4Int <- seq(as.Date("2009-01-01"), as.Date("2014-12-01"), by = "month")
month_4Int <- seq_along(time_4Int[time_4Int > as.Date("2011-12-01")]) - 1
rr_4Intci <- data.frame()
for (i in seq_along(month_4Int)){
   a <- month_4Int[i]
   rr_4Intci <-  rbind(
      rr_4Intci,
      exp(deltaMethod(obj.mean[c(2, 3)], "b2*(1-b1^(a+1))/(1-b1)", vcov. = obj.cov[c(2, 3), c(2, 3)]))[, c(1, 3:4)]
   )
}
rr_tab4 <- data.frame(
   Date = time_4Int[time_4Int > as.Date("2011-12-01")],
   t = month_4Int,
   RR = round(rr_4Intci[, 1], 2),
   RR_low = round(rr_4Intci[, 2], 2),
   RR_upp = round(rr_4Intci[, 3], 2)
)
ggplot(data = NULL, 
       aes(time_4Int, rr_4Int
           #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
       )) + 
  geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
  theme_classic() +
  geom_ribbon(aes(ymin = .5, ymax = rr_4Int), fill = "lightblue")

pl_int4 <- qsl_ts %>%
  subset(month >= "2009-01-01" & month <= "2014-12-01") %>%
  transform(preint4 = rate, 
            obs = rate,
            pred = exp(c(NewInter4)))
pl_int4$preint4[pl_int4$month >= "2012-01-01"] <- NA
pl_int4$obs[pl_int4$month <= "2012-01-01"] <- NA
pl_int4$pred[pl_int4$month <= "2012-01-01"] <- NA

pl_int4_long <- reshape(pl_int4, direction = "long", varying = list(names(pl_int4)[8:10]), 
                        v.names = "val", idvar = "month", timevar = "serie_type")
pl_int4_long$serie_type <- factor(pl_int4_long$serie_type, 
                                  labels = c("Observed before the intervention", 
                                             "Observed after the intervention", 
                                             "Predicted in the absence of intervention"))

save(data_milestone, qsl_ts, 
     rr_1Int, time_1Int, rr_tab1, pl_int1_long,
     rr_2Int, time_2Int, rr_tab2, pl_int2_long,
     rr_3Int, time_3Int, rr_tab3, pl_int3_long,
     rr_4Int, time_4Int, rr_tab4, pl_int4_long,
     file = "www/qls_app_data.Rdata")