require(TSA)
data <- read.csv("weekly_attendance_summary.csv")
head(data)

# Setting up the time series as in our lab 1:
attendance <- ts(data$Attended, start=c(2023, 35), frequency=52) 
cancellations <- ts(data$Cancelled, start=c(2023, 35), frequency=52)

# Plotting the time series now:
# Attendance
plot.ts(attendance, type="o", col="blue", pch="*", lty=2,
        main="Weekly Attendance Over Time",
        xlab="Week", ylab="Attendance")

# Cancellations
plot.ts(cancellations, type="o", col="red", pch="*", lty=2,
        main="Weekly Cancellations Over Time",
        xlab="Week", ylab="Cancellations")

# Attendance
summary(attendance)
mean(attendance)

# Cancellations
summary(cancellations)
mean(cancellations)

# Histograms and Q-Q plots
# Attendance
par(mfrow=c(2,1))
hist(attendance, prob=TRUE, main="Histogram of Attendance", col="lightblue", xlab="Attendance")
lines(density(attendance), col="red")  # Density overlay
qqnorm(attendance, main="Q-Q Plot of Attendance")
qqline(attendance, col="red")

# Cancellations
par(mfrow=c(2,1))
hist(cancellations, prob=TRUE, main="Histogram of Cancellations", col="lightpink", xlab="Cancellations")
lines(density(cancellations), col="red")
qqnorm(cancellations, main="Q-Q Plot of Cancellations")
qqline(cancellations, col="red")

lag.plot1=function(data1,max.lag=1,corr=TRUE,smooth=FALSE)
{ 
  name1=paste(deparse(substitute(data1)),"(t-",sep="")
  name2=paste(deparse(substitute(data1)),"(t)",sep="")
  data1=as.ts(data1)
  max.lag=as.integer(max.lag)
  prow=ceiling(sqrt(max.lag))
  pcol=ceiling(max.lag/prow)
  a=acf(data1,max.lag+1,plot=FALSE)$acf[-1]
  par(mfrow=c(prow,pcol), mar=c(2.5, 4, 2.5, 1), cex.main=1.1, font.main=1)
  for(h in 1:max.lag){                       
    plot(lag(data1,-h), data1, xy.labels=FALSE, main=paste(name1,h,")",sep=""), ylab=name2, xlab="") 
    if (smooth==TRUE) 
      lines(lowess(ts.intersect(lag(data1,-h),data1)[,1],
                   ts.intersect(lag(data1,-h),data1)[,2]), col="red")
    if (corr==TRUE)
      legend("topright", legend=round(a[h], digits=2), text.col ="blue", bg="white", x.intersp=0)
  }
}

# Lag Plots (1–12 lags)
# Attendance
dev.new()
par(mfrow=c(4,3))  # 4 rows, 3 columns
lag.plot1(attendance,12, TRUE, TRUE)

# Cancellations
dev.new()
par(mfrow=c(4,3))  # Reset layout
lag.plot1(cancellations, 12, TRUE, TRUE)

# ACF and PACF
# Attendance
par(mfrow=c(2,1))
acf(attendance, main="ACF of Attendance")
pacf(attendance, main="PACF of Attendance")

# Cancellations
par(mfrow=c(2,1))
acf(cancellations, main="ACF of Cancellations")
pacf(cancellations, main="PACF of Cancellations")

boxcox_attendance <- BoxCox.ar(attendance, method="yule-walker")
boxcox_attendance$ci  # Confidence interval for lambda
boxcox_attendance$mle  # Maximum likelihood estimate (MLE) for lambda

# Apply Box-Cox transformation for Cancellations
cancellations_shifted <- cancellations+1
boxcox_cancellations <- BoxCox.ar(cancellations_shifted, method="yule-walker")
boxcox_cancellations$ci  # Confidence interval for lambda
boxcox_cancellations$mle  # Maximum likelihood estimate (MLE) for lambda

# Apply log transformation to cancellations
log_cancellations <- log(cancellations_shifted)

# Plot the log-transformed cancellations
plot.ts(log_cancellations, type="o", col="purple", pch="*", lty=2,
        main="Log-Transformed Weekly Cancellations",
        xlab="Time", ylab="Log of Cancellations")

# ACF of log-transformed cancellations
acf(log_cancellations, main="ACF of Log-Transformed Cancellations")

# PACF of log-transformed cancellations
pacf(log_cancellations, main="PACF of Log-Transformed Cancellations")

eacf(attendance)
eacf(cancellations)
eacf(log_cancellations)

n <- length(attendance)  # Total number of observations
n
train_attendance <- attendance[1:(n-10)]  # Just not the last 10 weeks
train_cancellations

test_attendance <- attendance[(n-9):n]    # Last 10 weeks for testing

train_log_cancellations <- log_cancellations[1:(n-10)]  # excluding last 10 weeks

train_cancellations <- cancellations[1:(n-10)] 
test_cancellations <- cancellations[(n-9):n] 
test_log_cancellations <- log_cancellations[(n-9):n]    # Last 10 weeks

sim_plot=function(series, expression){
  dev.new(width=9.5, height=9.5,pointsize=12)
  par(mfrow=c(3,1))
  plot.ts(series, type="o", main=(expression))
  acf(series)
  pacf(series)
  return(0)
}

# Initial exploration:
sim_plot(train_attendance, expression="Training Attendance Time Series")
sim_plot(train_log_cancellations, expression="Training Log Cancellations Time Series")
sim_plot(train_cancellations, expression="Training Cancellations Time Series")


sim_plot_arima=function(series, expression,p,q){
  dev.new(width=9.5, height=9.5,pointsize=12)
  par(mfrow=c(3,1))
  plot.ts(series, type="o", main=(expression))
  acf(series)
  pacf(series)
  dev.new(width=9.5, height=5,pointsize=12)
  par(mfrow=c(2,1))
  ACF = ARMAacf(ar=p, ma=q, 20)
  plot(ACF[2:21], type="h", xlab="lag", main="theoretical ACF")
  abline(h=0)
  PACF = ARMAacf(ar=p, ma=q, 20,pacf=TRUE)
  plot(PACF, type="h", xlab="lag", main="theoretical PACF")
  abline(h=0)
  return(0)
}

ar1_attendance = arima(train_attendance, order=c(1, 0, 0))
ar1_attendance
ma1_attendance = arima(train_attendance, order=c(0, 0, 1))
ma1_attendance
arma11_attendance = arima(train_attendance, order=c(1, 0, 1))
arma11_attendance
ma3_cancellations = arima(train_log_cancellations, order=c(0, 0, 3))
ma3_cancellations
arma11_cancellations = arima(train_log_cancellations, order=c(1, 0, 1))
arma11_cancellations
arma21_cancellations = arima(train_log_cancellations, order=c(2, 0, 1))
arma21_cancellations


# simulations as per Lab 2:
# WE REMOVED THIS PORTION LATER.
# Attendance Models
# AR(1) Model
ar1_attendance_sim <- arima.sim(n = 42, list(order = c(1, 0, 0), ar = c(0.3705)), sd = sqrt(50.85))
sim_plot_arima(ar1_attendance_sim, expression((1 - 0.3705 * B) ~ Z[t] == 23.1211), c(0.3705), 0)

# MA(1) Model
ma1_attendance_sim <- arima.sim(n=42, list(order = c(0, 0, 1), ma = c(0.2942)), sd = sqrt(52.29))
sim_plot_arima(ma1_attendance_sim, expression(Z[t] == 23.2661 + (1 + 0.2942 * B) ~ a[t]), 0, c(0.2942) )

# ARMA(1,1) Model
arma11_attendance_sim <- arima.sim(n=42, list(order = c(1, 0, 1), ar = c(0.4154), ma = c(-0.0513)), sd = sqrt(50.82))
sim_plot_arima(arma11_attendance_sim, expression((1 - 0.4154 * B) ~ Z[t] == 23.1052 + (1 - 0.0513 * B) ~ a[t]), c(0.4154), c(-0.0513) )

#Cancelled models
# MA(3) Model
ma3_cancellations_sim <- arima.sim(n = 42, list(order = c(0, 0, 3), ma = c(0.4856, 0.1360, -0.1906)), sd = sqrt(0.1548))
sim_plot_arima(ma3_cancellations_sim, expression(Z[t] == 2.2138 + (1 + 0.4856 * B + 0.1360 * B^2 - 0.1906 * B^3) ~ a[t]), 0, c(0.4856, 0.1360, -0.1906))

# ARMA(1,1) Model
arma11_cancellations_sim <- arima.sim(n = 42, list(order = c(1, 0, 1), ar = c(0.3524), ma = c(0.0478)), sd = sqrt(0.1599))
sim_plot_arima(arma11_cancellations_sim, expression((1 - 0.3524 * B) ~ Z[t] == 2.1963 + (1 + 0.0478 * B) ~ a[t]), c(0.3524), c(0.0478) )

# ARMA(2,1) Model
arma21_cancellations_sim <- arima.sim(n=42, list(order = c(2, 0, 1), ar = c(0.2599, 0.0367), ma = c(0.1404)), sd = sqrt(0.1599))
sim_plot_arima(arma21_cancellations_sim, expression((1 - 0.2599 * B - 0.0367 * B^2) ~ Z[t] == 2.1961 + (1 + 0.1404 * B) ~ a[t]), c(0.2599, 0.0367),c(0.1404))

# DIAGONISTIC OF RESIDUALS (Based on Lab 3,5):
#=============================================================================================================
num_obs <- length(train_attendance)
ar1_attendance.mle1 = arima(train_attendance, order=c(1, 0, 0), method ="ML", xreg = 1: num_obs)
ar1_attendance.mle1$coef #(parameter estimates)
sqrt(diag(ar1_attendance.mle1$var.coef))# standard errors
ar1_attendance.mle1$sigma2 # noise variance
res1 = residuals(ar1_attendance.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res1, prob=TRUE, 12)   # histogram
lines(density(res1))     # smooth it - ?density for details
qqnorm(res1)             # normal Q-Q plot
qqline(res1)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res1, lag=40)
pacf(res1, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res1, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res1, type="o", main="residuals")
plot.ts(rstandard(ar1_attendance.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(ar1_attendance.mle1, robust=F)
detectAO(ar1_attendance.mle1)
detectIO(ar1_attendance.mle1)
#==============================================================================================
ma1_attendance.mle1 = arima(train_attendance, order=c(0, 0, 1), method ="ML", xreg = 1: num_obs)
ma1_attendance.mle1$coef #(parameter estimates)
sqrt(diag(ma1_attendance.mle1$var.coef))# standard errors
ma1_attendance.mle1$sigma2 # noise variance
res2 = residuals(ma1_attendance.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res2, prob=TRUE, 12)   # histogram
lines(density(res2))     # smooth it - ?density for details
qqnorm(res2)             # normal Q-Q plot
qqline(res2)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res2, lag=40)
pacf(res2, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res2, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res2, type="o", main="residuals")
plot.ts(rstandard(ma1_attendance.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(ma1_attendance.mle1, robust=F)
detectAO(ma1_attendance.mle1)
detectIO(ma1_attendance.mle1)



arma11_attendance.mle1 = arima(train_attendance, order=c(1, 0, 1), method ="ML", xreg = 1: num_obs)
arma11_attendance.mle1$coef #(parameter estimates)
sqrt(diag(arma11_attendance.mle1$var.coef))# standard errors
arma11_attendance.mle1$sigma2 # noise variance
res3 = residuals(arma11_attendance.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res3, prob=TRUE, 12)   # histogram
lines(density(res3))     # smooth it - ?density for details
qqnorm(res3)             # normal Q-Q plot
qqline(res3)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res3, lag=40)
pacf(res3, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res3, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res3, type="o", main="residuals")
plot.ts(rstandard(arma11_attendance.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(arma11_attendance.mle1, robust=F)
detectAO(arma11_attendance.mle1)
detectIO(arma11_attendance.mle1)


num_obs2 <- length(train_log_cancellations)
ma3_cancellations.mle1 = arima(train_log_cancellations, order=c(0, 0, 3), method ="ML", xreg = 1: num_obs2)
ma3_cancellations.mle1$coef #(parameter estimates)
sqrt(diag(ma3_cancellations.mle1$var.coef))# standard errors
ma3_cancellations.mle1$sigma2 # noise variance
res4 = residuals(ma3_cancellations.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res4, prob=TRUE, 12)   # histogram
lines(density(res4))     # smooth it - ?density for details
qqnorm(res4)             # normal Q-Q plot
qqline(res4)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res4, lag=40)
pacf(res4, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res4, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res4, type="o", main="residuals")
plot.ts(rstandard(ma3_cancellations.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(ma3_cancellations.mle1, robust=F)
detectAO(ma3_cancellations.mle1)
detectIO(ma3_cancellations.mle1)


arma11_cancellations.mle1 = arima(train_log_cancellations, order=c(1, 0, 1), method ="ML", xreg = 1: num_obs2)
arma11_cancellations.mle1$coef #(parameter estimates)
sqrt(diag(arma11_cancellations.mle1$var.coef))# standard errors
arma11_cancellations.mle1$sigma2 # noise variance
res5 = residuals(arma11_cancellations.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res5, prob=TRUE, 12)   # histogram
lines(density(res5))     # smooth it - ?density for details
qqnorm(res5)             # normal Q-Q plot
qqline(res5)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res5, lag=40)
pacf(res5, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res5, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res5, type="o", main="residuals")
plot.ts(rstandard(arma11_cancellations.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(arma11_cancellations.mle1, robust=F)
detectAO(arma11_cancellations.mle1)
detectIO(arma11_cancellations.mle1)


arma21_cancellations.mle1 = arima(train_log_cancellations, order=c(2, 0, 1), method ="ML", xreg = 1: num_obs2)
arma21_cancellations.mle1$coef #(parameter estimates)
sqrt(diag(arma21_cancellations.mle1$var.coef))# standard errors
arma21_cancellations.mle1$sigma2 # noise variance
res6 = residuals(arma21_cancellations.mle1)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))        # set up the graphics
hist(res6, prob=TRUE, 12)   # histogram
lines(density(res6))     # smooth it - ?density for details
qqnorm(res6)             # normal Q-Q plot
qqline(res6)             # add a line
dev.new(width=7, height=7,pointsize=12)
par(mfrow=c(3,1))
acf(res6, lag=40)
pacf(res6, lag=40)
p=c(1:40)
for (i in 3:20) p[i]=Box.test (res6, lag = i, type="Ljung", fitdf=2)$p.value
pts=ts(p[3:20], start=3,frequency=1)
plot.ts(pts,ylim=c(0,1),xlab="lag",ylab="p-value", type="o", main="p-values for BOX-LJUNG test")
abline(h=0.05)
dev.new(width=9.5, height=7,pointsize=12)
par(mfrow=c(2,1))
plot.ts(res6, type="o", main="residuals")
plot.ts(rstandard(arma21_cancellations.mle1), type="o", main="standardised residuals")
abline(h=0)
#####################################
detectAO(arma21_cancellations.mle1, robust=F)
detectAO(arma21_cancellations.mle1)
detectIO(arma21_cancellations.mle1)


arma21_cancellations.mle2 = arima(train_log_cancellations, order=c(2, 0, 1),method = "ML", xreg = data.frame(1: num_obs2, 1*(seq(train_log_cancellations)==41)))
detectAO(arma21_cancellations.mle2, robust=F)
detectAO(arma21_cancellations.mle2)
detectIO(arma21_cancellations.mle2)

arma11_cancellations.mle2 = arima(train_log_cancellations, order=c(1, 0, 1),method = "ML", xreg = data.frame(1: num_obs2, 1*(seq(train_log_cancellations)==41)))
detectAO(arma21_cancellations.mle2, robust=F)
detectAO(arma21_cancellations.mle2)
detectIO(arma21_cancellations.mle2)

ma3_cancellations.mle2 = arima(train_log_cancellations, order=c(0, 0, 3),method = "ML", xreg = data.frame(1: num_obs2, 1*(seq(train_log_cancellations)==41)))
detectAO(ma3_cancellations.mle2, robust=F)
detectAO(ma3_cancellations.mle2)
detectIO(ma3_cancellations.mle2)


#############################################################################
diag1=function(xdata, fitit)
{
  n = length(xdata)
  k = length(fitit$coef)
  BIC = -2*fitit$loglik+k*log(n)
  
  AIC = -2*fitit$loglik+2*k
  AICc = AIC+2*(1+k)*(k+2)/(n-k-2)
  list(AIC=AIC, AICc=AICc, BIC=BIC)
}

diag1(train_attendance, ar1_attendance.mle1)
diag1(train_attendance, ma1_attendance.mle1)
diag1(train_attendance, arma11_attendance.mle1)
diag1(train_log_cancellations, ma3_cancellations.mle1)
diag1(train_log_cancellations, arma11_cancellations.mle1)
diag1(train_log_cancellations, arma21_cancellations.mle1)
###########################################################
#=================================================================================================================================================================
# Step 3 of the project starts here and It is based on lab 4 :)
# Plot Predictions for Attendance Models
#======================================================================

# Number of observations in the training set

num_obs <- length(train_attendance)


# ---- Attendance Predictions ----
ar1_attendance.mle1 = arima(train_attendance, order=c(1, 0, 0), method ="ML", xreg = 1: num_obs)
# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(ar1_attendance.mle1, n.ahead = 10, type = "b", newxreg = (num_obs + 1):(num_obs + 10), pch = 16, main = "AR(1) Attendance Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_attendance), train_attendance, col = "red", pch = 16)

# Predict and overlay results for Attendance
attendance_forecast1 <- predict(ar1_attendance.mle1, n.ahead = 10, newxreg = (num_obs + 1):(num_obs + 10))
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(attendance, col = 2, type = "o", pch = 16, main = "Initial Attendance series ", xlim = c(2023.6538, 2024.6346))
lines(attendance_forecast1$pred, type="p", col=1, pch=16)
plot.ts(attendance, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(attendance_forecast1$pred, type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 

ma1_attendance.mle1 = arima(train_attendance, order=c(0, 0, 1), method ="ML", xreg = 1: num_obs)
# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(ma1_attendance.mle1, n.ahead = 10, type = "b", newxreg = (num_obs + 1):(num_obs + 10), pch = 16, main = "MA(1) Attendance Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_attendance), train_attendance, col = "red", pch = 16)

# Predict and overlay results for Attendance
attendance_forecast2 <- predict(ma1_attendance.mle1, n.ahead = 10, newxreg = (num_obs + 1):(num_obs + 10))
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(attendance, col = 2, type = "o", pch = 16, main = "Initial Attendance series ", xlim = c(2023.6538, 2024.6346))
lines(attendance_forecast2$pred, type="p", col=1, pch=16)
plot.ts(attendance, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(attendance_forecast2$pred, type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 

arma11_attendance.mle1 = arima(train_attendance, order=c(1, 0, 1), method ="ML", xreg = 1: num_obs)
# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(arma11_attendance.mle1, n.ahead = 10, type = "b", newxreg = (num_obs + 1):(num_obs + 10), pch = 16, main = "ARMA(1,1) Attendance Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_attendance), train_attendance, col = "red", pch = 16)

# Predict and overlay results for Attendance
attendance_forecast3 <- predict(arma11_attendance.mle1, n.ahead = 10, newxreg = (num_obs + 1):(num_obs + 10))
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(attendance, col = 2, type = "o", pch = 16, main = "Initial Attendance series ", xlim = c(2023.6538, 2024.6346))
lines(attendance_forecast3$pred, type="p", col=1, pch=16)
plot.ts(attendance, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(attendance_forecast3$pred, type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 


# MA(3)

# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(ma3_cancellations.mle1, n.ahead = 10, type = "b", newxreg = (num_obs2 + 1):(num_obs2 + 10), pch = 16, main = "MA(3) Log Cancellation Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_log_cancellations), train_log_cancellations, col = "red", pch = 16)

# Predict and overlay results for Attendance
cancellation_forecast1 <- predict(ma3_cancellations.mle1, n.ahead = 10, newxreg = (num_obs2 + 1):(num_obs2 + 10))
exp(cancellation_forecast1$pred)
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(cancellations, col = 2, type = "o", pch = 16, main = "Initial Cancellation series ")
lines(exp(cancellation_forecast1$pred), type="p", col=1, pch=16)
plot.ts(cancellations, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(exp(cancellation_forecast1$pred), type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 



# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(arma11_cancellations.mle1, n.ahead = 10, type = "b", newxreg = (num_obs2 + 1):(num_obs2 + 10), pch = 16, main = "ARMA(1,1) Log Cancellation Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_log_cancellations), train_log_cancellations, col = "red", pch = 16)

# Predict and overlay results for Attendance
cancellation_forecast2 <- predict(arma11_cancellations.mle1, n.ahead = 10, newxreg = (num_obs2 + 1):(num_obs2 + 10))
exp(cancellation_forecast2$pred)
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(cancellations, col = 2, type = "o", pch = 16, main = "Initial Cancellation series ")
lines(exp(cancellation_forecast2$pred), type="p", col=1, pch=16)
plot.ts(cancellations, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(exp(cancellation_forecast2$pred), type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 



arma21_cancellations.mle1 = arima(train_log_cancellations, order=c(2, 0, 1), method ="ML", xreg = 1: num_obs2)

# Plot the predictions for Attendance
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot(arma21_cancellations.mle1, n.ahead = 10, type = "b", newxreg = (num_obs2 + 1):(num_obs2 + 10), pch = 16, main = "ARMA(2,1) Log Cancellation Predictions")
grid(NULL, NA, lwd = 2)  # Add gridlines
points(time(train_log_cancellations), train_log_cancellations, col = "red", pch = 16)

# Predict and overlay results for Attendance
cancellation_forecast3 <- predict(arma21_cancellations.mle1, n.ahead = 10, newxreg = (num_obs2 + 1):(num_obs2 + 10))
exp(cancellation_forecast3$pred)
dev.new(width = 9.5, height = 7, pointsize = 12)
par(mfrow = c(2, 1))  # Set up plots
plot.ts(cancellations, col = 2, type = "o", pch = 16, main = "Initial Cancellation series ")
lines(exp(cancellation_forecast3$pred), type="p", col=1, pch=16)
plot.ts(cancellations, col = 2, type = "o", pch = 16, xlim = c(2024.4615,2024.6538))
lines(exp(cancellation_forecast3$pred), type="p", col="black", pch=16)
grid(NULL, NA, lwd = 2) 


attendance_forecast1$pred
attendance_forecast2$pred
attendance_forecast3$pred
exp(cancellation_forecast1$pred)
exp(cancellation_forecast2$pred)
exp(cancellation_forecast3$pred)


########################################################################################################
erro=function(xdata, preddata)
{
  #xdata are the real values
  #preddata are the predicted values
  #xdata and preddata should have the same dimension
  n = length(preddata)
  m=length(xdata)
  e=xdata-preddata # the error
  MSE=mean(e*e)
  MPE=mean(e/xdata)
  MAE=mean(abs(e))
  MAPE=mean(abs(e/xdata))   
  list(MPE=MPE, MSE=MSE, MAE=MAE, MAPE=MAPE)
}

# Calculate Errors for Attendance Models
erro(test_attendance, attendance_forecast1$pred)
erro(test_attendance, attendance_forecast2$pred)
erro(test_attendance, attendance_forecast3$pred)

length(test_cancellations)

# Calculate Errors for Cancellations Models
erro(test_cancellations,exp(cancellation_forecast1$pred))
erro(test_cancellations, exp(cancellation_forecast2$pred))
erro(test_cancellations, exp(cancellation_forecast3$pred))


#########################################################################################################################


newxreg <- data.frame(Time = (num_obs2 + 1):(num_obs2 + 10),  AO = rep(0, 10))
# Predict next 10 observations
cancellation_forecast1.0 <- predict(ma3_cancellations.mle2, n.ahead = 10, newxreg = newxreg)
cancellation_forecast2.0 <- predict(arma11_cancellations.mle2, n.ahead = 10, newxreg = newxreg)
cancellation_forecast3.0 <- predict(arma21_cancellations.mle2, n.ahead = 10, newxreg = newxreg)



erro(test_cancellations,exp(cancellation_forecast1.0$pred))
erro(test_cancellations,exp(cancellation_forecast2.0$pred))
erro(test_cancellations,exp(cancellation_forecast3.0$pred))

























