## Analyzing the juvenile data using logistic rgeression
## Mark A. Hayes 
## 5/10/2017

remove()
rm(list=ls())


juv_data = read.table(file.choose(), header=TRUE, sep=",") # This is the Juvenile proportion data 

summary(juv_data)

## Visualizing the juvenile proportion data

hist(juv_data$Number)
hist(juv_data$F_proportion, main="Proportion Female", xlab="Proportion")
hist(juv_data$Temp)
hist(juv_data$Precip)

## Histogram of proportion female and a density kernel
par(mfrow=c(1,1))
X <- juv_data$F_proportion
hist(X, main = "", xlab="Proportion Female by Year")
lines(density(X)) # add a density estimate with defaults
grid()

dev.print(tiff, "figure_hist.tiff", height=4, width=6.5, units='in', res=300)


## Logistic regression analysis

data = read.table(file.choose(), header=TRUE, sep=",") # This is the LR data

summary(data)

## The simple temp + precip model

model1 <- glm(female ~ temp + precip, data=data, family = binomial())
summary(model1)

## The same model with interactions

model2 <- glm(female ~ temp*precip, data=data, family = binomial())
summary(model2)

coef(model2)
exp(coef(model2))

## Model2 looks nice and has all parameters at > 0.05*

# Looking at overdispersion
# psi = Residual deviance / residual df
# Residual deviance: 442.76  on 340  degrees of freedom

442.76/340

# 1.3. If considerably larger than 1.00, then may be overdispersed. 

# Using the quasibinomial model to further test for overdispersion, following Kabakoff pp 322-323:

fit <- glm(female ~ temp*precip, data=data, family = binomial())

fit.od <- glm(female ~ temp*precip, data=data, family = quasibinomial())

pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual, lower = F)


## The resulting p-value (0.43) is clearly not significant (p > 0.05), strengthening our inference that overdispersion isn't a problem. 

## Assessing the impact of predictors on the probability of an outcome
## Kabacoff pp. 321

## varying temp:

testdata <- data.frame(temp=c(9:15), precip=mean(data$"precip"))
testdata

testdata$prob <- predict(fit, newdata=testdata, type="response")
testdata
plot(testdata$temp,testdata$prob, xlab="Temperature", ylab="Probability of Female", type="l", ylim=c(0,1))


# Varying precip

testdata2 <- data.frame(precip=c(300:1000), temp=mean(data$"temp"))
testdata2

testdata2$prob <- predict(fit, newdata=testdata2, type="response")
testdata2

plot(testdata2$precip,testdata2$prob, xlab="Precipitation", ylab="Probability of Female", type="l", ylim=c(0,1))

## Plotting side by side

par(mfrow=c(1,2))
plot(testdata$temp,testdata$prob, xlab="Temperature", ylab="Probability of Female", type="l", ylim=c(0,1), cex.axis = 0.75)
plot(testdata2$precip,testdata2$prob, xlab="Precipitation", ylab="Probability of Female", type="l", ylim=c(0,1), cex.axis = 0.75)
dev.print(tiff, "figure_temp_precip_plots.tiff", height=4, width=6.5, units='in', res=300)
par(opar)


## Now creating dataframes for future climate conditions and probability of female using the fit model:

rcp26 = read.table(file.choose(), header=TRUE, sep=",") # This is the RCP26 temp & precip data (C, mm) 
summary(rcp26)

rcp45 = read.table(file.choose(), header=TRUE, sep=",") # This is the RCP45 temp & precip data (C, mm) 
summary(rcp45)

rcp60 = read.table(file.choose(), header=TRUE, sep=",") # This is the RCP60 temp & precip data (C, mm) 
summary(rcp60)

rcp85 = read.table(file.choose(), header=TRUE, sep=",") # This is the RCP85 temp & precip data (C, mm) 
summary(rcp85)

juv_ave = read.table(file.choose(), header=TRUE, sep=",") # This is the Juvenile average proportion = 0.4025 

## Making predictions from these dataframes using fit model

## RCP2.6

rcp26data <- data.frame(year=rcp26$year, precip=rcp26$precip, temp=rcp26$temp)
rcp26data$prob26 <- predict(fit, newdata=rcp26data, type="response")
rcp26data

plot(rcp26data$year,rcp26data$prob, xlab="Year", ylab="Probability of Female", type="l", ylim=c(0,1), main = "RCP2.6")


## RCP4.5

rcp45data <- data.frame(year=rcp45$year, precip=rcp45$precip, temp=rcp45$temp)
rcp45data$prob45 <- predict(fit, newdata=rcp45data, type="response")
rcp45data

plot(rcp45data$year,rcp45data$prob, xlab="Year", ylab="Probability of Female", type="l", ylim=c(0,1), main = "RCP4.5")

## RCP6.0

rcp60data <- data.frame(year=rcp60$year, precip=rcp60$precip, temp=rcp60$temp)
rcp60data$prob60 <- predict(fit, newdata=rcp60data, type="response")
rcp60data

plot(rcp60data$year,rcp60data$prob, xlab="Year", ylab="Probability of Female", type="l", ylim=c(0,1), main = "RCP6.0")


## RCP8.5

rcp85data <- data.frame(year=rcp85$year, precip=rcp85$precip, temp=rcp85$temp)
rcp85data$prob85 <- predict(fit, newdata=rcp85data, type="response")
rcp85data

plot(rcp85data$year,rcp85data$prob, xlab="Year", ylab="Probability of Female", type="l", ylim=c(0,1), main = "RCP8.5")


## Combining the predictions from the 4 RCP predictions. df = data.frame

df <- data.frame(year=rcp26$year, rcp26 = rcp26data$prob26, rcp45 = rcp45data$prob45, rcp60 = rcp60data$prob60, rcp85 = rcp85data$prob85)
df




## Plotting all of the RCP projections together

# first plot

par(mfrow=c(1,1))
plot(df$year,df$rcp26, xlab="Year", ylab="Probability of Female", type="l", col = "black", ylim=c(0.25,0.45), main = "")

# second plot  EDIT: needs to have same ylim

par(new = TRUE)
plot(df$year,df$rcp45, type="l", ylim=c(0.25,0.45), col = "blue", axes = FALSE, xlab = "", ylab = "")

par(new = TRUE)
plot(df$year,df$rcp60, type="l", ylim=c(0.25, 0.45),col = "green", axes = FALSE, xlab = "", ylab = "")


par(new = TRUE)
plot(df$year,df$rcp85, type="l", ylim=c(0.25, 0.45),col = "red", axes = FALSE, xlab = "", ylab = "")

par(new = TRUE)
plot(juv_ave$year, juv_ave$females, type="l", ylim=c(0.25, 0.45),lty=2, axes = FALSE, xlab = "", ylab = "")


grid()

legend("bottomleft",legend = c("1996-2015","RCP2.5", "RCP4.5", "RCP6.0", "RCP8.5"),lty=c(2,1,1,1,1),col=c("black", "black","blue","green", "red"),bg="white", cex=0.75)

dev.print(tiff, "figure_rcp.tiff", height=4, width=6.5, units='in', res=300)





