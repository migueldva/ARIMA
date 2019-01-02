library(readxl)
library(tidyverse)
library(reshape2)
library(forecast)
library(xts)
library(polynom)


#Require the data
path2 <- ""
flights_domesc <- read_excel(path = paste0(path2,"/Base_File.xlsx"), sheet = 1)
flights_int <- read_excel(path = paste0(path2,"/Base_File.xlsx"), sheet = 2)
pax_domesc <- read_excel(path = paste0(path2,"/Base_File.xlsx"), sheet = 3)
pax_int <- read_excel(path = paste0(path2,"/Base_File.xlsx"), sheet = 4)


#Required function to read the series
convert_ts <- function(ts_name,
                       type.1 = c("route", "airport", "airport_group"),
                       type.2 = c("international", "domestic", "consolidated"),
                       type.3 = c("passengers", "flights"),
                       start.date = as.Date("2001-01-01", format = "%Y-%m-%d"),
                       end.date = as.Date("2018-12-01", format = "%Y-%m-%d")){
  
  #Prepare the  desired data
  if(type.3 == "passengers"){
    data.aux.d <- pax_domesc
    data.aux.i <- pax_int
  } else if(type.3 == "flights") {
    data.aux.d <- flights_domesc
    data.aux.i <- flights_int
  }
  
  
  if(type.2 == "consolidated"){
    data.aux <- rbind(data.aux.d, data.aux.i)
  } else if (type.2 == "international"){
    data.aux <- data.aux.i
  } else if(type.2 == "domestic"){
    data.aux <- data.aux.d
  }
  
  if(type.1 == "route"){
    ts_name.f <- strsplit(ts_name, split = "-")[[1]][1]
    ts_name.t <- strsplit(ts_name, split = "-")[[1]][2]
    aux.1 <- min(ts_name.f, ts_name.t)
    aux.2 <- max(ts_name.f, ts_name.t)
    ts_name <- paste0(aux.1,"-",aux.2)
    if(ts_name.t < ts_name.t){
      print(paste("Warning: variable name has been renamed to", ts_name))
    }
    ts <- data.aux %>% filter((`Origin (IATA)` == aux.1 & `Destination (IATA)` == aux.2) |
                                (`Origin (IATA)` == aux.2 & `Destination (IATA)` == aux.1))
    
  } else if(type.1 == "airport"){
    ts <- data.aux %>% filter(`Origin (IATA)` == ts_name | `Destination (IATA)` == ts_name)
    
  } else if (type.1 == "airport_group"){
    ts <- data.aux %>% filter(`Origin Airport Group` == ts_name | `Destination Airport Group` == ts_name)
    ts.aux <- data.aux %>% filter(`Origin Airport Group` == ts_name & `Destination Airport Group` == ts_name)
    ts <- rbind(ts, ts.aux)
  }
  
  #create the series
  ts <- ts %>% group_by(Year = Year) %>%
    summarise_at(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                   "Oct", "Nov", "Dec"), .funs = sum) %>%
    melt(id.vars = "Year")
  #Structure the dates
  start.date <- as.Date(start.date, format = "%Y-%m-%d")
  end.date <- as.Date(end.date, format = "%Y-%m-%d")
  ts$date <- as.factor(paste0(as.character(ts$Year), "-", as.character(ts$variable), "-01"))
  ts$date <- as.Date(ts$date, format = "%Y-%b-%d")
  
  #Remove NA values
  ts <- na.omit(ts)
  
  #Create time series object
  ts <- xts(ts[,3], order.by = ts$date)
  aux.txt <- paste0(as.character(start.date),"/",as.character(end.date))
  ts <- ts[aux.txt]
  #print(plot(ts, main = paste0(ts_name,"'s ", type.2, " ", type.3)))
  return(ts)
  
}


#Load OMA's passenger traffic until 2008

oma.2009 <- convert_ts("OMA", type.1 = "airport_group", type.2 = "consolidated",
                       type.3 = "passengers",
                       start.date = "2001-01-01", end.date = "2008-12-01")


ggplot(oma.2009) + geom_line(aes(x = index(oma.2009), y = coredata(oma.2009))) +
  labs(title = "OMA's consolidated passengers (2001 - 2008)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits =   c(as.Date("2000-12-01", format = "%Y-%m-%d"), NA)) +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 8))

#Transform the series and choose the appropriate amount of differences 


lambda <- BoxCox.lambda(oma.2009)
t.oma.2009 <- oma.2009^lambda
acf.t.oma.2009 <- acf(na.omit(diff(t.oma.2009, lag = 1)), lag.max = 30, main = "Transformed series' ACF")


ggplot(t.oma.2009) + geom_line(aes(x = index(t.oma.2009), y = coredata(t.oma.2009))) +
  labs(title = "OMA's transformed series (2001 - 2008)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits =   c(as.Date("2000-12-01", format = "%Y-%m-%d"), NA)) +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 8))


s <- matrix(ncol = 3, nrow = 3)

S <- function(data, j, i, E){
  if(i == 0 && j == 0){
    s[j+1,i+1] <<- sd(data)
  } else if (i != 0 && j == 0){
    s[j+1,i+1] <<- sd(diff(data, lag = E, differences = i), na.rm = T)
  } else if (i == 0 && j != 0){
    s[j+1,i+1] <<- sd(diff(data, differences = j), na.rm = T)
  } else 
    s[j+1,i+1] <<- sd(diff(diff(data, lag = E, differences = i), differences = j), na.rm = T)
}

#For E = 6
for (i in 0:2){
  for(j in 0:2){
    S(t.oma.2009, j, i, 6)
  }
}

s_6 <- s
s_6 <- as.data.frame(s_6, row.names = c("0","1","2"))
names(s_6) <- c("0","1","2")


#For E = 12
for (i in 0:2){
  for(j in 0:2){
    S(t.oma.2009, j, i, 12)
  }
}


s_12 <- s
s_12 <- as.data.frame(s_12, row.names = c("0","1","2"))
names(s_12) <- c("0","1","2")


#We now difference the series and determine the order of our ARIMA model


t.oma.2009.d <- diff(diff(t.oma.2009, differences = 1), differences = 1, lag = 12) %>% na.omit()

ggplot(t.oma.2009.d) + geom_line(aes(x = index(t.oma.2009.d), y = coredata(t.oma.2009.d))) +
  labs(title = "OMA's transformed series (2001 - 2008)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits =   c(as.Date("2000-12-01", format = "%Y-%m-%d"), NA)) +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 8))

acf(t.oma.2009.d, lag.max = 35, main = "Transformed series' ACF")
pacf(t.oma.2009.d, lag.max = 35, main = "Transformed series' PACF")


oma.2009.fit <- Arima(t.oma.2009, order = c(0,1,12), seasonal = list(order = c(1,1,0), period = 12), 
                      fixed = c(NA, rep(0,9), NA, NA, NA), method = "CSS")

aux <- data.frame(date = index(oma.2009), Series = coredata(oma.2009.fit$fitted), type = "Simulated")
oma.2009.df <- data.frame(date = index(oma.2009), Series = coredata(t.oma.2009), type = "Observed")

oma.2009.df <- rbind(oma.2009.df, aux)

ggplot(oma.2009.df, aes(x = date, y = Series, color = type)) +
  geom_line(size = 1.2, aes(linetype = type)) +
  labs(title = "Observed vs. fitted") +
  scale_color_manual(values = c("#E2A206", "#067DE2")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits =   c(as.Date("2000-12-01", format = "%Y-%m-%d"), NA)) +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 8))


#Verifying the model's assumptions

#1 Mean equals zero

oma.2009.fit$t_residuals <- oma.2009.fit$residuals[oma.2009.fit$residuals != 0]
mean(oma.2009.fit$t_residuals)
sd(oma.2009.fit$t_residuals)
# Además, calculamos el cociente sqrt(N-d-p-(D+P)E)
abs(sqrt(length(oma.2009) - 1 - 0 -(1+1)*12)*mean(oma.2009.fit$t_residuals)/sd(oma.2009.fit$t_residuals))


#2Constant variance

residual_df <- data.frame(t = 1:length(oma.2009.fit$t_residuals), residual = oma.2009.fit$t_residuals)

ggplot(residual_df) + geom_line(aes(x = t, y = residual), color = "#E2A206") +
  geom_hline(yintercept = 2*sd(oma.2009.fit$t_residuals), linetype="dashed", color = "#067DE2") +
  geom_hline(yintercept = -2*sd(oma.2009.fit$t_residuals), linetype="dashed", color = "#067DE2") +
  geom_hline(yintercept = 3*sd(oma.2009.fit$t_residuals), linetype="dashed", color = "#E83426") +
  geom_hline(yintercept = -3*sd(oma.2009.fit$t_residuals), linetype="dashed", color = "#E83426") +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.text = element_text(size = 8))

#3 Residuals are mutually independent


acf(oma.2009.fit$t_residuals)

Box.test(oma.2009.fit$t_residuals, lag = 24, type = "Ljung-Box")


#4 Residuals follow a normal distribution

ggplot(residual_df, aes(x = residual)) + geom_histogram(aes(y = ..density..),
                                                        breaks = seq(-.005, .005, by = .0008), 
                                                        colour = "#E2A206", 
                                                        fill = "#E2A206",
                                                        alpha = 0.8,
                                                        binwidth = 0.3) +
  stat_function(fun = dnorm, args = list(mean = mean(residual_df$residual), 
                                         sd = sd(residual_df$residual)),
                color = "#067DE2", size = 1, linetype = "dashed") +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.text = element_text(size = 8))


qqnorm(oma.2009.fit$t_residuals)
qqline(oma.2009.fit$t_residuals)


###Static estimation


oma.2009.static <- forecast(oma.2009.fit, h = 12, level = c(65,95))


oma.2009.static.df <- data.frame(date = index(oma.2009), pax = coredata(t.oma.2009), type = "observed")
preds.static <- data.frame(date =  seq.Date(from = as.Date("2009-01-01", format = "%Y - %m - %d"), 
                                   by = "1 month", to = as.Date("2009-12-01", format = "%Y - %m - %d")),
                  pax = oma.2009.static$mean, type = "predicted")
oma.2009.static.df <- rbind(oma.2009.static.df, preds.static)


###Transforming back the series 


xi <- vector()
xi[1] <- -1
theta <- c(oma.2009.fit$coef[-13], rep(0,13))
phi <- c(-1, rep(0,10), 0.717616, -0.717616, rep(0,10), 0.282384, -0.282384)


for(i in 2:25){
  xi[i] <- theta[i-1]
  for (j in 1:(i-1)){
    aux <- phi[j]*xi[i-j]
  }
  xi[i] <- xi[i] + aux
}

var.eh <- vector()
var.eh[1] <- (xi[1]^2)*var(oma.2009.fit$t_residuals)
for(i in 2:12){
  var.eh[i] <- var.eh[i-1] + (xi[i]^2)*(var(oma.2009.fit$t_residuals))
}


c_lambda <- vector()
aux.pred <- as.vector(preds.static$pax)
for(i in 1:12){
  aux.2 <- aux.pred[i]
  c_lambda[i] <- (.5 + sqrt(1 - 2*lambda*(lambda - 1)*((1 + lambda*aux.2)^(-2))*var.eh[i])*0.5)^(1/lambda)
}



oma.2009.basis <- convert_ts("OMA", type.1 = "airport_group", type.2 = "consolidated",
                       type.3 = "passengers",
                       start.date = "2009-01-01", end.date = "2009-12-01")

oma.2009.basis <- data.frame(date = index(oma.2009.basis), pax = coredata(oma.2009.basis), type = "observed")

pred.df <- data.frame(date = oma.2009.basis$date, pred = as.vector(oma.2009.static$mean), 
                      lower = as.vector(oma.2009.static$lower[,2]), upper = as.vector(oma.2009.static$upper[,2])) 

pred.df[,-1] <- (pred.df[,-1]^(1/lambda))*c_lambda

pred.df <- cbind(pred.df, pax = oma.2009.basis$pax)


ggplot(pred.df) +
  geom_line(aes(x = date, y = pred), color = "#E2A206", size = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = date), alpha = 0.2, fill = "#E2A206") +
  geom_line(aes(x = date, y = pax), color = "#067DE2", size = 1.1) +
  geom_text(aes(x = as.Date("2009-02-01"), y = 1500000, label = "Forecast"), color = "#E2A206", size = 6) +
  geom_text(aes(x = as.Date("2009-02-01"), y = 1450000, label = "Observed"), color = "#067DE2", size = 6) +
  labs(x = "Date", y = "Passengers", title = "") +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.text = element_text(size = 8))

write.csv(file = paste0(path2, "/pred.csv"), pred.df)


#Dynamic estimation

t.oma.2009.update <- rbind(t.oma.2009, (convert_ts("OMA", type.1 = "airport_group", type.2 = "consolidated",
                                                   type.3 = "passengers",
                                                   start.date = "2009-01-01", end.date = "2009-12-01"))^lambda)

oma.2009.fit.updated <- Arima(t.oma.2009.update["/2009-01"], model = oma.2009.fit)

oma.2009.fit.updated <- Arima(t.oma.2009.update["/2008"], model = oma.2009.fit)
oma.2009.dynamic <- forecast(oma.2009.fit.updated, h = 1, level = 95)

oma.2009.dynamic.df <- data.frame(mean = c(oma.2009.dynamic$mean,rep(0,11)),
                                  lower_bound = c(oma.2009.dynamic$lower,rep(0,11)),
                                  upper_bound = c(oma.2009.dynamic$upper,rep(0,11)))



for(i in 2:12){
  
  if(i == 6){
    t.oma.2009.update["2009-05-01"] <- oma.2009.dynamic.df[5, 1]
  }
  
  oma.2009.fit.updated <- Arima(t.oma.2009.update[paste0("/2009-",i-1)], model = oma.2009.fit)
  oma.2009.dynamic <- forecast(oma.2009.fit.updated, h = 1, level = 95)
  
  oma.2009.dynamic.df[i, 1] <- as.numeric(oma.2009.dynamic$mean)
  oma.2009.dynamic.df[i, 3] <- oma.2009.dynamic$upper
  oma.2009.dynamic.df[i, 2] <- oma.2009.dynamic$lower
  
}


oma.2009.dynamic.df <- (oma.2009.dynamic.df^(1/lambda))*c_lambda[1] 

oma.2009.dynamic.df <- cbind(index(oma.2009.basis), oma.2009.dynamic.df, coredata(oma.2009.basis))

names(oma.2009.dynamic.df) <- c("date", "pred", "lower", "upper", "pax")

ggplot(oma.2009.dynamic.df) +
  geom_line(aes(x = date, y = pred), color = "#E2A206", size = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = date), alpha = 0.2, fill = "#E2A206") +
  geom_line(aes(x = date, y = pax), color = "#067DE2", size = 1.1) +
  geom_text(aes(x = as.Date("2009-02-01"), y = 1300000, label = "Forecast"), color = "#E2A206", size = 6) +
  geom_text(aes(x = as.Date("2009-02-01"), y = 1250000, label = "Observed"), color = "#067DE2", size = 6) +
  labs(x = "Date", y = "Passengers", title = "") +
  theme(panel.background = element_rect(fill = "whitesmoke"),
        axis.text = element_text(size = 8))

write.csv(file = paste0(path2, "/pred2.csv"), oma.2009.dynamic.df)
