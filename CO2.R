setwd("~/")

# Read data
data=read.csv("co2.csv",header=TRUE)
# Time series plots
CO2=ts(data[,2:4],frequency = 1,start=1960)
plot(CO2, main="CO2 emission and Oil Production")

data=data[1:30,] # Using the first 30 observations
#data=data # Using the whole sample

# Define data frame
data1=data.frame(time=c(data$Time,data$Time),
                 oil=c(data$OPEC,data$OPEC),
                 co2=c(data$USA,data$Saudi),
                 group=c(rep("USA",nrow(data)),
                         rep("SAUDI",nrow(data))))

# Define dummy variables
data1$d2=ifelse(data1$time >= 1973,1,0)
data1$dt=ifelse(data1$group == "USA",1,0)
data1$did=data1$dt * data1$d2
data1$did_oil =data1$did * log(data1$oil)

# DID regression
M=lm(log(co2) ~ log(oil)+ d2 + dt + did + did_oil, data=data1)
summary(M)

