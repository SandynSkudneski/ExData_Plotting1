# exploratory_project1.R

# Copyright statement: none
# Author comment: none
# File description
#   purpose: Create plots from UC Irvine Machine Learning Repository dataset.
#             
#   inputs:  Electric power consumption [126MB unzipped]
#             - 1 table, 2,075,259 rows, 9 columns
#   outputs: plots
#            
#            
# Function definitions: none

library(dplyr)
library(lubridate)

setwd("~/R/Coursera/Exploratory/Week1/Project/")
f <- file.path(getwd(), "household_power_consumption.txt")
dat.dir <- file.path(getwd())

# for auto download and unzipif desired
# url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
# download.file(url, f) 
# unzip(f)

# load data, convert dates and subset for smaller working dataset (dat1)
dat <- read.table(f, header = TRUE, sep = ";", stringsAsFactors = FALSE)
dat$Date  <- as.Date(dat$Date, "%d/%m/%Y")
dat1 <- dat[dat$Date == "2007-02-01" | dat$Date == "2007-02-02", ]
# add DateTime column to help with plotting and convert classes on columns
dat1 <- mutate(dat1, DateTime = as.character(paste(Date, Time)))
dat1$DateTime <- ymd_hms(dat1$DateTime)
dat1[, 3:9] = apply(dat1[, 3:9], 2, function(x) as.numeric(as.character(x)))

# tested for "?" characters, it returned 0
# grep("\\<?\\>", dat1)

# Plot how household energy usage varies over a 2-day period in February, 2007

# Plot1: Active power in kw vs frequency of occurance
hist(dat1$Global_active_power, main = "Global Active Power", 
              xlab = "Global Active Power (kilowatts)", ylab = "Frequency",
              col = "red")
dev.copy(png, paste(getwd(), "/", "plot1.png", sep = ""))
dev.off()

# Plot2: DateTime @ 2 minute intervals vs Active power in kw
plot(dat1$DateTime, dat1$Global_active_power, main = "", type = "l" , 
      xlab = "", ylab = "Global Active Power (kilowatts)")
dev.copy(png, paste(getwd(), "/", "plot2.png", sep = ""))
dev.off()

# Plot3: DateTime @ 2 minute intervals vs Energy sub metering (kWh)
par(mar=c(1, 1, 1, 1))
plot(dat1$DateTime, dat1$Sub_metering_1,  type = "l", 
    main = "", xlab = "", ylab = "Energy sub metering", 
    ylim=range(dat1$Sub_metering_1))
    lines(dat1$DateTime, dat1$Sub_metering_2, col = "red")
    lines(dat1$DateTime, dat1$Sub_metering_3, col = "blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty=c(1,1,1), col=c(1, 2, 4))
dev.copy(png, paste(getwd(), "/", "plot3.png", sep = ""))
dev.off()

# Plot4: DateTime @ 2 minute intervals vs Energy sub metering (kWh)
par(mfrow = c(2, 2), pty = "s", mar=c(2, 2, 2, 2))
plot(dat1$DateTime, dat1$Global_active_power, main = "", type = "l" , 
     xlab = "", ylab = "Global Active Power")
plot(dat1$DateTime, dat1$Voltage, main = "", type = "l" , 
     xlab = "datetime", ylab = "Voltage")

plot(dat1$DateTime, dat1$Sub_metering_1,  type = "l", 
     main = "", xlab = "", ylab = "Energy sub metering", 
     ylim=range(dat1$Sub_metering_1))
lines(dat1$DateTime, dat1$Sub_metering_2, col = "red")
lines(dat1$DateTime, dat1$Sub_metering_3, col = "blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty=c(1,1,1), col=c(1, 2, 4), bty = "o")
plot(dat1$DateTime, dat1$Global_reactive_power, main = "", type = "l" , 
     xlab = "datetime", ylab = "Global_reactive_power")
dev.copy(png, paste(getwd(), "/", "plot4.png", sep = ""))
dev.off()
