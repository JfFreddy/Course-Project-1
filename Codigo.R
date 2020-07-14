## Paso 1 ## Código para leer en el conjunto de datos 

file <- read.csv2("C:/Users/fgomez/Desktop/Cursos Coursera/Exploratorio de datos/household_power_consumption.txt")

file$Date <- as.Date(file$Date, format="%d/%m/%Y")

activity <- file[(file$Date=="2007-02-01") | (file$Date=="2007-02-02"),]

activity$Global_active_power <- as.numeric(as.character(activity$Global_active_power))

activity$Global_reactive_power <- as.numeric(as.character(activity$Global_reactive_power))

activity$Voltage <- as.numeric(as.character(activity$Voltage))

activity <- transform(activity, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")

activity$Sub_metering_1 <- as.numeric(as.character(activity$Sub_metering_1))

activity$Sub_metering_2 <- as.numeric(as.character(activity$Sub_metering_2))

activity$Sub_metering_3 <- as.numeric(as.character(activity$Sub_metering_3))



# Plot 1

plot1 <- function() {
  hist(activity$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot1()


# Plot 2

plot2 <- function() {
  plot(activity$timestamp,activity$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
  cat("plot2.png has been saved in", getwd())
}
plot2()

# Plot 3

plot3 <- function() {
  plot(activity$timestamp,activity$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(activity$timestamp,activity$Sub_metering_2,col="red")
  lines(activity$timestamp,activity$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
  cat("plot3.png has been saved in", getwd())
}
plot3()

# Plot 4

plot4 <- function() {
  par(mfrow=c(2,2))
  
  ##PLOT 1
  plot(activity$timestamp,activity$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  ##PLOT 2
  plot(activity$timestamp,activity$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  ##PLOT 3
  plot(activity$timestamp,activity$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(activity$timestamp,activity$Sub_metering_2,col="red")
  lines(activity$timestamp,activity$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
  
  #PLOT 4
  plot(activity$timestamp,activity$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
  
  #OUTPUT
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()
  cat("plot4.png has been saved in", getwd())
}
plot4()

