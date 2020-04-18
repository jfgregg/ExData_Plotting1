plot4 <- function(data){
  ## read data from file with read.table
  
  vals <- read.table(data, header = TRUE, sep = ";")
  
  ## Convert day with strptime
  
  day <- strptime(vals$Date,"%d/%m/%Y")
  
  ## Remove data from 2007-02-01 and 2007-02-02 and set parameters for graphs
  
  par(mar = c(4,4,2,2))
  par(mfcol = c(2,2))
            
  isfeb <- day == "2007-02-01" | day == "2007-02-02"
  
  ## combine Date and Time columns
  
  datetime <- as.POSIXct(paste(vals$Date[isfeb == TRUE], vals$Time[isfeb == TRUE]), format="%d/%m/%Y %H:%M:%S")
  
  ##replot plot 1
  
  x1 <- as.numeric(as.character(vals$Global_active_power[isfeb == TRUE]))
  
  plot(x1 ~ datetime, type = "l",ylab = "Global Active Power (kilowatts)",xlab = "")
  
  ##replot plot 2
  
  
  x2 <- as.numeric(as.character(vals$Sub_metering_1[isfeb == TRUE]))
  y2 <- as.numeric(as.character(vals$Sub_metering_2[isfeb == TRUE]))
  z2 <- as.numeric(as.character(vals$Sub_metering_3[isfeb == TRUE]))
  
  plot(x2 ~ datetime, type = "l",ylab = "Energy sub metering",xlab = "")
  lines(datetime, y2, type = "l", col = "red")
  lines(datetime, z2, type = "l", col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black", "red", "blue"), lty = 1, bty = "n")
  
  ##plot new graph of Voltage
  
  Voltage <- as.numeric(as.character(vals$Voltage[isfeb == TRUE]))
  plot(datetime, Voltage, type = "l")
  
  ##plot new graph of Global_reactive_power column
  
  Global_reactive_power <- as.numeric(as.character(vals$Global_reactive_power[isfeb == TRUE]))
  plot(datetime, Global_reactive_power, type = "l")
  dev.copy(png,"plot4.png")
  dev.off()
  
}