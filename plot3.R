plot3 <- function(data){
  
  ## read data from file with read.table
  
  vals <- read.table(data, header = TRUE, sep = ";")
  
  ## Convert day with strptime
  
  day <- strptime(vals$Date,"%d/%m/%Y")
  
  ## Remove data from 2007-02-01 and 2007-02-02
  
  isfeb <- day == "2007-02-01" | day == "2007-02-02"
  
  ## combine Date and Time columns
  
  datetime <- as.POSIXct(paste(vals$Date[isfeb == TRUE], vals$Time[isfeb == TRUE]), format="%d/%m/%Y %H:%M:%S")
  
  ## select variables from sub-metering columns
  
  x <- as.numeric(as.character(vals$Sub_metering_1[isfeb == TRUE]))
  y <- as.numeric(as.character(vals$Sub_metering_2[isfeb == TRUE]))
  z <- as.numeric(as.character(vals$Sub_metering_3[isfeb == TRUE]))
  
  ##plot graph and copy to a png file
  
  par(mar = c(2,4,2,1))
  plot(x ~ datetime, type = "l",ylab = "Energy sub metering",xlab = "")
  lines(datetime, y, type = "l", col = "red")
  lines(datetime, z, type = "l", col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
  dev.copy(png,"plot3.png")
  dev.off()
  
}