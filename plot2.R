plot2 <- function(data){
  
  ## read data from file with read.table
  
  vals <- read.table(data, header = TRUE, sep = ";")
  
  ## Convert day with strptime
  
  day <- strptime(vals$Date,"%d/%m/%Y")
  
  ## Remove data from 2007-02-01 and 2007-02-02
  
  isfeb <- day == "2007-02-01" | day == "2007-02-02"
  
  ## combine Date and Time columns
  
  datetime <- as.POSIXct(paste(vals$Date[isfeb == TRUE], vals$Time[isfeb == TRUE]), format="%d/%m/%Y %H:%M:%S")

  ## select Global Active Power variable
  
  x <- as.numeric(as.character(vals$Global_active_power[isfeb == TRUE]))
  
  ##plot graph and copy to a png file
  
  par(mar = c(2,4,2,1))
  plot(x ~ datetime, type = "l",ylab = "Global Active Power (kilowatts)",xlab = "")
  dev.copy(png,"plot2.png")
  dev.off()
  
}