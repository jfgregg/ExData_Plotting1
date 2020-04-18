plot1 <- function(data){
  
  ## read data from file with read.table
  
  vals <- read.table(data, header = TRUE, sep = ";")
  
  ## Convert Date with strptime
  
  date <- strptime(vals$Date,"%d/%m/%Y")
  
  ## Remove data from 2007-02-01 and 2007-02-02
  
  isfeb <- date == "2007-02-01" | date == "2007-02-02"
  
  ## select data for Global Active Power and plot in a histogram
  
  x <- as.numeric(as.character(vals$Global_active_power[isfeb == TRUE]))
  hist(x, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", breaks = 12)
  
  ## save as a png file
  
  dev.copy(png,"plot1.png")
  dev.off()

}