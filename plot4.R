## Coursera
## Plotting-Assignment-1-for-Exploratory-Data-Analysis
## plot 4

# Download and unzip source data function
downloadsource <- function(fileurl, destfile, unzippedfilename) {
        ## local variables
        datafolder <- "./data"
        destfile <- paste0(datafolder, "/", destfile)
        
        ## create data directory if not exists
        if(!file.exists(datafolder)){dir.create(datafolder)}
        
        ## download a zip file containing raw data and save it to the data directory from working directory
        ## method="wininet" for windows system
        if(!file.exists(destfile)) {
                download.file(fileurl, destfile = destfile, method="wininet")
                
                ## print when the file was download:
                print(paste("zip file downloaded on:", 
                            format(Sys.time(), "%b %d, %Y %X"),
                            "from:", fileurl))
        }
        
        # unzip the file if not exists
        if (!file.exists(unzippedfilename)) { 
                unzip(destfile)
        }
}

plot4 <- function(){
        
        library(tidyr)
        library(dplyr)
        
        ## The data for the project and local variables
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        destfile <- "power_consumption.zip"
        unzippedfilename <- "household_power_consumption.txt"
        pngfilename <- "plot4.png"
        
        ## download and unzip file
        downloadsource(fileurl, destfile, unzippedfilename)
        
        print("Step 1 -> Read the source data")
        colClasses <- c(rep("character",2), rep("numeric", 7))
        powerconsumption <- read.table("household_power_consumption.txt", colClasses = colClasses, sep = ";",
                                       na.strings ="?", stringsAsFactors = FALSE, header = TRUE) %>%  
                subset(Date == "1/2/2007" | Date == "2/2/2007")
        
        ##Convert date to Date time
        powerconsumption$Date <- strptime(paste(powerconsumption$Date, powerconsumption$Time), "%d/%m/%Y %H:%M:%S")
        
        ##Overview of the data - for first analysis
        #str(powerconsumption)
        
        # delete file if exists already in the working directory
        if(file.exists(pngfilename)){file.remove(pngfilename)}
        
        # open device: width=480, height=480, units='px' - this are the defaults, but just to be sure
        png(filename = pngfilename, width = 480, height = 480, units="px") 
        
        # plot required data
        par(mfcol=c(2,2), mar = c(4,4,2,1))
        with(powerconsumption, { plot(Date, Global_active_power, type="n", ylab = "Global Active Power", xlab = "") 
                                lines(Date, Global_active_power, type = "l")
        })
        
        with(powerconsumption,{
                plot(Date, Sub_metering_1, type="n", ylab = "Energy sub metering", xlab = "")
                lines(Date, Sub_metering_1, type = "l")
                lines(Date, Sub_metering_2, type = "l", col = "red")
                lines(Date, Sub_metering_3, type = "l", col = "blue")
        })
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               pch = c(NA, NA, NA), lty=c(1,1,1), lwd = c(2,2,2),
               col = c("black", "red", "blue"))
        
        with(powerconsumption, {plot(Date, Voltage, type="n", ylab = "Voltage", xlab = "datetime")
             lines(Date, Voltage, type = "l")
        })
        
        with(powerconsumption, { plot(Date, Global_reactive_power, type="n", ylab = "Global_reactive_power", xlab = "datetime") 
                lines(Date, Global_reactive_power, type = "l")
        })
        
        # close device
        dev.off()
        
        print(paste(pngfilename,"file can be found here:", getwd()))
        
}