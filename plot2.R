## Coursera
## Plotting-Assignment-1-for-Exploratory-Data-Analysis
## plot 2

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

plot2 <- function(){
        
        library(tidyr)
        library(dplyr)
        
        ## The data for the project and local variables
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        destfile <- "power_consumption.zip"
        unzippedfilename <- "household_power_consumption.txt"
        pngfilename <- "plot2.png"
        
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
        # str(powerconsumption)
        
        # delete file if exists already in the working directory
        if(file.exists(pngfilename)){file.remove(pngfilename)}
       
        # open device: width=480, height=480, units='px' - this are the defaults, but just to be sure
        png(filename = pngfilename, width = 480, height = 480, units="px") 
        
        # plot required data
        with(powerconsumption, plot(Date, Global_active_power, type="n", ylab = "Global Active Power (kilowatts)", xlab = "")) 
        with(powerconsumption, lines(Date, Global_active_power, type = "l"))
        
        # close device
        dev.off()
        
        print(paste(pngfilename,"file can be found here:", getwd()))
        
}