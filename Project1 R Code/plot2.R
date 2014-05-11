plot2 <- function(){
    
    #read the data
    data <- read.table("household_power_consumption.txt",
                       sep =";",
                       header=TRUE,
                       colClasses="character",
                       na.strings="?")
    
    #convert character to date
    data[,1]<-as.Date(data[,1],"%d/%m/%Y")
    
    #subset the dates as per rquirements
    data <- subset(data,data[,1] >="2007-02-01" & data[,1] 
                   <="2007-02-02")
    
    DateTime <- strptime(paste(data[,1],data[,2]),"%Y-%m-%d 
                         %H:%M:%S")
    
    #convert the other columns to numeric
    data[,c(-1,-2)]<-sapply(data[,c(-1,-2)],as.numeric)
    
    #Bind the new DateTime column 
    ## and drop the old Date and Time columns
    data<-cbind(DateTime,data[,c(-1,-2)])
    
    #set the parameter
    par(mfrow=c(1,1), # 1 plot
        cex.lab=0.8, # axis label font size
        cex.axis=0.7, # axis font size
        cex.main= 0.9, #main title font size
        mgp=c(2.5,1,0), #margin sizes
        bg="transparent") #transparent background
    
    #set plot 4.1 margins
    par(mar=c(5,4,1.5,1))
    
    #Plot 2
    with(data, {
        plot(Global_active_power~ DateTime,
             type="n",
             ylab="Global Active Power(kilowatts)",
             xlab="")
        lines(Global_active_power~ DateTime)
    })
    
    dev.copy(png, file = "plot2.png") ## Copy plot2 to a PNG file
    dev.off() #turn off png device
}