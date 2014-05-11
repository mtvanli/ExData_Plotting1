plot3 <- function(){
    
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
    
    #set the parameters
    par(mfrow=c(1,1), # 1 plot
        cex.lab=0.8, # axis label font size
        cex.axis=0.7, # axis font size
        cex.main= 0.9, #main title font size
        #margin line for the axis title, axis labels and axis line
        mgp=c(2.5,1,0), 
        bg="transparent") #transparent background
    
    #set plot 3 margins
   # par(mar=c(5,4,1.5,1))
    
    #Plot 3
   
   par(mar=c(2,4,1,1))
    with(data, {
        plot(Sub_metering_1~ DateTime,
             type="n",
             ylab="Energy sub metering"
             ,xlab="",
             cex.lab=0.8,cex.axis=0.8)
        lines(Sub_metering_1~ DateTime)
        lines(Sub_metering_2~ DateTime,col="red")
        lines(Sub_metering_3~ DateTime, col="blue")
        legend("topright", 
               col=c("black","blue","red"),
               lty=c(1,1,1),
               cex=0.6, #legend text size
               xjust=0, 
               yjust=0, 
               y.intersp=0.7, #vertical spacing
               text.width=35000, #text width
               legend=c("Sub_metering_1", 
                        "Sub_metering_2" ,
                        "Sub_metering_3"))
    })
    
    
    dev.copy(png, file = "plot3.png") ## Copy plot3 to a PNG file
    dev.off() #turn off png device
}