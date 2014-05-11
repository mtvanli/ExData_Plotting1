plot4 <- function(){
    
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
    par(mfrow=c(2,2),
        cex.lab=0.8,
        cex.axis=0.7,
        mgp=c(2.5,1,0),
        bg="transparent")
    
    #set plot 4.1 margins
    par(mar=c(5,4,1,1))
    #plot 4.1
    with(data, {  
        plot(Global_active_power~ DateTime,
             type="n",
             ylab="Global Active Power"
             ,xlab="")
        lines(Global_active_power~ DateTime)
    })
    
    #set plot 4.2 margins
    par(mar=c(5,4,1,1))
    #plot 4.2
    with(data, {
        plot(Voltage~DateTime,
             type="n",
             ylab="Voltage",
             xlab="datetime")
        lines(Voltage~DateTime)
    })
    
    #set plot 4.3 margins
    par(mar=c(5,4,1,1))
    #plot 4.3
    with(data,{
        plot(Sub_metering_1~ DateTime,
             type="n",
             ylab="Energy sub metering",
             xlab="")
        lines(Sub_metering_1~ DateTime)
        lines(Sub_metering_2~ DateTime,col="red")
        lines(Sub_metering_3~ DateTime, col="blue")
        legend("topright", 
               col=c("black","blue","red"),
               bty ="n",
               lty=c(1,1,1),
               cex=0.6, #legend text size
               xjust=0, 
               yjust=0, 
               y.intersp=1, #vertical spacing
               text.width=60000, #text width
               legend=c("Sub_metering_1", 
                        "Sub_metering_2",
                        "Sub_metering_3"))
    })  
    
    #set plot 4.4 margins
    par(mar=c(5,4,1,1))
    #plot 4.4
    with(data,{
        plot(Global_reactive_power~ DateTime,
             type="n",
             ylab="Global_reactive_power",
             xlab="datetime")
        lines(Global_reactive_power~ DateTime)
        
    })
    
    dev.copy(png, file = "plot4.png") ## Copy plot4 to a PNG file
    dev.off() #turn off png device
}