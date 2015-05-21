## plot5.R
## How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

## Analysis script for the second project of Coursera Data Science Series, 
## Course 4: Exploratory Data Analysis (exdata-014)
## Timothy Johnstone, May 2015

# Set working directory
# TO RUN THIS SCRIPT please modify the path to reflect your system's file structure
setwd("Z:/Lotus/Dropbox/Coursera/4_Exploratory_Data_Analysis/Projects/ExData_Plotting2")

##########

# Load in dataset files
nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

##########

# Take only the subset of data related to motor vehicles in Baltimore
vehicles <- grepl("vehicle", scc$SCC.Level.Two, ignore.case=TRUE)
scc_vehicles <- scc[vehicles,]$SCC
nei_vehicles <- nei[(nei$SCC %in% scc_vehicles) & (nei$fips == 24510),]

# Calculate total per year
nei_vehiclesTotal <- aggregate(Emissions ~ year, nei_vehicles,sum)

# Calculate starting and ending values to easily display change over time
denom <- 1000
startVal <- nei_vehiclesTotal$Emissions[1]/denom
endVal <- nei_vehiclesTotal$Emissions[4]/denom
midPoint <- (nei_vehiclesTotal$year[4]+nei_vehiclesTotal$year[1])/2

##########

# Set up output device and plot
png("plots/plot5.png",600,600)

  plot(
    nei_vehiclesTotal$year, nei_vehiclesTotal$Emissions/denom, type='l',
    main="Total PM2.5 emissions from Baltimore MD motor vehicles, by year",
    xlab="Year",
    ylim=c(0,0.6), ylab="PM2.5 Emissions (kilotons)", 
  )
  points(nei_vehiclesTotal$year, nei_vehiclesTotal$Emissions/denom,pch=19)
  # Add arrow indicating total movement from start to end
  arrows(midPoint,startVal,y1=endVal,lwd=3, col='gray85', lty=6)
  # Add lines indicating the starting and ending value
  abline(h=c(startVal,endVal), lty=2, col=c('red','green'))

dev.off()