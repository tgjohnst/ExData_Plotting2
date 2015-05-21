## plot6.R
## Compares emissions from motor vehicle sources in Baltimore City with 
##  emissions from motor vehicle sources in Los Angeles County, California

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

# Take only the subset of data related to motor vehicles
vehicles <- grepl("vehicle", scc$SCC.Level.Two, ignore.case=TRUE)
scc_vehicles <- scc[vehicles,]$SCC
nei_vehicles <- nei[(nei$SCC %in% scc_vehicles),]

# Calculate total per year
nei_vehiclesTotal <- aggregate(Emissions ~ fips+year, nei_vehicles,sum)

##########

# Created function to plot any given city/fips to avoid repeating code later
plotCity <- function(city,fips,color="black", denom=1000) {
  # Subset to just this city
  nei_city <- nei_vehiclesTotal[nei_vehiclesTotal$fips==fips,]
  # Calculate starting and ending values to easily display change over time
  startVal <- nei_city$Emissions[1]/denom
  endVal <- nei_city$Emissions[4]/denom
  midPoint <- (nei_city$year[4]+nei_city$year[1])/2
  # Plot the data!
  lines(nei_city$year, nei_city$Emissions/denom, col=color)
  points(nei_city$year, nei_city$Emissions/denom,pch=19, col=color)
  # Add arrow indicating total movement from start to end
  arrows(midPoint,startVal,y1=endVal,lwd=3, col='gray85', lty=6,length=0.1)
  # Add lines indicating the starting and ending value
  abline(h=c(startVal, endVal), lty=2, col=c('red','green'))
}

##########

# Set up output device and plot 2 panels
png("plots/plot6.png",600,600)
  plot(
    c(1999,2002,2005,2008),rep(1,4), type='n',
    main=paste0("Total PM2.5 emissions from vehicles in multiple cities, by year"),
    xlab="Year",
    ylab="PM2.5 Emissions (kilotons)", ylim=c(0,7.2)
  )
  # Plot city data
  plotCity('LA','06037',"chocolate1")
  plotCity('Baltimore','24510',"slateblue1" )
  # Draw legend
  legend('center', legend=c("LA","Baltimore"), fill=c("chocolate1","slateblue1"))

dev.off()