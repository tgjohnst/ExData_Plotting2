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

# Take only the subset of data related to motor vehicles in Baltimore & LA
vehicles <- grepl("vehicle", scc$SCC.Level.Two, ignore.case=TRUE)
scc_vehicles <- scc[vehicles,]$SCC
nei_la_vehicles <- nei[(nei$SCC %in% scc_vehicles) & (nei$fips == '06037'),]
nei_balt_vehicles <- nei[(nei$SCC %in% scc_vehicles) & (nei$fips == '24510'),]

# Calculate total per year
nei_la_vehiclesTotal <- aggregate(Emissions ~ year, nei_la_vehicles,sum)
nei_balt_vehiclesTotal <- aggregate(Emissions ~ year, nei_balt_vehicles,sum)

# Take only the subset of data related to motor vehicles in Baltimore & LA
vehicles <- grepl("vehicle", scc$SCC.Level.Two, ignore.case=TRUE)
scc_vehicles <- scc[vehicles,]$SCC
nei_vehicles <- nei[(nei$SCC %in% scc_vehicles),]

# Calculate total per year
nei_vehiclesTotal <- aggregate(Emissions ~ fips+year, nei_vehicles,sum)

##########

# Function to plot any given city/fips to avoid repeating code later
plotCity <- function(city,fips) {
  nei_city <- nei_vehiclesTotal[nei_vehiclesTotal$fips==fips,]
  plot(
    nei_city$year, nei_city$Emissions/1000, type='l',
    main=paste0("Total PM2.5 emissions from vehicles in ", city, ", by year"), cex.main=0.6,
    xlab="Year",
    ylab="PM2.5 Emissions (kilotons)", ylim=c(0,8) 
  )
  points(nei_city$year, nei_city$Emissions/1000,pch=19)
  
  # Add lines indicating the starting and ending value
  abline(h=nei_city$Emissions[1]/1000, lty=2, col='red')
  abline(h=nei_city$Emissions[4]/1000, lty=2, col='green')
}

##########

# Set up output device and plot 2 panels
png("plots/plot6.png",600,600)
par(mfrow=c(1,2))
  plotCity('Baltimore','24510')
  plotCity('LA','06037')

dev.off()