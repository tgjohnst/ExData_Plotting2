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


##########

# Set up output device and plot
png("plots/plot5.png",600,600)

  plot(
    nei_vehiclesTotal$year, nei_vehiclesTotal$Emissions/1000, type='l',
    main="Total PM2.5 emissions from Baltimore MD motor vehicles, by year",
    xlab="Year",
    ylim=c(0,0.6), ylab="PM2.5 Emissions (kilotons)", 
  )
  points(nei_vehiclesTotal$year, nei_vehiclesTotal$Emissions/1000,pch=19)
  
  # Add lines indicating the starting and ending value
  abline(h=nei_vehiclesTotal$Emissions[1]/1000, lty=2, col='red')
  abline(h=nei_vehiclesTotal$Emissions[4]/1000, lty=2, col='green')

dev.off()