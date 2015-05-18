## plot2.R
## Have total emissions from PM2.5 decreased in the Baltimore City, 
##  Maryland (fips == "24510") from 1999 to 2008?

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

# Get just baltimore emissions
nei_Baltimore <- nei[nei$fips=="24510",]

# Calculate total emissions for each year in Baltimore
totalEmissions_Baltimore <- aggregate(Emissions ~ year, nei_Baltimore,sum)

# Set up output device and plot
png("plots/plot2.png",600,600)

  plot(
    totalEmissions_Baltimore$year, totalEmissions_Baltimore$Emissions/1000, type='l',
    main="Total PM2.5 emissions in Baltimore MD (24510) by year",
    xlab="Year",
    ylim=c(0,4), ylab="PM2.5 Emissions (kilotons)", 
  )
  
  points(totalEmissions_Baltimore$year, totalEmissions_Baltimore$Emissions/1000,pch=19)
  
  # Add lines indicating the starting and ending value
  abline(h=totalEmissions_Baltimore$Emissions[1]/1000, lty=2, col='red')
  abline(h=totalEmissions_Baltimore$Emissions[4]/1000, lty=2, col='green')

dev.off()