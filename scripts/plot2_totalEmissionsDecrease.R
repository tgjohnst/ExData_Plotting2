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

# Calculate starting and ending values to easily display change over time
startVal <- totalEmissions_Baltimore$Emissions[1]/1000
endVal <- totalEmissions_Baltimore$Emissions[4]/1000
midPoint <- (totalEmissions_Baltimore$year[4]+totalEmissions_Baltimore$year[1])/2

##########

# Set up output device and plot
png("plots/plot2.png",600,600)

  plot(
    totalEmissions_Baltimore$year, totalEmissions_Baltimore$Emissions/1000, type='l',
    main="Total PM2.5 emissions in Baltimore MD (24510) by year",
    xlab="Year",
    ylim=c(0,4), ylab="PM2.5 Emissions (kilotons)", 
  )
  points(totalEmissions_Baltimore$year, totalEmissions_Baltimore$Emissions/1000,pch=19)
  # Add arrow indicating total movement from start to end
  arrows(midPoint,startVal,y1=endVal,lwd=3, col='gray85', lty=6)
  # Add lines indicating the starting and ending value
  abline(h=c(startVal,endVal), lty=2, col=c('red','green'))

dev.off()