## plot1.R
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

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

# Calculate total emissions for each year
totalEmissions <- aggregate(Emissions ~ year, nei, sum)

# Calculate starting and ending values to easily display change over time
denom <- 1000000
startVal <- totalEmissions$Emissions[1]/denom
endVal <- totalEmissions$Emissions[4]/denom
midPoint <- (totalEmissions$year[4]+totalEmissions$year[1])/2

##########

# Set up output device and plot
png("plots/plot1.png",600,600)

  plot(
    totalEmissions$year, totalEmissions$Emissions/denom, type='l',
    main="Total PM2.5 emissions from US sources by year",
    xlab="Year",
    ylim=c(0,8), ylab="PM2.5 emissions (megatons)", 
  )
  points(totalEmissions$year, totalEmissions$Emissions/denom,pch=19)
  # Add arrow indicating total movement from start to end
  arrows(midPoint,startVal,y1=endVal,lwd=3, col='gray85', lty=6)
  # Add lines indicating the starting and ending value
  abline(h=c(startVal, endVal), lty=2, col=c('red','green'))

dev.off()
