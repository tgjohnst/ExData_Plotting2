## plot1.R
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

# Set up output device and plot
png("plots/plot1.png",600,600)

plot(
  totalEmissions$year, totalEmissions$Emissions/1000000, type='l',
  main="Total PM2.5 emissions from US sources by year",
  xlab="Year",
  ylim=c(0,8), ylab="PM2.5 Emissions (millions of tons)", 
)

points(totalEmissions$year, totalEmissions$Emissions/1000000,pch=19)

#add lines indicating the starting and ending value
abline(h=totalEmissions$Emissions[1]/1000000, lty=2, col='red')
abline(h=totalEmissions$Emissions[4]/1000000, lty=2, col='green')

dev.off()
