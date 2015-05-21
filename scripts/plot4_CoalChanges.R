## plot4.R
## Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

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

# Take only the subset of data related to coal combustion
comb <- grepl("comb", scc$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", scc$SCC.Level.Four, ignore.case=TRUE) 
scc_coalCombustion <- scc[(comb & coal),]$SCC
# Subset nei by scc codes
nei_coalCombustion <- nei[nei$SCC %in% scc_coalCombustion,]

# Calculate total per year
nei_coalCombustion <- aggregate(Emissions ~ year, nei_coalCombustion,sum)

# Calculate starting and ending values to easily display change over time
denom <- 1000000
startVal <- nei_coalCombustion$Emissions[1]/denom
endVal <- nei_coalCombustion$Emissions[4]/denom
midPoint <- (nei_coalCombustion$year[4]+nei_coalCombustion$year[1])/2


##########

# Set up output device and plot
png("plots/plot4.png",600,600)

  plot(
    nei_coalCombustion$year, nei_coalCombustion$Emissions/denom, type='l',
    main="Total PM2.5 emissions from coal combustion sources, by year",
    xlab="Year",
    ylim=c(0,0.6), ylab="PM2.5 Emissions (megatons)", 
  )
  points(nei_coalCombustion$year, nei_coalCombustion$Emissions/denom,pch=19)
  # Add arrow indicating total movement from start to end
  arrows(midPoint,startVal,y1=endVal,lwd=3, col='gray85', lty=6)
  # Add lines indicating the starting and ending value
  abline(h=c(startVal, endVal), lty=2, col=c('red','green'))

dev.off()
