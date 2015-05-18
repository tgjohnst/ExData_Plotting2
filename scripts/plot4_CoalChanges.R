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
# Find coal and combustion related scc codes
comb <- grepl("comb", scc$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", scc$SCC.Level.Four, ignore.case=TRUE) 
scc_coalCombustion <- scc[(comb & coal),]$SCC
# Subset nei by scc codes
nei_coalCombustion <- nei[nei$SCC %in% scc_coalCombustion,]

# Calculate total per year
nei_coalCombustion <- aggregate(Emissions ~ year, nei_coalCombustion,sum)


##########

# Set up output device and plot
png("plots/plot4.png",600,600)

  plot(
    nei_coalCombustion$year, nei_coalCombustion$Emissions/1000000, type='l',
    main="Total PM2.5 emissions from coal combustion sources, by year",
    xlab="Year",
    ylim=c(0,0.6), ylab="PM2.5 Emissions (megatons)", 
  )
  points(nei_coalCombustion$year, nei_coalCombustion$Emissions/1000000,pch=19)
  
  # Add lines indicating the starting and ending value
  abline(h=nei_coalCombustion$Emissions[1]/1000000, lty=2, col='red')
  abline(h=nei_coalCombustion$Emissions[4]/1000000, lty=2, col='green')

dev.off()
