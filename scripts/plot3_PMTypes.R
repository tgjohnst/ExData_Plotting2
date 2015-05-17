## plot1.R
## Analysis script for the second project of Coursera Data Science Series, 
## Course 4: Exploratory Data Analysis (exdata-014)
## Timothy Johnstone, May 2015

# Set working directory
# TO RUN THIS SCRIPT please modify the path to reflect your system's file structure
setwd("Z:/Lotus/Dropbox/Coursera/4_Exploratory_Data_Analysis/Projects/ExData_Plotting2")

# You must have the ggplot2 library installed to run this script
library(ggplot2)
##########

# Load in dataset files
nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

##########

# Get just baltimore emissions
nei_Baltimore <- nei[nei$fips=="24510",]

# Calculate total emissions for each year & type in Baltimore
totalEmissions_Baltimore <- aggregate(Emissions ~ type+year, nei_Baltimore,sum)

# Calculate max and min per type for hlines
firstPerType <- data.frame(z=totalEmissions_Baltimore$Emissions[1:4],type=totalEmissions_Baltimore$type[1:4])
lastPerType <- data.frame(z=totalEmissions_Baltimore$Emissions[13:16],type=totalEmissions_Baltimore$type[13:16])

# Create output device and plot
png("plots/plot3.png",600,600)
ggp <- ggplot(totalEmissions_Baltimore, aes(year,Emissions,fill=type)) +
  geom_line() + geom_point() + 
  geom_hline(data=firstPerType, aes(yintercept = z), lty=2, col='green') +
  geom_hline(data=lastPerType, aes(yintercept = z),lty=2, col='red') +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(title=expression("PM"[2.5]*" dmissions in Baltimore MD (24510) for each source type by year")) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (tons)"))
print(ggp)
dev.off()
