########
#Plot 1
########

setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Plot 1: make a plot showing the total PM2.5 emission 
#from all sources for each of the years 1999, 2002, 2005, and 2008.

MeanYearlyEmiss <- aggregate(NEI[c("Emissions")], list(year = NEI$year), sum)


barplot(height = MeanYearlyEmiss$Emissions, names.arg = MeanYearlyEmiss$year,  
        main = expression('Total PM'[2.5]*' Emissions by Year'),
        xlab = "Year", ylab = "Emissions")

dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

########
#Plot 2
########

setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Plot 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008?

# Subset NEI data by Baltimore's fip.
baltimoreNEI <- NEI[NEI$fips=="24510",]

MeanYearlyEmissBalt <- aggregate(baltimoreNEI[c("Emissions")], list(year = baltimoreNEI$year), sum)

barplot(height = MeanYearlyEmissBalt$Emissions, names.arg = MeanYearlyEmissBalt$year,  
        main = expression('Total PM'[2.5]*' Emissions by Year in Baltimore City'),
        xlab = "Year", ylab = "Emissions")

dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()


########
#Plot 3
########

setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)

#Plot 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 

# Subset NEI data by Baltimore's fip.
baltimoreNEI <- NEI[NEI$fips=="24510",]

MeanYearlyEmissBalt <- aggregate(baltimoreNEI[c("Emissions")], list(year = baltimoreNEI$year), sum)

plot3 <- ggplot(baltimoreNEI, aes(factor(year), Emissions, fill=type)) +
  geom_bar(stat = "identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type, scales = "free", space="free") +
  labs(x="Year", y=expression("PM"[2.5]*" Emissions")) +
  labs(title=expression("PM"[2.5]*" Emissions in Baltimore City (1999-2008) by Source Type"))

print(plot3)

dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()


########
#Plot 4
########

#Plot 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008? 

setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge the two data files

NEISCC <- merge(NEI, SCC, by="SCC")

#subset the data for all records where coal is involved

coal <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
coalNEISCC <- NEISCC[coal, ]
MeanYearlyCoalEmiss <- aggregate(Emissions ~ year, coalNEISCC, sum)

library(ggplot2)

plot4 <- ggplot(MeanYearlyCoalEmiss, aes(factor(year), Emissions)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  labs(x="Year", y=expression("PM"[2.5]*" Emissions")) +
  labs(title=expression("PM"[2.5]*" Emissions from Coal Sources by Year"))
print(plot4)

dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()



########
#Plot 5
########

#Plot 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 

setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge the two data files

NEISCC <- merge(NEI, SCC, by="SCC")

#subset the data for all records where motor vehicles are involved

MV <- grepl("Highway Veh", NEISCC$Short.Name, ignore.case=TRUE)
MVNEISCC <- NEISCC[MV, ]

#subset the data for all records for Baltimore City only

MVNEISCCBC <- MVNEISCC[MVNEISCC$fips=="24510",]
MeanYearlyMVEmissBC <- aggregate(Emissions ~ year, MVNEISCCBC, sum)


library(ggplot2)

plot5 <- ggplot(MeanYearlyMVEmissBC, aes(factor(year), Emissions)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  labs(x="Year", y=expression("PM"[2.5]*" Emissions")) +
  labs(title=expression("PM"[2.5]*" Emissions from Motor Vehicle Sources in Baltimore City by Year"))
print(plot5)

dev.copy(png, file="plot5.png", height=480, width=480)
dev.off()


########
#Plot 6
########

#Plot 6: Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in 
#Los Angeles County, California (fips == 06037). 
#Which city has seen greater changes over time in motor vehicle emissions?


setwd("C:/Users/Karyn/Dropbox/Data Science/R stuff/Exploratory Data Analysis/EDA Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge the two data files

NEISCC <- merge(NEI, SCC, by="SCC")

#subset the data for all records where motor vehicles are involved

MV <- grepl("Highway Veh", NEISCC$Short.Name, ignore.case=TRUE)
MVNEISCC <- NEISCC[MV, ]

#subset the data for all records for Baltimore City only

MVNEISCCBC <- MVNEISCC[MVNEISCC$fips=="24510",]
MVNEISCCBC$City <- "Baltimore City"

#subset the data for all records for LA only

MVNEISCCLA <- MVNEISCC[MVNEISCC$fips=="06037",]
MVNEISCCLA$City <- "Los Angeles"

#combine the two subset dataframes

MVNEISCCBCandLA <- rbind(MVNEISCCBC, MVNEISCCLA)


library(ggplot2)

plot6 <- ggplot(MVNEISCCBCandLA, aes(x=factor(year), y=Emissions, fill=City)) +
  geom_bar(aes(fill=year), stat="identity") +
  facet_grid(scales="free", space="free", .~City) +
  theme_bw() + guides(fill=FALSE)+
  labs(x="Year", y=expression("PM"[2.5]*" Emissions")) +
  labs(title=expression("PM"[2.5]*" Emissions from Motor Vehicle Sources in Baltimore City and Los Angeles by Year"))
print(plot6)

dev.copy(png, file="plot6.png", height=480, width=620)
dev.off()