# plot1.r
# make plot 1 from first course assignment
# 
# CS O'Connell, UMN EEB/IonE


#####################################################################
# plot1.r

## read in data

# csv info
filepath <- "~/Documents/GITHUB/CourseraMaterials/ExData_Plotting1/data/household_power_consumption.txt"

# csv is big - load in faster using colclasses
tab5rows <- read.table(filepath, header = TRUE, nrows = 5, sep=";")
classes <- sapply(tab5rows, class)

# how many rows to read in? (skip and nrows)
# 69518 - 66637 = 2881, so use 2880
dat <- read.table(filepath, skip=66637, nrows=2880, sep=";", na.strings = "?", colClasses = classes)
head(dat)
tail(dat)


## make plot1

# where to save
filesave <- "~/Documents/GITHUB/CourseraMaterials/ExData_Plotting1/plot1.png"

# make the fig
png(filename=filesave, width=480, height=480, bg="transparent")   # choose device.. 
par(mar=c(6,4,3,2)) 
hist(dat$V3, breaks=15, col= "red", ylim = c(0,1200), xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main= "Global Active Power")
dev.off() 


