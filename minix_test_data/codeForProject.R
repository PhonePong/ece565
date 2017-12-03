

library(XML)
library(RCurl)
library(ggplot2)
library(gridExtra)
library(taRifx)
library(stringr)
library ("MASS")

# all spec CINT2006 results (for CFP2006 use "https://www.spec.org/cpu2006/results/cfp2006.html")
theURL <- "https://www.spec.org/cpu2006/results/cint2006.html"
#urlData <- getURL(theURL)
#data <- readHTMLTable(urlData, stringsAsFactors = FALSE)

startTime <- Sys.time()
xData <- getURL(theURL)
doc <- htmlParse(xData)
theLinks <- xpathSApply(doc, "//div[@class = 'idx_table']//span[@class = 'disclosures']/a[contains(@href, '.html')]/@href")
Links <- as.vector(theLinks)
head(theLinks)

#=================================== *Creating DataFrame of Characters From All Hardware Tables* =====================================================
firstURL <- "https://www.spec.org/cpu2006/results/res2011q3/cpu2006-20110620-17230.html"
firstData <- getURL(firstURL)
hardwareTable <- readHTMLTable(firstData, which = 2)

hardwareTable$V2 <- NULL

beginURL <- "https://www.spec.org/cpu2006/results/"

for ( i in theLinks){
  
  tryCatch({
    if(i %in% "res2011q3/cpu2006-20110620-17230.html") stop(e)
    print(i)   
  },error = function(e) print(paste(i,'is first iteration')))
  
  loopURL <- c(beginURL, i)
  
  wholeURL <- paste(loopURL, collapse = "")
  
  pageData <- getURL(wholeURL)
  
  hardwareTableLoop <- readHTMLTable(pageData, which = 2)
  
  hardwareTable <- cbind(hardwareTable, hardwareTableLoop$V2)
  
}

TTDL <- Sys.time() - startTime
# printing DL date and time
print(paste0("Download date and time: ", Sys.time()))

print(paste0("Time it took to download data: ", TTDL))


hardwareTable <- data.frame(lapply(hardwareTable, as.character), stringsAsFactors = FALSE, check.names = FALSE)
cnames <- hardwareTable$V1
hardwareTable$V1 <- NULL
cnames <- gsub("\\s|:", "_", cnames)
# this takes care of conversion to matrix by t()
newHardwareTable <- as.data.frame(t(hardwareTable), stringsAsFactors = FALSE)
colnames(newHardwareTable) <- cnames

#length(unique(ht[,1]))

################## end Hardware table creation ###

bigTable <- as.data.frame(readHTMLTable(doc, stringsAsFactors = FALSE ))
colnames(bigTable) <- c("test_sponsor", "system_name", "auto_parallel", 
                        "enabled_cores", "enabled_chips", "coresPer_chip", 
                        "threadsPer_core", "Base", "Peak")

#gets rid of some unwanted rows
bigTable <- bigTable[!(bigTable$test_sponsor=='Test Sponsor' | bigTable$test_sponsor=='EnabledCores'), ]

#need to get rid of unwanted columns, and refactor to numeric
uncleanTable <- cbind(bigTable, newHardwareTable)

# I will drop unwanted columns. Namely: 
drops <- c("test_sponsor", "system_name", "auto_parallel", "CPU_Name_", "CPU_Characteristics_", 
              "FPU_", "CPU(s)_enabled_", "CPU(s)_orderable_", "Primary_Cache_", "Secondary_Cache_", 
                "L3_Cache_", "Other_Cache_", "Disk_Subsystem_", "Other_Hardware_")
uncleanTable <- uncleanTable[ , !(names(uncleanTable) %in% drops)]

# rearranging columns
uncleanTable <- uncleanTable[ , c("enabled_cores", "enabled_chips", "coresPer_chip", "threadsPer_core", 
                    "CPU_MHz_","Memory_", "Base", "Peak")]

# cleaning the Memory_ column, which will be in GB
# unique(str_extract(henry$Memory_, "GB|TB"))

TBindex <- grep("TB", uncleanTable$Memory_)
memoryValues <- as.numeric(str_extract(uncleanTable$Memory_, "[0-9]+"))

for(i in TBindex){
  # converting the TB values to GB values
    memoryValues[i] = memoryValues[i] * 1024
}

uncleanTable$Memory_ <- memoryValues

# convert all clumns to numeric value
tidyData <- japply(uncleanTable, which(sapply(uncleanTable, class) == "character"), as.numeric)


# keep only complete data (subtracts 150 empty rows from data downloaded on April 20th)
tidyData <- tidyData[complete.cases(tidyData), ]

# DRAW INITIAL SCATTER PLOTS

p1 <- ggplot(tidyData, aes(x = enabled_cores, y = Base))
p1 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Base Scores vs. Enabled Cores") + labs(x = "Enabled Cores", y = "Base Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))
p2 <- ggplot(tidyData, aes(x = enabled_cores, y = Peak))
p2 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Peak Scores vs. Enabled Cores") + labs(x = "Enabled Cores", y = "Peak Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))

p3 <- ggplot(tidyData, aes(x = coresPer_chip, y = Base))
p3 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Base Scores vs. Cores Per Chip") + labs(x = "Cores/ Chip", y = "Base Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))
p4 <- ggplot(tidyData, aes(x = coresPer_chip, y = Peak))
p4 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Peak Scores vs. Cores Per Chip") + labs(x = "Cores/ Chip", y = "Peak Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))

p5 <- ggplot(tidyData, aes(x = CPU_MHz_, y = Base))
p5 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Base Scores vs. Clock Rate (MHz)") + labs(x = "Clock Rate", y = "Base Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))
p6 <- ggplot(tidyData, aes(x = CPU_MHz_, y = Peak))
p6 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Peak Scores vs. Clock Rate (MHz)") + labs(x = "Clock Rate", y = "Peak Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))

p7 <- ggplot(tidyData, aes(x = Memory_, y = Base))
p7 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Base Scores vs. Memory (GB)") + labs(x = "Memory", y = "Base Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))
p8 <- ggplot(tidyData, aes(x = Memory_, y = Peak))
p8 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  ggtitle("Peak Scores vs. Memory (GB)") + labs(x = "Memory", y = "Peak Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))

# show that base is good linear fit to peak, so only consider peak from here on
p9 <- ggplot(tidyData, aes(x = Base, y = Peak))
p9 + geom_point(color = "black", size = 1) + geom_smooth(method = "lm", se = TRUE, color = "yellow") + 
  ggtitle("Peak Scores vs. Base Scores") + labs(x = "Base Scores", y = "Peak Scores") + 
  theme(plot.title = element_text(size = 32, face = "bold")) + theme(axis.title = element_text(size = 22, face = "bold"))

# DRAWING BOX PLOTS 

#creating factors of enabled_chips, and threadsPer_core
eChips <- cut(tidyData$enabled_chips, breaks = c(0, 1, 2, 4, 8, 16), labels = c("(1)", "(2)", "(4)", "(8)", "(16)"))
tPerCore <- cut(tidyData$threadsPer_core, breaks = c(0, 1, 2), c("(1)", "(2)"))

# replacing old data with new factors
tidyData$enabled_chips <- eChips
tidyData$threadsPer_core <- tPerCore


bp1 <- plot(tidyData$threadsPer_core, tidyData$Base, main = "Base Scores vs. Threads Per Core", 
     xlab = "Threads/ Core", ylab = "Base Scores")
bp1
bp2 <- plot(tidyData$threadsPer_core, tidyData$Peak, main = "Peak Scores vs. Threads Per Core", 
     xlab = "Threads/ Core", ylab = "Peak Scores")
bp2

bp3 <- plot(tidyData$enabled_chips, tidyData$Base, main = "Base Score vs. Enabled Chips", 
     xlab = "Enabled Chips", ylab = "Base Scores")
bp3
bp4 <- plot(tidyData$enabled_chips, tidyData$Peak, main = "Peak Scores vs. Enabled Chips", 
     xlab = "Enabled Chips", ylab = "Peak Scores")
bp4


# LINEAR REGRESSION MODELING

# choosing reference factors for modeling (doesnt seem to be impact). Thought I might be able to filter linear model
eChips <- relevel(eChips, "(4)")
tPerCore <- relevel(tPerCore, "(2)")

tidyData$enabled_chips <- eChips
tidyData$threadsPer_core <- tPerCore
# setting seed for reproducible results 
set.seed(123)

splitTidy <- sample(nrow(tidyData), size = floor(0.85 * nrow(tidyData)))
# data to train the model (85% from original)
trainTidy <- tidyData[splitTidy,]
# data to test the model later (the other 15% from original)
testTidy <- tidyData[-splitTidy,]

# consider all factors + all interactions (not using this model)
# predictionModel <- lm(Peak ~ enabled_cores * enabled_chips * coresPer_chip * threadsPer_core * CPU_MHz_ * Memory_, data = trainTidy)
#predictionModel <- lm(Peak ~ CPU_MHz_*(enabled_chips + coresPer_chip + Memory_) + threadsPer_core + enabled_cores, data = trainTidy)
#summary(predictionModel)

# rSQ = .7597, (first attempt) take screen shot compare why I tried new model
# predictionModel <- lm(Peak ~ Memory_ * enabled_chips * enabled_cores * CPU_MHz_ + 
#                        (coresPer_chip + threadsPer_core), data = trainTidy)

# rSQ = .7422, cannot remove categorical factors as might effect reference (playing with data?) explain not enough values for 4 or 
predictionModel <- lm(Peak ~ Memory_ * enabled_chips * enabled_cores + 
                         (coresPer_chip + CPU_MHz_ + threadsPer_core ), data = trainTidy)

resSample <- sample(predictionModel$residuals, 100, replace = TRUE)
shapiro.test(resSample)
hgram1 <- hist(resSample, main = "Histogram: 100 Residual Samples from Linear Model", xlab = "Residual Samples")
hgram1
# by testing more data values, we can demostrate Central Limit Theorum (?)

resSample2 <- sample(predictionModel$residuals, 2000, replace = TRUE)
shapiro.test(resSample2)
hgram2 <- hist(resSample2, main = "Histogram: 2000 Residual Samples from Linear Model", xlab = "Residual Samples")
hgram2

# so we use other plots to visualize relationship
# this allows me to view 4 plots at once ( par(mfrow = c(1, 1)) goes back to 1 window)
par(mfrow = c(2,2))
# need to explain these
plot(predictionModel)

# ---------------datascienceplus.com/linear-regression-predict-energy-output-power-plant/
# easiest way to check accuracy of a model is R^2 value. Multiple R^2 = 1 - SSE/SST, where SSE is sum of square of residuals,
# residuals is the difference between predicted value, and actual value, SST is total sum of squares, and is calculated,
# by summing the squares of difference between the actual value and the mean value

# adjusted r value is similar to the multiple, but accounts for number of variables. multiple R^2 always increases when a new variable is 
#added to model, but if it is not significant, adjusted R^2 will decrease

# R^2 value of 1 means perfect prediction

# testing prediction against the test Data
prediction <- predict(predictionModel, newdata = testTidy)
head(prediction)

head(testTidy$Peak)

SSE <- sum((testTidy$Peak - prediction) ^ 2)
SST <- sum((testTidy$Peak - mean(testTidy$Peak)) ^ 2)

R.Squared.Prediction <- 1 - SSE/SST
R.Squared.Prediction

#try confidence levels !!!!! predict(confidence = ???)
 # dont need to find table by XPAth!
#this gets entire table in html format
# hardTB <- xpathSApply(hardware, "//table[contains(@id, 'Hardware')]")
#  but i want just the data within

# getting "th/a" for col names
#  hardCOLs <- xpathSApply(hardware, "//table[contains(@id, 'Hardware')]//th/a/text()") 
# dont want hardware node though (being more specific with tbody)
#hardCOLs <- xpathSApply(hardware, "//table[contains(@id, 'Hardware')]//tbody//th/a/text()")

# this checks which protocol curl version supports (found out here umassd guest network not supporting https)
#curlVersion()$protocol
