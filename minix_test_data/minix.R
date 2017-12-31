library(xlsx)
library(ggplot2)
library(ggpubr)
library(survMisc)
library(survival)
library(survminer)
library(data.table)
library(grid)
library(gridExtra)
library(XML)
library(RCurl)
library(taRifx)
library(stringr)
library("scales")
library ("MASS")
source("misc.R")
source("weibull.R")

# Root links of all closed issue pages (will use seperate loops)
closedURLs <- "https://github.com/Stichting-MINIX-Research-Foundation/minix/issues?page=1&q=is%3Aissue+is%3Aclosed"
openURLs <- "https://github.com/Stichting-MINIX-Research-Foundation/minix/issues?q=is%3Aopen+is%3Aissue"
startTime <- Sys.time()
xData.closed <- getURL(closedURLs)
xData.open <- getURL(openURLs)
doc.closed <- htmlParse(xData.closed)
doc.open <- htmlParse(xData.open)
closedURLs <- append(closedURLs, paste("https://github.com", xpathSApply(doc.closed, "//div[@class = 'pagination']/a/@href"), sep = ""))
openURLs <- append(openURLs, paste("https://github.com", xpathSApply(doc.open, "//div[@class = 'pagination']/a/@href"), sep = ""))
# The last one is duplicate
closedURLs <- closedURLs[-length(closedURLs)]
openURLs <- openURLs[-length(openURLs)]

submitDates <- NULL
closeDates <- NULL
issueNumber <- NULL
issueLabels <- NULL
for(i in closedURLs){
  # Parse the page url
  xData <- getURL(i)
  doc <- htmlParse(xData)
  # get links to each issue ticket page for submit dates
  theLinks <- paste("https://github.com", xpathSApply(doc, "//div[@class = 'border-right border-bottom border-left']//div[@class = 'float-left col-9 p-2 lh-condensed']/a[contains(@class, 'link-gray-dark')]/@href"), sep = "")
  closeDates <- append(closeDates, xpathSApply(doc, "//div[@class = 'repository-content']//span[@class = 'opened-by']//relative-time/@datetime"))
  
  for (j in theLinks){
  
    temp <- NULL
    pageData <- getURL(j)
    doc <- htmlParse(pageData)
    submitDates <- append(submitDates, xpathSApply(doc, "//div[@class = 'TableObject-item TableObject-item--primary']//relative-time/@datetime"))
    issueNumber <- append(issueNumber, xpathSApply(doc, "//div[@class = 'repository-content']//span[@class = 'gh-header-number']/text()"))
    
    temp <- xpathSApply(doc, "//div[@class = 'repository-content']//div[@class = 'discussion-sidebar']//div[@class = 'labels css-truncate']/a/text()")
    if(length(temp) == 0){
      
      issueLabels <- c(issueLabels, NA)
    }
    else {
      
      temp <- list(sapply(temp, xmlValue))
      issueLabels <- c(issueLabels, temp)
    }
  }
}
# Convert xml value from the text() function to character, and get rid of '#'.
issueNumber <- gsub("#", "", sapply(issueNumber, xmlValue))

for(i in openURLs){
  # Parse the page url
  xData <- getURL(i)
  doc <- htmlParse(xData)
  # get links to each issue ticket page for submit dates
  submitDates <- append(submitDates, xpathSApply(doc, "//div[@class = 'd-table table-fixed width-full Box-row--drag-hide position-relative']//span[@class = 'opened-by']/relative-time/@datetime"))
  issueNumber <- append(issueNumber, xpathSApply(doc, "//ul[@class = 'js-navigation-container js-active-navigation-container']//li[contains(@id, 'issue')]/@id"))
}
# Time to DownLoad
TTDL <- Sys.time() - startTime

issueNumber <- gsub("issue_", "", issueNumber)

# Coerce NAs since needs to be same length for dataframe
length(closeDates) <- length(submitDates)
# This actually coerces NULLs (maybe because its a list?)
length(issueLabels) <- length(submitDates)

# convert to posix time standards (use %OS for fractional seconds)
submitDates <- strptime(submitDates, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
closeDates <- strptime(closeDates, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")

# Create the data frame
github_data <- data.frame(Issue_Num = issueNumber, Opened_Date = submitDates, Closed_Date = closeDates)
# Since the labels column will contain lists... 
github_data$Labels <- vector(mode = "list", length = length(issueLabels))
github_data$Labels <- issueLabels
github_data$Labels[github_data$Labels == "NULL"] <- NA

# ====== Keep only confirmed bugs (I confirmed them, and marked the data with 1, or 0)
markedData <- read.xlsx("github_data/marked.xlsx", 1, startRow = 1, colIndex = c(1, 2))
merged_data <- merge(github_data, markedData, by = 'Issue_Num')
#View(d)
confirmedBugs <- merged_data[!(merged_data$Marked %in% 0), ]
# Sort by date issue was opened, then calculate the cumulative failures
confirmedBugs <- confirmedBugs[order(confirmedBugs$Opened_Date), ]
confirmedBugs$Time_To_Fail <- difftime(confirmedBugs$Opened_Date, confirmedBugs$Opened_Date[1], units= "hours")

#Calculate MTBF (custom functions)
confirmedBugs$TBFs <- make.interFailures(confirmedBugs$Time_To_Fail)
confirmedBugs$MTBF <- make.MTBF(confirmedBugs$TBFs)
githubMTBFs <- ggplot(data=confirmedBugs, aes(Time_To_Fail, MTBF))+ggtitle("Cumulative MTBF (GitHub Data)")+ labs(x ="SYSTEM AGE (hrs)", y = "MTBF (hrs)") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))

# Reliability
gitData.Surv <- survfit(Surv(Time_To_Fail) ~ 1, data = confirmedBugs)
gitData.gg <- ggsurvplot(gitData.Surv, conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none", xlab = "SYSTEM AGE (hrs)", ylab = "RELIABILITY", font.y = c(23, "black"), font.x = c(23, "black"), font.xtickslab = c(14, "plain", "black"), font.ytickslab = c(14, "plain", "black"))
gitData.km <- gitData.gg$plot

# Put plots together for presentation
R_and_MTBFs <- ggarrange(githubMTBFs, gitData.km, ncol = 1, nrow = 2)

# =================== View of failure frequencies for test data ================================

github_freq <- ggplot(confirmedBugs, aes(Opened_Date, fill=..count..)) +geom_histogram(color='black', alpha=0.9) + scale_x_datetime(breaks = date_breaks("1 months"),
                                                                                                        labels = date_format("%Y-%b") 
                                                                                                        ) + ggtitle("GitHub DATA (NEXT RELEASE EFFECT?)")+
  labs(y="FAILURE DENSITY\n(# Failures Observed)",x="MONTHS") +
  theme(axis.title = element_text(size=22), axis.text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(size = 24))


#=============== Using Accelerated Test Data ============================================================================

# Getting the data
dat_3.2 <- read.xlsx("test_3.2/test_3.2.xlsx", 1, startRow = 1, colIndex = c(1, 2, 3, 4, 5, 6))
dat_3.3 <- read.xlsx("test_3.3/test_3.3.xlsx", 1, startRow = 1, colIndex = c(1, 2, 3, 4, 5, 6))
dat_3.4 <- read.xlsx("test_3.4/test_3.4.xlsx", 1, startRow = 1, colIndex = c(1, 2, 3, 4, 5, 6))

#Taking unique failures only
dat_3.2 <- dat_3.2[row.names(unique(dat_3.2[,c('Version', 'Name')])),]
dat_3.3 <- dat_3.3[row.names(unique(dat_3.3[,c('Version', 'Name')])),]
dat_3.4 <- dat_3.4[row.names(unique(dat_3.4[,c('Version', 'Name')])),]

# # incase of misunderstanding...
# drops <- c("Yanran")
# dat_3.2 <- dat_3.2[!(dat_3.2$Student %in% drops), ]
# dat_3.3 <- dat_3.3[!(dat_3.3$Student %in% drops), ]
# dat_3.4 <- dat_3.4[!(dat_3.4$Student %in% drops), ]

# cumulative time-to-fail
dat_3.2$TTF <- unlist(by(dat_3.2, dat_3.2$Version, function(x) difftime(x$Time, x$Time[1], units= "secs")))
dat_3.3$TTF <- unlist(by(dat_3.3, dat_3.3$Version, function(x) difftime(x$Time, x$Time[1], units= "secs")))
dat_3.4$TTF <- unlist(by(dat_3.4, dat_3.4$Version, function(x) difftime(x$Time, x$Time[1], units= "secs")))

# create times between failures 
dat_3.2$TBFs <- make.interFailures(dat_3.2$TTF)
dat_3.3$TBFs <- make.interFailures(dat_3.3$TTF)
dat_3.4$TBFs <- make.interFailures(dat_3.4$TTF)

#Calculate MTBF (custom functions)
dat_3.2$MTBF <- make.MTBF(dat_3.2$TBFs)
dat_3.3$MTBF <- make.MTBF(dat_3.3$TBFs)
dat_3.4$MTBF <- make.MTBF(dat_3.4$TBFs)

# create Data for weibull plots (can't use the zero point with survreg or fitdistr)
d1 <- data.frame(TTF = dat_3.2$TTF[-1])
d2 <- data.frame(TTF = dat_3.3$TTF[-1])
d3 <- data.frame(TTF = dat_3.4$TTF[-1])

fit_d1 <- fitdistr(d1$TTF, "weibull")
fit_d2 <- fitdistr(d2$TTF, "weibull")
fit_d3 <- fitdistr(d3$TTF, "weibull")

# weibull CDF
wcdf1 <- ggplot(d1, aes(TTF)) + stat_ecdf(geom = "step", pad = FALSE)+stat_function(fun = pweibull, color = "blue", args = list(fit_d1$estimate[1], fit_d1$estimate[2]))
wcdf2 <- ggplot(d2, aes(TTF)) + stat_ecdf(geom = "step", pad = FALSE)+stat_function(fun = pweibull, color = "red", args = list(fit_d2$estimate[1], fit_d2$estimate[2]))
wcdf3 <- ggplot(d3, aes(TTF)) + stat_ecdf(geom = "step", pad = FALSE)+stat_function(fun = pweibull, color = "green", args = list(fit_d3$estimate[1], fit_d3$estimate[2]))

# linearized weibull plots (use plot(wp))
wp1 <- get.weibull.analysis(d1, "MINIX TEST 3.2", line = 'lm')
wp2 <- get.weibull.analysis(d2, "MINIX TEST 3.3", line = 'lm')
wp3 <- get.weibull.analysis(d3, "MINIX TEST 3.4", line = 'lm')

# Merge the data set (this is for facet plots, and is replaced with a new merge later)
all_dat <- rbind(dat_3.2, dat_3.3, dat_3.4)
Large.Surv <- survfit(Surv(TTF) ~ Version, data = all_dat)
Large.gg <- ggsurvplot(Large.Surv, fun = "event", conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none", xlab = "SYSTEM AGE (s)", ylab = "CDF", font.y = c(23, "black"), font.x = c(23, "black"), font.xtickslab = c(14, "plain", "black"), font.ytickslab = c(14, "plain", "black"))
ind_test_cdf.km <- Large.gg$plot + facet_wrap(~Version)

Large.Surv <- survfit(Surv(TTF) ~ Version, data = all_dat)
Large.gg <- ggsurvplot(Large.Surv, conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none", xlab = "SYSTEM AGE (s)", ylab = "RELIABILITY", font.y = c(23, "black"), font.x = c(23, "black"), font.xtickslab = c(14, "plain", "black"), font.ytickslab = c(14, "plain", "black"))
ind_test_Reliability.km <- Large.gg$plot + facet_wrap(~Version)

# MTBF plots
dat_3.2_MTBFs <- ggplot(data=dat_3.2, aes(TTF, MTBF))+ggtitle("v3.2")+ labs(x ="", y = "CUMULATIVE MTBF (s)") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))
dat_3.3_MTBFs <- ggplot(data=dat_3.3, aes(TTF, MTBF))+ggtitle("v3.3")+ labs(x ="SYSTEM AGE (s)", y = "") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))
dat_3.4_MTBFs <- ggplot(data=dat_3.4, aes(TTF, MTBF))+ggtitle("v3.4")+ labs(x ="", y = "") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))
all_MTBFs <- ggarrange(dat_3.2_MTBFs, dat_3.3_MTBFs, dat_3.4_MTBFs, ncol = 3, nrow = 1)

cdf_and_MTBFs <- ggarrange(all_MTBFs, ind_test_cdf.km, ncol = 1, nrow = 2)
R_and_MTBFs <- ggarrange(all_MTBFs, ind_test_Reliability.km, ncol = 1, nrow = 2)

# # CDF plots for individual tests
# dat_3.2.Surv <- survfit(Surv(TTF) ~ 1, data = dat_3.2)
# dat_3.2.gg <- ggsurvplot(dat_3.2.Surv, fun = "event", conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none")
# dat_3.2.km <- dat_3.2.gg$plot
# 
# # RELIABILITY plot for individual tests
# R_3.2.Surv <- survfit(Surv(TTF) ~ 1, data = dat_3.2)
# R_3.2.gg <- ggsurvplot(R_3.2.Surv, conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none")
# R_3.2.km <- R_3.2.gg$plot

# Calculate running sums of whole test time
temp <- cumsum(c(dat_3.2$TTF[length(dat_3.2$TTF)], dat_3.3$TBFs))
temp_1 <- cumsum(c(temp[length(temp)], dat_3.4$TBFs))

dat_3.3$TTF <- temp[-1]
dat_3.4$TTF <- temp_1[-1]
# Get Rid of censor point I created as start time (= 0).
dat_3.3 <- dat_3.3[-1, ]
dat_3.4 <- dat_3.4[-1, ]
# Merge the data set
all_dat <- rbind(dat_3.2, dat_3.3, dat_3.4)

# Make TBFs for whole data set
all_dat$TBFs <- make.interFailures(all_dat$TTF)
# make MTBFs
all_dat$MTBF <- make.MTBF(all_dat$TBFs)
# plot
all_dat_MTBFs <- ggplot(data=all_dat, aes(TTF, MTBF))+ggtitle("v3.2 - 3.4 MTBF (Test)")+ labs(x ="SYSTEM AGE (s)", y = "MTBF (s)") + theme(axis.title = element_text(size=22), axis.text=element_text(size=16)) + geom_point()+geom_line()+ theme(plot.title = element_text(size = 24))

# # CDF plot for all test data
# all_dat.Surv <- survfit(Surv(TTF) ~ 1, data = all_dat)
# all_dat.gg <- ggsurvplot(all_dat.Surv, fun = "event", conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none")
# all_dat.km <- all_dat.gg$plot
# RELIABILITY plot for all data
R_all_dat.Surv <- survfit(Surv(TTF) ~ 1, data = all_dat)
R_all_dat.gg <- ggsurvplot(R_all_dat.Surv, conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none", xlab = "SYSTEM AGE (s)", ylab = "RELIABILITY", font.y = c(23, "black"), font.x = c(23, "black"), font.xtickslab = c(14, "plain", "black"), font.ytickslab = c(14, "plain", "black"))
R_all_dat.km <- R_all_dat.gg$plot

all_dat_MTBFs_and_Reliability <- ggarrange(all_dat_MTBFs, R_all_dat.km, ncol = 1, nrow = 2)


# =================== View of failure frequencies for test data ================================
# set up boundaries for intervals/bins
breaks <- seq(0, 21600, by=600)
# specify interval/bin labels
labels <- c('1 (v3.2)', seq(from = 2, to = 12), '13 (v3.3)', seq(from = 14, to = 24), '25 (v3.4)', seq(from = 26, to = 36))
# bucketing data points into bins
bins <- cut(all_dat$TTF, breaks, include.lowest = T, right=FALSE, labels=labels)
# inspect bins
#summary(bins)
all_dat$bins <- bins
# Plot frequency to see if we have simulated the next release effect
freqPlot <- ggplot(data=all_dat, aes(x=all_dat$bins,fill=..count..)) + 
  geom_bar(color='black', alpha=0.9) + ggtitle("NEXT RELEASE EFFECT OBSERVED DURING TEST")+
  labs(y="FAILURE DENSITY\n(# Unique Failures Observed)",x="HYPOTHETICAL MONTHLY")+
  scale_x_discrete(drop=FALSE)+ theme(axis.title = element_text(size=22), axis.text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.title = element_text(size = 24))



# #===========================================================================================
# # If want to view by Student results
# 
# # using ('Version', 'Name') to make unique() return a data frame
# AllData <- AllData[row.names(unique(AllData[,c('Student', 'Version', 'Name')])),]
# 
# #   Sort by Student, then Time
# AllData <- AllData[order(AllData$Student, AllData$Time), ]
# 
# # cumulative time-to-fail by student
# AllData$Time_To_Fail <- unlist(by(AllData, AllData$Student, function(x) difftime(x$Time, x$Time[1], units= "secs")))
# 
# #Facet plots
# Full.Surv <- survfit(Surv(Time_To_Fail) ~ Student, data = AllData)
# Full.gg <- ggsurvplot(Full.Surv, fun = "event", conf.int = TRUE, color = "strata", ggtheme = theme_bw(), legend = "none")
# Full.km <- Full.gg$plot + facet_wrap(~Student)
# #===========================================================================================
