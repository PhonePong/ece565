# by: JR @ IDA

library(ggplot2)
library(scales)


# ================== Begin Weibull Scale Plotting functions ====================
# Create custom scale transformation for weibull plots to work
weib_yScale <- function(y){log(qweibull(y, 1, 1))}
weib_inv_yScale <- function(y){exp(-exp(y))}
weibull_trans <- function()trans_new("weibull", weib_yScale, weib_inv_yScale)

# This list was determined by trial and error
brks.n.lbls <- list(
  # Could just use condition index, so that the list only has first 2 items
  brk.0000001 = c(seq(.0000001, .0000009, .00000005), seq(.000001, .000009, .0000005), seq(.00001, .00009, .000005), seq(.0001, .0009, .00005), seq(.0001, .0009, .00005), seq(.001, .009, .0005), seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)), 
  lbl.0000001 = c(.00001, .00002, .00003, .00004, .00006, .00008, .0001, .0002, .0003, .0004, .0006, .0008, .001, .002, .003, .004, .006, .008, .01, .02, .03, .04, .06, .08, .1, .2, .3, .4, .6, .8, 1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.000001 = c(seq(.000001, .000009, .0000005), seq(.00001, .00009, .000005), seq(.0001, .0009, .00005), seq(.0001, .0009, .00005), seq(.001, .009, .0005), seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)), 
  lbl.000001 = c(.0001, .0002, .0003, .0004, .0006, .0008, .001, .002, .003, .004, .006, .008, .01, .02, .03, .04, .06, .08, .1, .2, .3, .4, .6, .8, 1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.00001 = c(seq(.00001, .00009, .000005), seq(.0001, .0009, .00005), seq(.0001, .0009, .00005), seq(.001, .009, .0005), seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)), 
  lbl.00001 = c(.001, .002, .003, .004, .006, .008, .01, .02, .03, .04, .06, .08, .1, .2, .3, .4, .6, .8, 1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.0001 = c(seq(.0001, .0009, .00005), seq(.001, .009, .0005), seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)), 
  lbl.0001 = c(.01, .02, .03, .04, .06, .08, .1, .2, .3, .4, .6, .8, 1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.001 = c(seq(.001, .009, .0005), seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)), 
  lbl.001 = c(.1, .2, .3, .4, .6, .8, 1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.01 = c(seq(.01, .09, .005), seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)),
  lbl.01 = c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9),
  
  brk.1 = c(seq(.1, .9, .025), seq(.95, .99, .01), c(.994, .996, .997, .998, .999)),
  lbl.1 = c(10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 95, 97, 99, 99.6, 99.9)
)

make.weibull.reg <- function(surv.item = NULL){
  return(survreg(surv.item ~ 1, dist = "weibull"))
}
get.weibull.reg <- function(surv.item = NULL){
  return(make.weibull.reg(surv.item))
}
make.weibull.shape <- function(fit = NULL){
  return(as.numeric(1/fit$scale))
}
get.weibull.shape <- function(fit = NULL){
  return(make.weibull.shape(fit))
}
make.weibull.scale <- function(fit = NULL){
  return(as.numeric(exp(fit$coefficients)))
}
get.weibull.scale <- function(fit = NULL){
  return(make.weibull.scale(fit))
}


# Component level failures recorded at same miles, are complete.
# Determines if data is right censored, then creates a Surv item for regression
# ASSUMPTION: Failures occured the exact mileage it was recorded.
#             Thus, multiple failures recorded that have the same failure mileage
#             were discovered sequentially, at an unknown time between the first occurence of that mileage,
#               to the next recorded failure mileage. ERGO, RIGHT CENSORED
make.right.censored <- function(failureData = NULL){
  
  if(is.unsorted(failureData)){
    failureData <- sort(failureData)
  }
  t <- data.frame(table(failureData))
  t <- setDF(t)
  count <- 1
  status <- numeric(0)
  
  for(i in t$Freq){
    if(i == 1){
      status[count] <- i
      count = count + 1
    }
    else{
      status <- c(status, c(1, replicate(i - 1, 0)))
      count = count + 1 + (i - 1)
    }
  }
  rm(list = c("t", "count"))
  return(Surv(failureData, status))
}

get.right.censored <- function(failureData = NULL){
  return(make.right.censored(failureData))
}

# Median Ranks is better for weibull plotting
#   source: Catalano, "Evaluation of median ranks and mean ranks Plotting for Reliability Estimation Using the Weibull Distribution"
medianranks <- function(data){
  if(is.unsorted(data)){
    data <- sort(data)
  }
  n = length(data)
  
  # throws error: # of items to replace not multple of replace
  # NEED data[i:i], since coming from dataframe (df[ , 1])
  for (i in seq_along(data)) {
    data[i:i] <- (i - 0.3)/(n + 0.4)
  }
  return(data)
}

# wrapper
get.medianranks <- function(frme = NULL){
  frme$median.ranks <- medianranks(frme[,1])
  return(frme)
}

remove.nulls <- function(ls = NULL){
  return(ls[!sapply(ls, is.null)])
}

# Regex counts # zeros after decimal, between next [1-9]
numZeros_AfterDecimal <- function(num){
  options(scipen = 10)
  n <- toString(attr(regexpr("(?<=\\.)0+", num, perl = TRUE), "match.length"))
  options(scipen = 0)
  return(n)
}

# Returns 10 for ten's place, 100 for hundredths...
getPlace <- function(x){
  if(is.data.frame(x) || is.matrix(x) || !is.numeric(x) || (length(x) > 1)){
    stop("Argument must be single numeric value", call. = FALSE)
  }
  if(floor(as.numeric(x)) > 0){
    return(1)
  }
  else{
    num <- numZeros_AfterDecimal(x)
    switch(num, 
           "-1" = {place <- '10'}, 
           "1" = {place <- '100'}, 
           "2" = {place <- '1000'}, 
           "3" = {place <- '10000'},
           "4" = {place <- '100000'},
           "5" = {place <- '1000000'},
           "6" = {place <- '10000000'},
           # Could check for larger values here
           stop("get.place: Weibull paper not in range", call. = FALSE))
    return(place)
  }
}

# Wrapper function to avoid indexing
get.First <- function(num = NULL){
  if(length(num) == 1){
    warning("get.First: argument is length 1", call. = FALSE)
    return(num[1])
  }
  else{
    return(num[1])
  }
}
# Wrapper function to avoid indexing
get.Last <- function(num = NULL){
  if(length(num) == 1){
    warning("get.Last: argument is length 1", call. = FALSE)
    return(num[length(num)])
  }
  else{
    return(num[length(num)])
  }
}
# Wrapper function to avoid indexing
get.First.and.Last <- function(vect = NULL){
  #return(c(head(vect)))
  if(length(vect) == 1){
    warning(length(vect), call. = FALSE)
    stop("get.First.and.Last: argument has length 1", call. = FALSE)
  }
  else{
    return(c(get.First(vect), get.Last(vect)))
  }
}

# Retrieve first and last of first 2 columns (X-Xn, Y-Yn)
get.xy.Range <- function(frme = NULL){
  if(!is.data.frame(frme)){
    stop("get.xyRange: argument requires data frame", call. = FALSE)
  }
  if(length(frme) == 1){
    stop("get.xyRange: argument has 1 column", call. = FALSE)
  }
  else{
    return(lapply(frme[1:2], get.First.and.Last))
  }
}

# Returns lowest break of x scale
get.xlow.limit <- function(xLow = NULL){
  if (xLow <= .000000001 & xLow >= .0000000001) return(.0000000001)
  if (xLow <= .00000001 & xLow >= .000000001) return(.000000001)
  if (xLow <= .0000001 & xLow >= .00000001) return(.00000001)
  if (xLow <= .000001 & xLow >= .0000001) return(.0000001)
  if (xLow <= .00001 & xLow >= .000001) return(.000001)
  if (xLow <= .0001 & xLow >= .00001) return(.00001)
  if (xLow <= .001 & xLow >= .0001) return(.0001)
  if (xLow <= .01 & xLow >= .001) return(.001)
  if (xLow <= .1 & xLow >= .01) return(.01)
  if (xLow <= 1 & xLow >= .1) return(.1)
  if (xLow <= 10 & xLow >= 1) return(1)
  if (xLow <= 100 & xLow >= 10) return(10)
  if (xLow <= 1000 & xLow >= 100) return(100)
  if (xLow <= 10000 & xLow >= 1000) return(1000)
  if (xLow <= 100000 & xLow >= 10000) return(10000)
  stop(c("get.xlow.limit: argument out of minimum range: ", xLow), call. = FALSE)
}

# Returns highest break of x scale
get.xhi.limit <- function(xHi = NULL){
  if (xHi > 1000000000000) stop(c("get.xhi.limit: argument out of maximum range", xHi), call. = FALSE)
  if (xHi >= 100000000000) return(1000000000000)
  if (xHi >= 10000000000) return(100000000000)
  if (xHi >= 1000000000) return(10000000000)
  if (xHi >= 100000000) return(1000000000)
  if (xHi >= 10000000) return(100000000)
  if (xHi >= 1000000) return(10000000)
  if (xHi >= 100000) return(1000000)
  if (xHi >= 10000) return(100000)
  if (xHi >= 1000) return(10000)
  if (xHi >= 100) return(1000)
  if (xHi >= 10) return(100)
  if (xHi >= 1) return(10)
  if (xHi >= .1) return(1)
}

# Returns lowest (minor)break of y scale
get.ylow.limit <- function(place = NULL){
  yLow <- vector('numeric')
  switch(place, 
         #'1' = {yLow <- 1}, 
         '10' = {yLow <- '0.1'}, 
         '100' = {yLow <- '0.01'}, 
         '1000' = {yLow <- '0.001'},
         '10000' = {yLow <- '0.0001'},
         '100000' = {yLow <- '0.00001'},
         '1000000' = {yLow <- '0.000001'},
         '10000000' = {yLow <- '0.0000001'},
         stop(c("get.ylow.limit: weibull paper not in range (val = ", as.character(place), ")"), call. = FALSE))
  return(yLow)
}

# Returns both lowest and highest breaks for x scale
get.xLimits <- function(xRange = NULL){
  #return(c(1, get.xhi.limit(xRange[2])))
  return(c(get.xlow.limit(xRange[1]), get.xhi.limit(xRange[2])))
}

# Returns both lowest and highest (always .999) (minor)breaks for y scale
# Could probably seperate this get
get.yLimits <- function(yRange = NULL){
  place <- getPlace(yRange[1])
  yLo <- get.ylow.limit(place)
  #return(list('0.000001', .999))
  return(list(yLo, .999))
}

# Creates the major x axis breaks 
get.factors.between <- function(bounds = NULL, by = NULL){
  # if this function is to be generalized, then must check for
  # by less than 1
  if(bounds[1] == 0){
    stop("get.factors.between: lower bound is zero", call. = FALSE)
  }
  last <- FALSE
  i <- 1
  facts <- bounds[1]
  while(!last){
    i = i + 1
    facts[i] <- facts[i - 1] * by
    last <- (facts[i] >= bounds[length(bounds)])
  }
  return(facts)
}

# Wrapper function (since we might want to generalize get.factors.between)
get.xMajorBreaks <- function(xLims = NULL){
  # perhaps create conditional if less than 1 (send 100?)
  return(get.factors.between(xLims, 10))
}

# Creates the minor break lines between the majors on the log scaled x axis
get.xMinorBreaks <- function(majors = NULL){
  # I've designed this algorithm to work for log scale.
  greater.One <- TRUE
  len <- length(majors)
  minors <- vector('numeric')
  i <- 1
  if(majors[1] <= 1){
    greater.One <- FALSE
    while(!greater.One){
      by <- majors[i] / 2
      minors <- c(minors, seq(majors[i], majors[i + 1], by))
      
      i <- i + 1
      greater.One <- (majors[i] > 1)
    }
  }
  if(greater.One){
    while( !(i ==  len) ){
      by = majors[i] / 2
      minors <- c(minors, seq(majors[i], majors[i + 1], by))
      i <- i + 1
    }
  }
  return(minors[!(minors %in% majors)])
}

# Wrapper function to return all x-axis breaks
get.xBreaks <- function(xLimits = NULL){
  majors <- get.xMajorBreaks(xLimits)
  minors <- get.xMinorBreaks(majors)
  return(list(majors, minors))
}

# Wrapper function to return both the (minor)breaks and labels used for them
get.yBreaks <- function(yLimits = NULL){
  # All breaks for weibull can only be minor because of ggPlot limitation 
  # Otherwise I would get majors here as well
  brkslbls <- vector('numeric')
  #stop(as.character(yLimits[[1]]), call. = FALSE)
  switch(as.character(yLimits[[1]]), 
         '0.0000001' = {brkslbls <- brks.n.lbls[c("brk.0000001", "lbl.0000001")]},
         '0.000001' = {brkslbls <- brks.n.lbls[c("brk.000001", "lbl.000001")]},
         '0.00001' = {brkslbls <- brks.n.lbls[c("brk.00001", "lbl.00001")]},
         '0.0001' = {brkslbls <- brks.n.lbls[c("brk.0001", "lbl.0001")]}, 
         '0.001' = {brkslbls <- brks.n.lbls[c("brk.001", "lbl.001")]}, 
         '0.01' = {brkslbls <- brks.n.lbls[c("brk.01", "lbl.01")]}, 
         '0.1' = {brkslbls <- brks.n.lbls[c("brk.1", "lbl.1")]},
         #'1' = {brkslbls <- 1},
         stop(c("get.yBreaks: Weibull paper not in range (val = ", as.character(yLimits[[1]]), ")"), call. = FALSE))
  return(brkslbls)
}

# does small data manipulation on the labels list
# to increase distance of y labels from plot, add space here
get.yLabels <- function(yBrksLbls = NULL){
  yBrksLbls[["label"]] <- 100 * yBrksLbls[[1]]
  # No other way for even space between labels and plot
  yBrksLbls[["label"]] <- as.character(paste(yBrksLbls[["label"]], "  ", sep = ""))
  yBrksLbls[[2]] <- as.character(paste(yBrksLbls[[2]], "  ", sep = ""))
  yBrksLbls[["label"]][!(yBrksLbls[["label"]] %in% yBrksLbls[[2]])] <- "" # for y labels
  yBrksLbls[["frame"]] <- as.data.frame(yBrksLbls[[1]])
  return(yBrksLbls)
}

# Returns ggPlot item for x axis 
make.xScale <- function(xRnge = NULL){
  xLims <- get.xLimits(xRnge)
  xBreaks <- get.xBreaks(xLims)
  return(
    list(scale_x_log10(
      limits = xLims, 
      breaks = xBreaks[[1]], 
      minor_breaks = xBreaks[[2]])))
}

# Returns ggPlot items for the y scale, including labels
make.yScale <- function(yRnge = NULL){
  yLims <- get.yLimits(yRnge)
  yBreaks <- get.yBreaks(yLims)
  # GLOBAL IS NECESSARY HERE ===============
  y.BreaksLabels <<- get.yLabels(yBreaks)
  # ==========================================
  return(
    list(
      scale_y_continuous(
        trans = "weibull",
        limits = as.numeric(unlist(yLims)),
        minor_breaks = y.BreaksLabels[[1]]
        # the secondary actually works, but it appears to lose scale
        # a similar issue is reported on github. The coming fix should handle it
        # https://github.com/tidyverse/ggplot2/pull/2095
        # sec.axis = sec_axis(~ weib_yScale(.), 
        #                     # using unicode here since no parse argument
        #                     name = c("AUXILLARY\n - ln(\u03b7)"), 
        #                     breaks = c(0.0))),
      ),
      
      geom_text(
        data = y.BreaksLabels[["frame"]], 
        aes(
          0,
          y.BreaksLabels[[1]],
          label = y.BreaksLabels[["label"]]),
        hjust = 1, 
        vjust = .5),
      theme(
        axis.title.y = 
          # Distance from plot
          element_text(margin = margin(0,40,0,0)))
    )
  )
}


make.scale.restrictions <- function(frme = NULL, restr = NULL){
#restr = c(xMin, yMin, xMax, yMax)
  
# The options here are limited, and not all cases are handled.
# I use this because ggplot's coord_cartesian will not pick up the scaling I have created
  if(!(is.numeric(restr)) | (length(restr) > 4)){
    stop(c("make.scale.restrictions: arguments are not valid (" , restr, ")"), call. = FALSE)
  }
  Len <- length(restr)
  if(Len == 1){
    if(restr > frme[[1]][1]){
      warning("scale.restrictions: Zooming in within data bounds", call. = FALSE)
    }
    frme[[1]][1] <- restr
  }
  else if (Len == 2){
    if((restr[1] > frme[[1]][1]) | restr[2] > frme[[2]][1]){
      warning("scale.restrictions: Zooming within data bounds", call. = FALSE)
    }
    frme[[1]][1] <- restr[1]
    frme[[2]][1] <- restr[2]
  }
  else if (Len == 3){
    if((restr[1] > frme[[1]][1]) | (restr[2] > frme[[2]][1]) | (restr[3] < frme[[1]][2]) ){
      warning("scale.restrictions: Zooming within data bounds", call. = FALSE)
    }
    frme[[1]][1] <- restr[1]
    frme[[2]][1] <- restr[2]
    frme[[1]][2] <- restr[3]
  }
  else if (Len == 4){
    if((restr[1] > frme[[1]][1]) | (restr[2] > frme[[2]][1]) | (restr[3] < frme[[1]][2]) |  (restr[4] < frme[[2]][2])){
      warning("scale.restrictions: Zooming within data bounds", call. = FALSE)
    }
    frme[[1]][1] <- restr[1]
    frme[[2]][1] <- restr[2]
    frme[[1]][2] <- restr[3]
    frme[[2]][2] <- restr[4]
  }
  return(frme)
}

get.scale.restrictions <- function(frme = NULL, restr = NULL){
  return(make.scale.restrictions(frme, restr))
}
# Returns all of the ggPlot items required for the x and y scale
# TODO: should have a make for this. split the vector function
get.xyScales <- function(frme = NULL, restr = NULL){
  if(length(frme) == 1){
    stop("get.xyScales: argument is length 1", call. = FALSE)
  }

  xyRnge <- get.xy.Range(frme)
  
  if(!is.null(restr)){
    xyRnge <- get.scale.restrictions(xyRnge, restr)
  }
  
  xScale <- make.xScale(xyRnge[[1]])
  yScale <- make.yScale(xyRnge[[2]])
  
  return(list(xScale, yScale))
}

# test if confidence level was specified
switch.interval <- function(level = NULL){
  return(ifelse(is.null(level), FALSE, TRUE))
}

# returns linear regression line on the data
make.LM.Line <- function(line = NULL, lvl = NULL){
  if(!('lm' %in% line)){
    return(NULL)
  }
  show.interval <- switch.interval(lvl)
  return(
    list(
      geom_smooth(
        method = 'lm',
        #fill = "yellow",
        #color = "blue",
        aes(fill = 'red', 
            color = "red"
            ),
        alpha = .3,
        se = show.interval,
        level = lvl, 
        show.legend = FALSE,
        fullrange = TRUE
      )
      
    #   coord_cartesian(
    #     # I could use Coord here and pass to it values I want, but then scale wont be generated
    #     # xlim = c(10, 10000),
    #     # ylim = c(.000001, .999),
    #     expand = FALSE)
    # )
  ))
}

# wrapper
get.LM.Line <- function(line = NULL, conf.lvl = NULL){
  return(make.LM.Line(line, conf.lvl))
}

# returns linear regression line on MLE estimate data
make.MLE.Line <- function(line = NULL, fmla = NULL, weibull.frame = NULL, lvl = NULL, fit = NULL){
  if(!('mle' %in% line)){
    return(NULL)
  }
  show.interval <- switch.interval(lvl)
  return(
    list(
      geom_smooth(
        inherit.aes = TRUE,
        aes(x = MLE.x,
            y = MLE.y,
            fill = 'lightgreen',
            color = 'red'),
        #fill = "cyan",
        data = weibull.frame,
        method = 'lm',
        alpha = .5,
        se = show.interval,
        level = lvl, 
        show.legend = TRUE
      )
    )
  )
}

# wrapper
get.MLE.Line <- function(line = NULL, fmla = NULL, frme = NULL, lvl = NULL, fit = NULL){
  return(make.MLE.Line(line, fmla, frme, lvl, fit))
}

# ggPlot item/style Wrappers
get.title <- function(title = NULL){
  return(list(ggtitle(title)))
}

get.labels <- function(sub.title = "WEIBULL SCALE PROBABILITY CHART", 
                        x.axis = "MILES", 
                        y.axis = "PERCENT FAILURE (%)"){#\n ln(ln( 1/( 1 - F(t) )))
  return(list(
    labs(
      subtitle = sub.title,
      x = x.axis,
      y = y.axis
    )
  ))
}

get.theme <- function(){
  return(list(
    theme(
      plot.title = element_text(size = 25,
                                hjust = .5,
                                face = "bold",
                                color = "black"),
      plot.subtitle = element_text(size = 13,
                                   hjust = .5,
                                   #face = "italic",
                                   color = "black"),
      axis.title.y = element_text(size = 15,
                                  face = "bold",
                                  color = "black"),
      axis.title.x = element_text(size = 15,
                                  face = "bold",
                                  color = "black"),
      # there is no y-axis text (those are labels from make.yScale)
      axis.text.x = element_text(size = 10,
                                 color = "black"),
      plot.margin = unit(c(.5,.5,.5,1),"cm"),
      panel.grid.minor = element_line(color = "white"),
      panel.background = element_rect(fill = "grey")
    ),
    coord_cartesian(
      expand = FALSE)
  ))
}

make.ggWeibull <- function(frme = NULL, title = NULL, line = NULL, conf.lvl = NULL, fit = NULL, restr = NULL ){
  #newFrame <- get.medianranks(frme)
  #fmla <- frme$MLE.y ~ frme$MLE.x
  weibItems <- list(
    # Cannot include initial ggplot() call 
    # geom_point is default
    geom_point(),
    # define title outside of labs() argument to seperate from subtitle styles
    get.title(title),
    get.labels(),
    get.theme(),
    get.xyScales(frme, restr),
    get.LM.Line(line, conf.lvl),
    get.MLE.Line(line, fmla, frme, conf.lvl, fit)
  )
  
  weibItems <- remove.nulls(weibItems)
 
  # NOTE: thefollowing argument will not work with ggplot using the weibull scale
  #   + coord_cartesian(xlim = c(1,10000),ylim= c(.000001,.999))
  ggWeib <- ggplot(data = frme, aes(frme[[1]], frme[[2]])) + geom_point(size = 4)+ weibItems + scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) + scale_colour_manual(name = 'the colour', values =c('black'='black','red'='red'), labels = c('c2','c1')) 
  
  options(scipen = 10)
  return(ggWeib)
}

get.ggWeibull <- function(frme = NULL, title = NULL, line = NULL, conf.lvl = NULL, fit = NULL, restr = NULL){
  options(scipen = 0)
  return(make.ggWeibull(frme, title, line, conf.lvl, fit, restr))
}

# This basically unravels the ggplot item
make.gTable <- function(ggPlt = NULL){
  # This gtable call is the reason for global 'y.BreaksLabels' from function make.yScales
  withCallingHandlers(gt <- ggplot_gtable(ggplot_build(ggPlt)),
                      warning = function(w){
                        # suppress this specific warning
                        if(grepl("Transformation introduced infinite values in continuous x-axis",
                                 w$message))
                          invokeRestart("muffleWarning")
                      })
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  return(gt)
}
get.gTable <- function(ggPlt = NULL){
  return(make.gTable(ggPlt))
}

make.weibull.analysis <- function(failure.data = NULL, title = NULL, line = NULL, conf.lvl = NULL, restr = NULL){
  
  # median ranks is specific for weibull plotting (for now)
  failure.data <- get.medianranks(failure.data)

  # the data is right censored
  # probably want to seperate this call (its general)
  
  surv.item <- get.right.censored(failure.data[[1]])
  
  # but this call should be ok here since i generalized the actual weibull fit
  weibull.fit <- get.weibull.reg(surv.item)
  
  # may not need these here...
  weibull.shape <- get.weibull.shape(weibull.fit)
  weibull.scale <- get.weibull.scale(weibull.fit)
  
  # adding the MLE estimates to data. May be able to move this. maybe with the above...
  failure.data$MLE.x <- qweibull(failure.data$median.ranks, weibull.shape, weibull.scale)
  failure.data$MLE.y <- pweibull(failure.data[[1]], weibull.shape, weibull.scale)
  
  # should return a list of items! (instead of just the plot)
  return(get.gTable(get.ggWeibull(failure.data, title, line, conf.lvl, weibull.fit, restr)))
}

# main
get.weibull.analysis <- function(failure.data = NULL, title = NULL, line = NULL, conf.lvl = NULL, restr = NULL){
  return(make.weibull.analysis(failure.data, title, line, conf.lvl, restr))
}

# ============ End Weibull Scale Plotting functions ===========================