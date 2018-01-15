# Use only uncensored subject data (those who died due to recurrence)
colon.new <- merge(aggregate(time ~ id, data = colon, max), aggregate(status ~ id, data = colon, max))
colon.new <- colon.new[!(colon.new$status %in% 1), ]

# Create Kaplan-Maier estimate
colon.kaplan <- survfit(Surv(time) ~ 1, data = colon.new)

# Plot KM estimate
ggsurv.km <- ggsurvplot(colon.kaplan, legend = "right", legend.title = "LEGEND", legend.labs = c("Kaplan-Meier"), fun = 'event', ggtheme = theme_gray(), conf.int = TRUE, linetype = c(1), palette = "strata", censor = FALSE)
km.plot <- ggsurv.km + ggtitle("DEATH WITH RECURRENCE") + labs(subtitle = "(The Uncensored Data)", x ="TIME", y = "CDF")


# Create Cox Proportional Hazard estimate
colon.coxPH <- survfit(coxph(Surv(time) ~ 1, data = colon.new))

# List both KM and coxPH estimates for overlay comparison
both.fits <- list(Kaplan = colon.kaplan, Cox = colon.coxPH)

# Plot KM estimate , combine = TRUE
ggsurv.both <- ggsurvplot(both.fits, legend = "right", legend.title = "LEGEND", legend.labs = c("Kaplan-Meier", "Cox PH"), linetype = c(1, 2), combine = TRUE, fun = 'event', ggtheme = theme_gray(), conf.int = TRUE, palette = "strata", censor = FALSE)
nonParametric.plot <- ggsurv.both + ggtitle("DEATH WITH RECURRENCE") + labs(subtitle = "(The Uncensored Data)", x ="TIME", y = "CDF")

library("fitdistrplus")

# Fit weibull distribution
colon.time <- colon.new$time
weib.fit <- fitdist(colon.time, distr = "weibull", method = "mle")

# > weib.fit
# Fitting of the distribution ' weibull ' by maximum likelihood 
# Parameters:
#   estimate Std. Error
# shape    7.241311  0.2588041
# scale 2517.689836 17.8897868

# Plot all, and add to legend
plot.all <- nonParametric.plot$plot + stat_function(fun = pweibull, size = 2, args = list(weib.fit$estimate[1], weib.fit$estimate[2]), aes(colour = "Weibull")) + scale_colour_manual(values = c("blue"))

#At first, it is asking for 3 colors in the manual adjustment, because of the 2 previous survival fits.

# > plot.all
# Error: Insufficient values in manual scale. 3 needed but only 1 provided.

So, lets add 2 more colors (despite that we already specified palette() in the ggsurvplot() call).
# Plot all, and add to legend
plot.all <- nonParametric.plot$plot + stat_function(fun = pweibull, size = 1, args = list(weib.fit$estimate[1], weib.fit$estimate[2]), aes(colour = "Weibull")) + scale_colour_manual(values = c("green", "red","blue"))


# CORRECTION
# Use only uncensored subject data (those who died with recurrence)
colon.new <- merge(aggregate(time ~ id, data = colon, max), aggregate(status ~ id, data = colon, max))
colon.new <- colon.new[!(colon.new$status %in% 1), ]

# 1. Km and cox fits
colon.kaplan <- survfit(Surv(time) ~ 1, data = colon.new)
colon.coxPH <- survfit(coxph(Surv(time) ~ 1, data = colon.new))

both.fits <- list(Kaplan = colon.kaplan, Cox = colon.coxPH)

ggsurv.both <- ggsurvplot(both.fits, 
                          legend = "right",
                          legend.labs = c("Kaplan-Meier", "Cox PH"),
                          linetype = c(1, 2),  
                          conf.int = TRUE,
                          combine = TRUE,
                          palette = "jco",
                          fun = 'event', 
                          ggtheme = theme_gray(), 
                          censor = FALSE)

#shape = override.shape, , 
# override.aes = list(linetype = c(2, 3))
ggsurv.both + guides(color = guide_legend(title = "COLOR LEGEND"),
                     fill = guide_legend(title = "FILL LEGEND"),
                     linetype = guide_legend(title = "LINETYPE LEGEND"),
                     size = guide_legend(title = "SIZE LEGEND"))
                      


nonParametric.plot <- ggsurv.both + 
  ggtitle("A LOT OF LEGENDS!") + 
  labs(subtitle = "(but where's the size legend?...)", x ="TIME", y = "CDF")

# 2. Fit weibull distribution
library("fitdistrplus")
colon.time <- colon.new$time
weib.fit <- fitdist(colon.time, distr = "weibull", method = "mle")


plot.all <- nonParametric.plot$plot + 
  # theme(legend.background = element_rect(fill = "grey92",   #fill color
  #                                        color = "black",   #border color
  #                                        size = 2,          #border size
  #                                        linetype = 2),     #border linetype
  #       legend.title = element_text("LEGEND")) 
  stat_function(fun = pweibull,
                size = 2,
                args = list(weib.fit$estimate[1],
                            weib.fit$estimate[2]),
                aes(colour = "Weibull")) + 
  scale_size_discrete(guide = "legend")
  
  # scale_colour_manual(values=c("red","blue", "green"),
  #                     name="LEGEND", 
  #                     #breaks=c("A", "B", "C"), 
  #                     labels = "Weibull") +
  # scale_fill_manual(values = c(#values=c("red","blue"),
  #                              name="LEGEND",
  #                              labels = c("Kaplan-Meier", "Cox PH", "Weibull")))#+
  # scale_linetype_discrete(name="LEGEND", 
  #                         breaks=c("A", "B", "C"), 
  #                         labels = c("easy", "medium", "hard"))
  # 



# new example

# generate random weibull data
random.weib1 <- as.data.frame(rweibull(45, shape = .5, scale = 1))
names(random.weib1) <- "dat1"
random.weib2 <- as.data.frame(rweibull(45, shape = 1.5, scale = 1))
names(random.weib2) <- "dat2"

# fit Kaplan-Meier Estimate
weib1.kaplan <- survfit(Surv(dat1) ~ 1, data = random.weib1)
weib2.kaplan <- survfit(Surv(dat2) ~ 1, data = random.weib2)

# list both kaplan estimates
both.fits <- list(Kaplan.1 = weib1.kaplan, Kaplan.2 = weib2.kaplan)

# fit weibull distribution
weib.fit1 <- fitdist(random.weib1$dat1, distr = "weibull", method = "mle")
weib.fit2 <- fitdist(random.weib2$dat2, distr = "weibull", method = "mle")

# plot both
ggsurv.both <- ggsurvplot(both.fits, 
                          legend = "right",
                          legend.labs = c("Random 1", "Random 2"),
                          linetype = c(2, 3),  
                          conf.int = TRUE,
                          combine = TRUE,
                          palette = "jco",
                          fun = 'event', 
                          ggtheme = theme_gray(), 
                          censor = FALSE)

all.plot <- ggsurv.both + 
  ggtitle("A LOT OF LEGENDS!") + 
  labs(subtitle = "but where's the size legend?...", x ="TIME", y = "CDF")+
  stat_function(fun = pweibull,
                size = 2,
                args = list(weib.fit1$estimate[1],
                            weib.fit1$estimate[2]),
                aes(color = "Weibull Fit 1")) +
  stat_function(fun = pweibull,
                size = 1,
                args = list(weib.fit2$estimate[1],
                            weib.fit2$estimate[2]),
                aes(color = "Weibull Fit 2")) + 
  guides(color = guide_legend(title = "COLOR LEGEND"),
         fill = guide_legend(title = "FILL LEGEND"),
         linetype = guide_legend(title = "LINETYPE LEGEND"),
         size = guide_legend(title = "SIZE LEGEND")) +
  coord_cartesian(xlim = c(0, 4))

# scale_linetype_manual(values = c("dotted", "dashed", "dotdash", "solid"))

