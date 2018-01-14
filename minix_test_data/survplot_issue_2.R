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

> weib.fit
Fitting of the distribution ' weibull ' by maximum likelihood 
Parameters:
  estimate Std. Error
shape    7.241311  0.2588041
scale 2517.689836 17.8897868

# Plot all, and add to legend
plot.all <- nonParametric.plot$plot + stat_function(fun = pweibull, size = 2, args = list(weib.fit$estimate[1], weib.fit$estimate[2]), aes(colour = "Weibull")) + scale_colour_manual(values = c("blue"))

At first, it is asking for 3 colors in the manual adjustment, because of the 2 previous survival fits.

> plot.all
Error: Insufficient values in manual scale. 3 needed but only 1 provided.

So, lets add 2 more colors (despite that we already specified palette() in the ggsurvplot() call).
# Plot all, and add to legend
plot.all <- nonParametric.plot$plot + stat_function(fun = pweibull, size = 1, args = list(weib.fit$estimate[1], weib.fit$estimate[2]), aes(colour = "Weibull")) + scale_colour_manual(values = c("green", "red","blue"))




