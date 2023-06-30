# WORKING DIRECTORY
getwd()

# PACKAGES
# install.packages("devtools")
# devtools::install_github("m-clark/visibly")
library(visibly)
library(dplyr)
library(mgcv)
library(MuMIn)
library(ggcorrplot)
library(ggplot2)
library(lme4)
library(nlme)
library(arm)
library(AICcmodavg)
library(devtools)
library(tidyr)
library(gridExtra)

####################################################################################################################

# IMPORT DATA
grazers.detritivores.metrics <- read.csv("grazers_detritivores.csv")
large.small.excavators.metrics <- read.csv("large_and_small_excavators.csv")
corallivores.metrics <- read.csv("corallivores.csv")
primary.piscivores.metrics <- read.csv("primary_piscivores.csv")

grazers.detritivores.metrics.df <- as.data.frame(grazers.detritivores.metrics)
large.small.excavators.metrics.df <- as.data.frame(large.small.excavators.metrics)
corallivores.metrics.df <- as.data.frame(corallivores.metrics)
primary.piscivores.metrics.df <- as.data.frame(primary.piscivores.metrics)

####################################################################################################################

# CORRELATIONS

########################
### Grazers Detritivores
grazers.detritivores.corr <- grazers.detritivores.metrics[,c(5,14,19)]
grazers.detritivores.matrix <- data.matrix(grazers.detritivores.corr, rownames.force = NA)
grazers.detritivores.matrix.df <- as.data.frame(grazers.detritivores.matrix) # input for corr plot matrix

# Compute a correlation matrix
grazers.detritivores.corr.matrix <- cor(grazers.detritivores.matrix, use = "complete.obs")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(grazers.detritivores.corr.matrix)

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(grazers.detritivores.corr.matrix)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(grazers.detritivores.corr.matrix, hc.order = TRUE, outline.col = "white")

# Get the upper triangle
ggcorrplot(grazers.detritivores.corr.matrix, hc.order = TRUE, type = "upper",
           outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(grazers.detritivores.corr.matrix, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
jpeg(file="correlation_plot_ratios_grazers_detritivors.jpg", width=15, height=15, units="cm", res=600)
ggcorrplot(grazers.detritivores.corr.matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
dev.off()

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(grazers.detritivores.corr.matrix, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Leave blank on no significant coefficient
ggcorrplot(grazers.detritivores.corr.matrix, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

##############################
### Large and Small Excavators
large.small.excavators.metrics.corr <- large.small.excavators.metrics[,c(5,14,19)]
large.small.excavators.matrix <- data.matrix(large.small.excavators.metrics.corr, rownames.force = NA)
large.small.excavators.matrix.df <- as.data.frame(large.small.excavators.matrix) # input for corr plot matrix

# Compute a correlation matrix
large.small.excavators.corr.matrix <- cor(large.small.excavators.matrix, use = "complete.obs")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(large.small.excavators.corr.matrix)

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(large.small.excavators.corr.matrix)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(large.small.excavators.corr.matrix, hc.order = TRUE, outline.col = "white")

# Get the upper triangle
ggcorrplot(large.small.excavators.corr.matrix, hc.order = TRUE, type = "upper",
           outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(large.small.excavators.corr.matrix, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
jpeg(file="correlation_plot_ratios_large_small_excavators.jpg", width=15, height=15, units="cm", res=600)
ggcorrplot(large.small.excavators.corr.matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
dev.off()

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(large.small.excavators.corr.matrix, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Leave blank on no significant coefficient
ggcorrplot(large.small.excavators.corr.matrix, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

################
### Corallivores
corallivores.metrics.corr <- corallivores.metrics[,c(5,14,19)]
corallivores.matrix <- data.matrix(corallivores.metrics.corr, rownames.force = NA)
corallivores.matrix.df <- as.data.frame(corallivores.matrix) # input for corr plot matrix

# Compute a correlation matrix
corallivores.corr.matrix <- cor(corallivores.matrix, use = "complete.obs")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corallivores.corr.matrix)

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corallivores.corr.matrix)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corallivores.corr.matrix, hc.order = TRUE, outline.col = "white")

# Get the upper triangle
ggcorrplot(corallivores.corr.matrix, hc.order = TRUE, type = "upper",
           outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corallivores.corr.matrix, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
jpeg(file="correlation_plot_ratios_corallivores.jpg", width=15, height=15, units="cm", res=600)
ggcorrplot(corallivores.corr.matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
dev.off()

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corallivores.corr.matrix, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Leave blank on no significant coefficient
ggcorrplot(corallivores.corr.matrix, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

######################
### Primary Piscivores
primary.piscivores.metrics.corr <- primary.piscivores.metrics[,c(5,14,19)]
primary.piscivores.matrix <- data.matrix(primary.piscivores.metrics.corr, rownames.force = NA)
primary.piscivores.matrix.df <- as.data.frame(primary.piscivores.matrix) # input for corr plot matrix

# Compute a correlation matrix
primary.piscivores.corr.matrix <- cor(primary.piscivores.matrix, use = "complete.obs")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(primary.piscivores.corr.matrix)

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(primary.piscivores.corr.matrix)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(primary.piscivores.corr.matrix, hc.order = TRUE, outline.col = "white")

# Get the upper triangle
ggcorrplot(primary.piscivores.corr.matrix, hc.order = TRUE, type = "upper",
           outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(primary.piscivores.corr.matrix, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
jpeg(file="correlation_plot_ratios_primary_piscivores.jpg", width=15, height=15, units="cm", res=600)
ggcorrplot(primary.piscivores.corr.matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
dev.off()

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(primary.piscivores.corr.matrix, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Leave blank on no significant coefficient
ggcorrplot(primary.piscivores.corr.matrix, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

##################################################################################
### Correlation scatter plot example (for largest group: grazers and detritivores)
jpeg(file="correlation_plot_scatter.jpg", width=50, height=30, units="cm", res=600)
pairs(grazers.detritivores.matrix.df, pch = 19, cex = 1, cex.labels = 1.9, font.labels = 2,  cex.axis = 1.5, lower.panel = NULL)
dev.off()

####################################################################################################################

# NAME VARIABLES

grazers.detritivores.year <- grazers.detritivores.metrics$Year
grazers.detritivores.sst.mean <- grazers.detritivores.metrics$MEANsst
grazers.detritivores.sst.sd <- grazers.detritivores.metrics$SDsst
grazers.detritivores.sst.anomaly <- grazers.detritivores.metrics$ANOMALYsst
grazers.detritivores.chla.mean <- grazers.detritivores.metrics$MEANchla
grazers.detritivores.conn <- grazers.detritivores.metrics$pcConn
grazers.detritivores.abund <- grazers.detritivores.metrics$Abundance

large.small.excavators.year <- large.small.excavators.metrics$Year
large.small.excavators.sst.mean <- large.small.excavators.metrics$MEANsst
large.small.excavators.sst.sd <- large.small.excavators.metrics$SDsst
large.small.excavators.sst.anomaly <- large.small.excavators.metrics$ANOMALYsst
large.small.excavators.chla.mean <- large.small.excavators.metrics$MEANchla
large.small.excavators.conn <- large.small.excavators.metrics$pcConn
large.small.excavators.abund <- large.small.excavators.metrics$Abundance

corallivores.year <- corallivores.metrics$Year
corallivores.sst.mean <- corallivores.metrics$MEANsst
corallivores.sst.sd <- corallivores.metrics$SDsst
corallivores.sst.anomaly <- corallivores.metrics$ANOMALYsst
corallivores.chla.mean <- corallivores.metrics$MEANchla
corallivores.conn <- corallivores.metrics$pcConn
corallivores.abund <- corallivores.metrics$Abundance

primary.piscivores.year <- primary.piscivores.metrics$Year
primary.piscivores.sst.mean <- primary.piscivores.metrics$MEANsst
primary.piscivores.sst.sd <- primary.piscivores.metrics$SDsst
primary.piscivores.sst.anomaly <- primary.piscivores.metrics$ANOMALYsst
primary.piscivores.chla.mean <- primary.piscivores.metrics$MEANchla
primary.piscivores.conn <- primary.piscivores.metrics$pcConn
primary.piscivores.abund <- primary.piscivores.metrics$Abundance

####################################################################################################################

# MODELLING

############################
### Grazers and Detritivores

# Poisson Regression
# where count is a count and x1-x3 are continuous predictors (pcConn is percentage)
# glm.grazers.detritivores <- glm(grazers.detritivores.abund ~ grazers.detritivores.sst.mean + grazers.detritivores.chla.mean +
# grazers.detritivores.conn, data = grazers.detritivores.metrics.df, family = poisson())
# summary(glm.grazers.detritivores)

# Poisson Regression with interaction
# glm.grazers.detritivores.interaction <- glm(grazers.detritivores.abund ~ grazers.detritivores.sst.mean +
# grazers.detritivores.chla.mean + grazers.detritivores.sst.mean*grazers.detritivores.chla.mean + grazers.detritivores.conn,
# data = grazers.detritivores.metrics.df, family = poisson())
# summary(glm.grazers.detritivores.interaction)

# Poisson Model (mixed linear model)
# glmer.grazers.detritivores <- glmer(grazers.detritivores.abund ~ grazers.detritivores.sst.mean + grazers.detritivores.chla.mean +
# grazers.detritivores.conn + (1|grazers.detritivores.year), data = grazers.detritivores.metrics.df, family = "poisson")
# summary(glmer.grazers.detritivores)

############
# GAM Models
gam.grazers.detritivores.mean = gam(grazers.detritivores.abund ~ s(grazers.detritivores.sst.mean) + s(grazers.detritivores.chla.mean) +
                                      s(grazers.detritivores.conn), method = "ML", family = "poisson")

# gam.grazers.detritivores.sd = gam(grazers.detritivores.abund ~ s(grazers.detritivores.sst.sd) + s(grazers.detritivores.chla.sd) +
#                                 s(grazers.detritivores.conn), method = "ML", family = "poisson")

# gam.grazers.detritivores.all = gam(grazers.detritivores.abund ~ s(grazers.detritivores.sst.mean) + s(grazers.detritivores.chla.mean) +
# s(grazers.detritivores.sst.sd) + s(grazers.detritivores.chla.sd) + s(grazers.detritivores.conn), method = "ML", family = "poisson")

gam.grazers.detritivores.mean.only = gam(grazers.detritivores.abund ~ s(grazers.detritivores.sst.mean) + s(grazers.detritivores.chla.mean),
                                         method = "ML", family = "poisson")

# gam.grazers.detritivores.sd.only = gam(grazers.detritivores.abund ~ s(grazers.detritivores.sst.sd) + s(grazers.detritivores.chla.sd),
#                                        method = "ML", family = "poisson")

# Model Summaries
summary(gam.grazers.detritivores.mean)
# summary(gam.grazers.detritivores.sd)
# summary(gam.grazers.detritivores.all)
summary(gam.grazers.detritivores.mean.only)
# summary(gam.grazers.detritivores.sd.only)

# AICC Values
AICc(gam.grazers.detritivores.mean)
# AICc(gam.grazers.detritivores.sd)
# AICc(gam.grazers.detritivores.all)
AICc(gam.grazers.detritivores.mean.only)
# AICc(gam.grazers.detritivores.sd.only)

# AICc Weights
sum.delta.AICc.grazers.detritivores <- sum(exp(-0.5 * (1589.851 - 1505.969)) + exp(-0.5 * (1636.288 - 1505.969)) +
                                             exp(-0.5 * (1505.969 - 1505.969)) +  exp(-0.5 * (1722.451 - 1505.969)) +
                                             exp(-0.5 * (1926.771 - 1505.969)))

AICc.gam.grazers.detritivores.mean <- exp(-0.5 * (1589.851 - 1505.969)) / sum.delta.AICc.grazers.detritivores
# AICc.gam.grazers.detritivores.sd <- exp(-0.5 * (1636.288 - 1505.969)) / sum.delta.AICc.grazers.detritivores
# AICc.gam.grazers.detritivores.all <- exp(-0.5 * (1505.969 - 1505.969)) / sum.delta.AICc.grazers.detritivores
AICcgam.grazers.detritivores.mean.only <- exp(-0.5 * (1722.451 - 1505.969)) / sum.delta.AICc.grazers.detritivores
# AICc.gam.grazers.detritivores.sd.only <- exp(-0.5 * (1926.771 - 1505.969)) / sum.delta.AICc.grazers.detritivores

# Estimated Degrees of Freedom
sum(influence(gam.grazers.detritivores.mean))
# sum(influence(gam.grazers.detritivores.sd))
# sum(influence(gam.grazers.detritivores.all))
sum(influence(gam.grazers.detritivores.mean.only))
# sum(influence(gam.grazers.detritivores.sd.only))

# GAM Check Plots
jpeg(file="gam_check_mean_grazers_detritivores.jpg", width=32, height=32, units="cm", res=600)
plot_gam_check(
  gam.grazers.detritivores.mean,
  single_page = TRUE,
  type = "deviance",
  scatter = FALSE,
  kcheck = FALSE
)
dev.off()

# jpeg(file="gam_check_sd_grazers_detritivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.grazers.detritivores.sd,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_all_grazers_detritivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.grazers.detritivores.all,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_mean_only_grazers_detritivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.grazers.detritivores.mean.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_sd_only_grazers_detritivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.grazers.detritivores.sd.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# Concurvity
concurvity(gam.grazers.detritivores.mean, full = FALSE)
concurvity(gam.grazers.detritivores.sd, full = FALSE)
concurvity(gam.grazers.detritivores.all, full = FALSE)
concurvity(gam.grazers.detritivores.mean.only, full = FALSE)
concurvity(gam.grazers.detritivores.sd.only, full = FALSE)

# PLOTTING
jpeg(file="gam_MEANsst_grazers_detritivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.grazers.detritivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.grazers.detritivores.mean)[1], xlab = "Mean sea surface temperature (º C)",
     ylab = "Abundance grazers and detritivores (count)", select = c(1), shade.col = "lightgrey", trans = exp, ylim = c(0,350))
dev.off()

jpeg(file="gam_MEANchla_grazers_detritivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.grazers.detritivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.grazers.detritivores.mean)[1], xlab = "Mean chlorophyll a (mg/m3)",
     ylab = "Abundance grazers and detritivores (count)", select = c(2), shade.col = "lightgrey", trans = exp, ylim = c(0,350))
dev.off()


jpeg(file="gam_conn_grazers_detritivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.grazers.detritivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.grazers.detritivores.mean)[1], xlab = "Oceanographic connectivity (%)",
     ylab = "Abundance grazers and detritivores (count)", select = c(3), shade.col = "lightgrey", trans = exp)
dev.off()

##############################
### Large and Small Excavators

# Model Comparisons
# Poisson Regression
# where count is a count and x1-x3 are continuous predictors (pcConn is percentage)
# glm.large.small.excavators <- glm(large.small.excavators.abund ~ large.small.excavators.sst.mean +
# large.small.excavators.chla.mean + large.small.excavators.conn, data = large.small.excavators.metrics.df, family = poisson())
# summary(glm.large.small.excavators)

# Poisson Regression with interaction
# glm.large.small.excavators.interaction <- glm(large.small.excavators.abund ~ large.small.excavators.sst.mean + 
# large.small.excavators.chla.mean + large.small.excavators.sst.mean*large.small.excavators.chla.mean + large.small.excavators.conn,
# data = large.small.excavators.metrics.df, family = poisson())
# summary(glm.large.small.excavators.interaction)

# Poisson Model (mixed linear model)
# glmer.large.small.excavators <- glmer(large.small.excavators.abund ~ large.small.excavators.sst.mean + large.small.excavators.chla.mean +
# large.small.excavators.conn + (1|large.small.excavators.year), data = large.small.excavators.metrics.df, family = "poisson")
# summary(glmer.large.small.excavators)

############
# GAM Models
gam.large.small.excavators.mean = gam(large.small.excavators.abund ~ s(large.small.excavators.sst.mean) +
                                        s(large.small.excavators.chla.mean) + s(large.small.excavators.conn),
                                        method = "ML", family = "poisson")

# gam.large.small.excavators.sd = gam(large.small.excavators.abund ~ s(large.small.excavators.sst.sd) +
#                                     s(large.small.excavators.chla.sd) + s(large.small.excavators.conn),
#                                     method = "ML", family = "poisson")

# gam.large.small.excavators.all = gam(large.small.excavators.abund ~ s(large.small.excavators.sst.mean) +
#                                      s(large.small.excavators.chla.mean) + s(large.small.excavators.sst.sd) +
#                                      s(large.small.excavators.chla.sd) + s(large.small.excavators.conn),
#                                      method = "ML", family = "poisson")

gam.large.small.excavators.mean.only = gam(large.small.excavators.abund ~ s(large.small.excavators.sst.mean) + 
                                             s(large.small.excavators.chla.mean), method = "ML", family = "poisson")

# gam.large.small.excavators.sd.only = gam(large.small.excavators.abund ~ s(large.small.excavators.sst.sd) +
# s(large.small.excavators.chla.sd), method = "ML", family = "poisson")

# Model Summaries
summary(gam.large.small.excavators.mean)
# summary(gam.large.small.excavators.sd)
# summary(gam.large.small.excavators.all)
summary(gam.large.small.excavators.mean.only)
# summary(gam.large.small.excavators.sd.only)

# AICc Values
AICc(gam.large.small.excavators.mean)
# AICc(gam.large.small.excavators.sd)
# AICc(gam.large.small.excavators.all)
AICc(gam.large.small.excavators.mean.only)
# AICc(gam.large.small.excavators.sd.only)

# AICc Weights
sum.delta.AICc.large.small.excavators <- sum(exp(-0.5 * (1321.101 - 1096.842)) + exp(-0.5 * (1096.842 - 1096.842)) +
                                               exp(-0.5 * (1220.281 - 1096.842)) +  exp(-0.5 * (1657.719 - 1096.842)) +
                                               exp(-0.5 * (1448.58 - 1096.842)))

AICc.gam.large.small.excavators.mean <- exp(-0.5 * (1321.101 - 1096.842)) / sum.delta.AICc.large.small.excavators
# AICc.gam.large.small.excavators.sd <- exp(-0.5 * (1096.842 - 1096.842)) / sum.delta.AICc.large.small.excavators
# AICc.gam.large.small.excavators.all <- exp(-0.5 * (1220.281 - 1096.842)) / sum.delta.AICc.large.small.excavators
AICc.gam.large.small.excavators.mean.only <- exp(-0.5 * (1657.719 - 1096.842)) / sum.delta.AICc.large.small.excavators
# AICc.gam.large.small.excavators.sd.only <- exp(-0.5 * (1448.58 - 1096.842)) / sum.delta.AICc.large.small.excavators

# Estimated Degrees of Freedom
sum(influence(gam.large.small.excavators.mean))
# sum(influence(gam.large.small.excavators.sd))
# sum(influence(gam.large.small.excavators.all))
sum(influence(gam.large.small.excavators.mean.only))
# sum(influence(gam.large.small.excavators.sd.only))

# GAM Check Plots
jpeg(file="gam_check_mean_large_small_excavators.jpg", width=32, height=32, units="cm", res=600)
plot_gam_check(
  gam.large.small.excavators.mean,
  single_page = TRUE,
  type = "deviance",
  scatter = FALSE,
  kcheck = FALSE
)
dev.off()

# jpeg(file="gam_check_sd_large_small_excavators.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.large.small.excavators.sd,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_all_large_small_excavators.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.large.small.excavators.all,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_mean_only_large_small_excavators.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.large.small.excavators.mean.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_sd_only_large_small_excavators.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.large.small.excavators.sd.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# Concurvity
concurvity(gam.large.small.excavators.mean, full = FALSE)
# concurvity(gam.large.small.excavators.sd, full = FALSE)
# concurvity(gam.large.small.excavators.all, full = FALSE)
concurvity(gam.large.small.excavators.mean.only, full = FALSE)
# concurvity(gam.large.small.excavators.sd.only, full = FALSE)

# PLOTTING
jpeg(file="gam_MEANsst_large_small_excavators.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.large.small.excavators.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.large.small.excavators.mean)[1],
     xlab = "Mean sea surface temperature (º C)", ylab = "Abundance large and small excavators (count)", select = c(1),
     shade.col = "lightgrey", trans = exp, ylim = c(0,110))

dev.off()

jpeg(file="gam_MEANchla_large_small_excavators.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.large.small.excavators.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.large.small.excavators.mean)[1],
     xlab = "Mean chlorophyll a (mg/m3)", ylab = "Abundance large and small excavators (count)", select = c(2),
     shade.col = "lightgrey", trans = exp, ylim = c(0,250))

dev.off()

jpeg(file="gam_conn_large_small_excavators.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.large.small.excavators.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.large.small.excavators.mean)[1],
     xlab = "Oceanographic connectivity (%)", ylab = "Abundance large and small excavators (count)", select = c(3),
     shade.col = "lightgrey", trans = exp, ylim = c(0,1000))

dev.off()

################
### Corallivores

# Model Comparisons
# Poisson Regression
# where count is a count and x1-x3 are continuous predictors (pcConn is percentage)
# glm.corallivores <- glm(corallivores.abund ~ corallivores.sst.mean + corallivores.chla.mean + corallivores.conn,
#                         data = corallivores.metrics.df, family = poisson())
# summary(glm.corallivores)

# Poisson Regression with interaction
# glm.corallivores.interaction <- glm(corallivores.abund ~ corallivores.sst.mean + corallivores.chla.mean +
# corallivores.sst.mean*corallivores.chla.mean + corallivores.conn, data = corallivores.metrics.df, family = poisson())
# summary(glm.corallivores.interaction)

# Poisson Model (mixed linear model)
# glmer.corallivores <- glmer(corallivores.abund ~ corallivores.sst.mean + corallivores.chla.mean + corallivores.conn +
# (1|corallivores.year), data = corallivores.metrics.df, family = "poisson")
# summary(glmer.corallivores)

############
# GAM Models
gam.corallivores.mean = gam(corallivores.abund ~ s(corallivores.sst.mean) + s(corallivores.chla.mean) +
                           s(corallivores.conn), method = "ML", family = "poisson")

# gam.corallivores.sd = gam(corallivores.abund ~ s(corallivores.sst.sd) + s(corallivores.chla.sd) +
#                               s(corallivores.conn), method = "ML", family = "poisson")

# gam.corallivores.all = gam(corallivores.abund ~ s(corallivores.sst.mean) + s(corallivores.chla.mean) + s(corallivores.sst.sd) +
# s(corallivores.chla.sd) + s(corallivores.conn), method = "ML", family = "poisson")

gam.corallivores.mean.only = gam(corallivores.abund ~ s(corallivores.sst.mean) + s(corallivores.chla.mean), method = "ML", family = "poisson")

# gam.corallivores.sd.only = gam(corallivores.abund ~ s(corallivores.sst.sd) + s(corallivores.chla.sd), method = "ML", family = "poisson")

# Model Summaries
summary(gam.corallivores.mean)
# summary(gam.corallivores.sd)
# summary(gam.corallivores.all)
summary(gam.corallivores.mean.only)
# summary(gam.corallivores.sd.only)

# AICc Values
AICc(gam.corallivores.mean)
# AICc(gam.corallivores.sd)
# AICc(gam.corallivores.all)
AICc(gam.corallivores.mean.only)
# AICc(gam.corallivores.sd.only)

# AICc Weights
sum.delta.AICc.corallivores <- sum(exp(-0.5 * (571.8982 - 571.8982)) + exp(-0.5 * (583.7218 - 571.8982)) +
                                     exp(-0.5 * (586.5891 - 571.8982)) +  exp(-0.5 * (584.1719 - 571.8982)) +
                                     exp(-0.5 * (640.9652 - 571.8982)))
AICc.gam.corallivores.mean <- exp(-0.5 * (571.8982 - 571.8982)) / sum.delta.AICc.corallivores
# AICc.gam.corallivores.sd <- exp(-0.5 * (583.7218 - 571.8982)) / sum.delta.AICc.corallivores
# AICc.gam.corallivores.all <- exp(-0.5 * (586.5891 - 571.8982)) / sum.delta.AICc.corallivores
AICc.gam.corallivores.mean.only <- exp(-0.5 * (584.1719 - 571.8982)) / sum.delta.AICc.corallivores
# AICc.gam.corallivores.sd.only <- exp(-0.5 * (640.9652 - 571.8982)) / sum.delta.AICc.corallivores

# Estimated Degrees of Freedom
sum(influence(gam.corallivores.mean))
# sum(influence(gam.corallivores.sd))
# sum(influence(gam.corallivores.all))
sum(influence(gam.corallivores.mean.only))
# sum(influence(gam.corallivores.sd.only))

# GAM Check Plots
jpeg(file="gam_check_mean_corallivores.jpg", width=32, height=32, units="cm", res=600)
plot_gam_check(
  gam.corallivores.mean,
  single_page = TRUE,
  type = "deviance",
  scatter = FALSE,
  kcheck = FALSE
)
dev.off()

# jpeg(file="gam_check_sd_corallivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.corallivores.sd,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_all_corallivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.corallivores.all,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_mean_only_corallivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.corallivores.mean.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_sd_only_corallivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.corallivores.sd.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# Concurvity
concurvity(gam.corallivores.mean, full = FALSE)
# concurvity(gam.corallivores.sd, full = FALSE)
# concurvity(gam.corallivores.all, full = FALSE)
concurvity(gam.corallivores.mean.only, full = FALSE)
# concurvity(gam.corallivores.sd.only, full = FALSE)

# PLOTTING
jpeg(file="gam_MEANsst_corallivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.corallivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r", shift = coef(gam.corallivores.mean)[1],
     xlab = "Mean sea surface temperature (º C)", ylab = "Abundance corallivores (count)", select = c(1), shade.col = "lightgrey", trans = exp, ylim = c(0, 110))
dev.off()

jpeg(file="gam_MEANchla_corallivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.corallivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r", shift = coef(gam.corallivores.mean)[1],
     xlab = "Mean chlorophyll a (mg/m3)", ylab = "Abundance corallivores (count)", select = c(2), shade.col = "lightgrey", trans = exp, ylim = c(0, 110))
dev.off()

jpeg(file="gam_conn_corallivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.corallivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r", shift = coef(gam.corallivores.mean)[1],
     xlab = "Oceanographic connectivity (%)", ylab = "Abundance corallivores (count)", select = c(3), shade.col = "lightgrey", trans = exp, ylim = c(0, 110))
dev.off()

######################
### Primary Piscivores

# Model Comparisons
# Poisson Regression
# where count is a count and x1-x3 are continuous predictors (pcConn is percentage)
# glm.primary.piscivores <- glm(primary.piscivores.abund ~ primary.piscivores.sst.mean + primary.piscivores.chla.mean +
# primary.piscivores.conn, data = primary.piscivores.metrics.df, family = poisson())
# summary(glm.primary.piscivores)

# Poisson Regression with interaction
# glm.primary.piscivores.interaction <- glm(primary.piscivores.abund ~ primary.piscivores.sst.mean + primary.piscivores.chla.mean +
# primary.piscivores.sst.mean*primary.piscivores.chla.mean + primary.piscivores.conn, data = primary.piscivores.metrics.df, family = poisson())
# summary(glm.primary.piscivores.interaction)

#Poisson Model (mixed linear model)
# glmer.primary.piscivores <-glmer(primary.piscivores.abund ~ primary.piscivores.sst.mean + primary.piscivores.chla.mean +
# primary.piscivores.conn + (1|primary.piscivores.year), data = primary.piscivores.metrics.df, family = "poisson")
# summary(glmer.primary.piscivores)

############
# GAM Models
gam.primary.piscivores.mean = gam(primary.piscivores.abund ~ s(primary.piscivores.sst.mean) + s(primary.piscivores.chla.mean) +
                                      s(primary.piscivores.conn), method = "ML", family = "poisson")

# gam.primary.piscivores.sd = gam(primary.piscivores.abund ~ s(primary.piscivores.sst.sd) + s(primary.piscivores.chla.sd) +
#                                     s(primary.piscivores.conn), method = "ML", family = "poisson")

# gam.primary.piscivores.all = gam(primary.piscivores.abund ~ s(primary.piscivores.sst.mean) + s(primary.piscivores.chla.mean) +
# s(primary.piscivores.sst.sd) + s(primary.piscivores.chla.sd) + s(primary.piscivores.conn), method = "ML", family = "poisson")

gam.primary.piscivores.mean.only = gam(primary.piscivores.abund ~ s(primary.piscivores.sst.mean) + s(primary.piscivores.chla.mean),
                                       method = "ML", family = "poisson")

# gam.primary.piscivores.sd.only = gam(primary.piscivores.abund ~ s(primary.piscivores.sst.sd) + s(primary.piscivores.chla.sd),
# method = "ML", family = "poisson")

# Model Summaries
summary(gam.primary.piscivores.mean)
# summary(gam.primary.piscivores.sd)
# summary(gam.primary.piscivores.all)
summary(gam.primary.piscivores.mean.only)
# summary(gam.primary.piscivores.sd.only)

# AICc Values
AICc(gam.primary.piscivores.mean)
# AICc(gam.primary.piscivores.sd)
# AICc(gam.primary.piscivores.all)
AICc(gam.primary.piscivores.mean.only)
# AICc(gam.primary.piscivores.sd.only)

# AICc Weights
sum.delta.AICc.primary.piscivores <- sum(exp(-0.5 * (362.3596 - 338.7614)) + exp(-0.5 * (367.788 - 338.7614)) +
                                           exp(-0.5 * (338.7614 - 338.7614)) +  exp(-0.5 * (377.5648 - 338.7614)) +
                                           exp(-0.5 * (382.6198 - 338.7614)))

AICc.gam.primary.piscivores.mean <- exp(-0.5 * (362.3596 - 338.7614)) / sum.delta.AICc.primary.piscivores
# AICc.gam.primary.piscivores.sd <- exp(-0.5 * (367.788 - 338.7614)) / sum.delta.AICc.primary.piscivores
# AICc.gam.primary.piscivores.all <- exp(-0.5 * (338.7614 - 338.7614)) / sum.delta.AICc.primary.piscivores
AICcgam.primary.piscivores.mean.only <- exp(-0.5 * (377.5648 - 338.7614)) / sum.delta.AICc.primary.piscivores
# AICc.gam.primary.piscivores.sd.only <- exp(-0.5 * (382.6198 - 338.7614)) / sum.delta.AICc.primary.piscivores

# Estimated Degrees of Freedom
sum(influence(gam.primary.piscivores.mean))
# sum(influence(gam.primary.piscivores.sd))
# sum(influence(gam.primary.piscivores.all))
sum(influence(gam.primary.piscivores.mean.only))
# sum(influence(gam.primary.piscivores.sd.only))

# GAM Check Plots
jpeg(file="gam_check_mean_primary_piscivores.jpg", width=32, height=32, units="cm", res=600)
plot_gam_check(
  gam.primary.piscivores.mean,
  single_page = TRUE,
  type = "deviance",
  scatter = FALSE,
  kcheck = FALSE
)
dev.off()

# jpeg(file="gam_check_sd_primary_piscivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.primary.piscivores.sd,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_all_primary_piscivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.primary.piscivores.all,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_mean_only_primary_piscivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.primary.piscivores.mean.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# jpeg(file="gam_check_sd_only_primary_piscivores.jpg", width=32, height=32, units="cm", res=600)
# plot_gam_check(
#   gam.primary.piscivores.sd.only,
#   single_page = TRUE,
#   type = "deviance",
#   scatter = FALSE,
#   kcheck = FALSE
# )
# dev.off()

# Concurvity
concurvity(gam.primary.piscivores.mean, full = FALSE)
# concurvity(gam.primary.piscivores.sd, full = FALSE)
# concurvity(gam.primary.piscivores.all, full = FALSE)
concurvity(gam.primary.piscivores.mean.only, full = FALSE)
# concurvity(gam.primary.piscivores.sd.only, full = FALSE)

# PLOTTING
jpeg(file="gam_MEANsst_primary_piscivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.primary.piscivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.primary.piscivores.mean)[1], xlab = "Mean sea surface temperature (º C)",
     ylab = "Abundance primary piscivores (count)", select = c(1), shade.col = "lightgrey", trans = exp, ylim = c(0, 30))
dev.off()

jpeg(file="gam_MEANchla_primary_piscivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.primary.piscivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.primary.piscivores.mean)[1], xlab = "Mean chlorophyll a (mg/m3)",
     ylab = "Abundance primary piscivores (count)", select = c(2), shade.col = "lightgrey", trans = exp, ylim = c(0, 30))
dev.off()

jpeg(file="gam_conn_primary_piscivores.jpg",  width=17, height=17, units="cm", res=600)
par(font.lab = 2)
plot(gam.primary.piscivores.mean, seWithMean = TRUE, shade = TRUE,  xaxs = "r", yaxs = "r",
     shift = coef(gam.primary.piscivores.mean)[1], xlab = "Oceanographic connectivity (%)",
     ylab = "Abundance primary piscivores (count)", select = c(3), shade.col = "lightgrey", trans = exp, ylim = c(0, 30))
dev.off()

####################################################################################################################

# ALTERNATIVE TO COMPARE MODELS

# F=\frac{(SSE_R-SSE_F)/(df_R-df_F)}{SSE_F/df_F}
# \]
# 
# where $SSE_R$ is the residual error of the simpler model (with $df_R$ degrees of freedom),
# $SSE_F$ is the residual error of the more complicated model (with $df_R$ degrees of freedom).
#Comparisons are made on $(df_R-df_F), df_F$ degrees of freedom.

# PLOTS WITH GGPLOT

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
# p <- predict(b1, type="lpmatrix")
# beta <- coef(b1)[grepl("x1", names(coef(b1)))]
# s <- p[,grepl("x1", colnames(p))] %*% beta
# ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()
# 
# # predict
# newdf <- gamSim(1,n=n,scale=sig)
# f <- predict(b1, newdata=newdf)
