# This script creates the plots used in the paper and saves them to file.

# Clearing environment:
rm(list = ls())

# Loading libraries and resources:
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
source("simulation.r")

exportFormat = "png" #"png" or "tiff" are supported.




# Figure 1: merit distribution__________________________________________________
figureParameters <- list(
  filename = paste0("./outputGraphics/figure_1.", exportFormat),
  width = 1500,
  height = 800, #600,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
    do.call(tiff, figureParameters)}

par(mfrow=c(1,2))

yl = 2.5
x = seq(from = 0, to = 1, by = 0.001)


plot(
  x, dbeta(x, shape1 = 2, shape2 = 5),
  col = "darkorange", type = "l", frame.plot = FALSE, ylim = c(0,yl), yaxs="i",
  yaxt = "n", xaxp = c(0, 1, 2), ylab = "", xlab = "merit"
)
title(bquote(atop(
  "low merit condition", paste(alpha, " = 2, ", beta, " = 5"))))
title(ylab="density", line=1, cex.lab=1.2)

plot(
  x, dbeta(x, shape1 = 5, shape2 = 2),
  col = "darkorange", type = "l", frame.plot = FALSE, ylim = c(0,yl), yaxs="i",
  yaxt = "n", xaxp = c(0, 1, 2), ylab = "", xlab = "merit"
)

title(bquote(atop(
  "high merit condition", paste(alpha, " = 5, ", beta, " = 2"))))

dev.off()
rm(x, yl)




# Figure 2: Grading languages___________________________________________________
#
#
gl <- list(
  qbeta(1:4 / 5, shape1 = 2, shape2 = 1), # strict
  c(0.2, 0.4, 0.6, 0.8) # regular
)

d <- data.frame(c(
  "strict",
  "regular"
))
names(d) <- "id"
d <- cbind(d, as.data.frame(matrix(rbind(gl[[1]], gl[[2]]), nrow=2)))
d$init <- c(0,0)
d$fin <- c(1,1)

d$id <- as.factor(d$id)
cl = "gray40"
pointer=")["
labz = c(
  " very bad"," average"," good"," very good"," outstanding")
ps = 3#30
ls = 2.8
padd = 0.02

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_2.", exportFormat),
  width = 800,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  d,
  aes(x=id)
) +
  geom_rect(aes(ymin=V1-padd, ymax=V1+padd, xmin=id, xmax=id), col="white") +
  geom_text(aes(y=V1, x=id, angle=90,label=pointer), size=ps) +
  geom_rect(aes(ymin=V2-padd, ymax=V2+padd, xmin=id, xmax=id), col="white") +
  geom_text(aes(y=V2, x=id, angle=90, label=pointer), size=ps) +
  geom_rect(aes(ymin=V3-padd, ymax=V3+padd, xmin=id, xmax=id), col="white") +
  geom_text(aes(y=V3, x=id, angle=90, label=pointer), size=ps) +
  geom_rect(aes(ymin=V4-padd, ymax=V4+padd, xmin=id, xmax=id), col="white") +
  geom_text(aes(y=V4, x=id, angle=90, label=pointer), size=ps) +
  
  geom_rect(aes(ymin=init-0.01, ymax=init+0.01, xmin=id, xmax=id), col="white")+
  geom_text(aes(y=init, x=id, angle=90, label="["), size=ps) +
  geom_rect(aes(ymin=fin-0.01, ymax=fin+0.01, xmin=id, xmax=id), col="white")+
  geom_text(aes(y=fin, x=id, angle=90, label="]"), size=ps) +
  
  geom_text(aes(y=V1/2, x=id, label=labz[1], hjust = 0),
            color=cl,check_overlap=TRUE, size=ls) +
  geom_text(aes(y=(V2-V1)/2+V1, x=id, label=labz[2], hjust = 0),
            color=cl,check_overlap=TRUE, size=ls) +
  geom_text(aes(y=(V3-V2)/2+V2, x=id, label=labz[3], hjust = 0),
            color=cl,check_overlap=TRUE, size=ls) +
  geom_text(aes(y=(V4-V3)/2+V3, x=id, label=labz[4], hjust = 0),
            color=cl,check_overlap=TRUE, size=ls) +
  geom_text(aes(y=(1-V4)/2+V4, x=id, label=labz[5], hjust = 0),
            color=cl,check_overlap=TRUE, size=ls) +
  ylab("\nmerit") +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:5/5)) +
  scale_x_discrete(expand = expansion(mult = c(0.3, 0.9)), position = "top") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "darkorange1"),#black
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank()
  )

dev.off()
rm(asymm, gl, cl, labz, ls, padd, pointer, ps, t, d)




# Figure 3: Grading languages___________________________________________________
#
#
granul <- c(2, 5, 10)#c(2,5,10,20)
for (gr in 1:length(granul)) {
  scale = granul[[gr]]
  th <- data.frame(
    granularity = granul[[gr]],
    th = qbeta(1:(scale - 1) / scale, shape1 = 2, shape2 = 1)
  )
  ifelse(gr == 1, d <- th, d <- rbind(d, th))
}
granul <- factor(granul, levels = c("5", "2", "10")) # reordering factor levels


th <- ggplot(data = d, aes(x = th, y = as.factor(granularity))) +
  geom_segment(
    data = data.frame(
      x = rep(0, times = length(granul)),
      xend = rep(1, times = length(granul)), y = granul, yend = granul
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "darkorange1"#alpha("darkorange1", 0.5) 
  ) +
  geom_point(color = "white", shape = 15, size = 2) +# masks the orange line
  geom_point( # masks the line at the lowest and highest limit of the scale
    data = data.frame(
      granularity = c(granul, granul),
      th = c(rep(0, times = length(granul)), rep(1, times = length(granul)))
    ),
    color = "white", shape = 15, size = 1
    ) +
  geom_text(data = d, aes(label = ")["), size = 3, color = "black") +
  geom_text( # opening square brackets
    data = data.frame(
      granularity = granul, th = rep(0, times = length(granul))
    ), 
    aes(label = "["), size = 3, color = "black"
  ) +
  geom_text( # closing square brackets
    data = data.frame(
      granularity = granul, th = rep(1, times = length(granul))
    ), 
    aes(label = "]"), size = 3, color = "black"
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0.004,0), breaks = 0:5/5) +
  scale_y_discrete(
    limits = rev(levels(granul)),
    labels = c("L=10", "L=2", "L=5")
  ) +
  labs(
    x = "merit", y = ""
  ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(size = 11, color = "black")
  )


quantiles <- data.frame(
  x = qbeta(1:4 / 5, shape1 = 2, shape2 = 1),
  xend = qbeta(1:4 / 5, shape1 = 2, shape2 = 1),
  y = c(0, 0, 0, 0),
  yend = dbeta(qbeta(1:4 / 5, shape1 = 2, shape2 = 1), shape1 = 2, shape2 = 1)
)
betapdf <- data.frame(
  x = 1:100/100,
  y = dbeta(1:100/100, shape1 = 2, shape2 = 1))

pdf <- ggplot() + # probability density function
  geom_line(data = betapdf, aes(x = x, y = y), linetype = "dashed") +
  geom_area(
    data = betapdf, aes(x = x, y = y), fill = alpha("black", 0.08)
  ) +
  geom_segment(
    data = quantiles,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black"
  ) +
  geom_text(aes(
    x = qbeta(1:4 / 5, shape1 = 2, shape2 = 1) - 0.03,
    y = rep(0.45, times = 4),#quantiles$yend / 2,
    label =
      c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile")
  ), size = 3, angle = 90, color = "black"
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0.004,0), breaks = 0:5/5) +
  scale_y_continuous(expand = c(0, 0)) +
  labs (y = "L=5\ndensity function") +#, x = "merit") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "darkorange1"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank() 
  )

figure3 <- ggarrange(
  pdf, th,
  labels = c("A", "B"),
  ncol = 1,
  heights = c(1.2,1),
  align = "v"
)
#plot(figure3)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_3.", exportFormat),
  width = 1000,
  height = 1000,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

plot(figure3)

dev.off()




#_______________________________________________________________________________
# 
# Importing simulation data
#_______________________________________________________________________________

# Loading the results data file:
# (can take a few seconds)
load(file = "./output/ri.RData")

colorScheme = "A" # we'll use this palette from Viridis

ri$aggrRule <- factor(ri$aggrRule, levels = rev(levels(ri$aggrRule)))
ri$baseline <-
  ri$tqd == "top skewed" &
  ri$scale == 5 &
  ri$glh == 0.05 &
  ri$truthNoise == 0 &
  ri$nReviewersPerProp == 5 &
  ri$competence == 0.8 &
  ri$ruleVariant == "none" &
  ri$aggrRule == "mean" &
  ri$discreteMerit == FALSE

ri$commonUnderstGrades <- 1 - ri$glh




# Figure 4 _____________________________________________________________________
# Baseline: mean vs control
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)
rii$aggrRule <- as.character(rii$aggrRule)
rii$aggrRule[rii$aggrRule == "mean"] <- "baseline"
rii$aggrRule <- as.factor(rii$aggrRule)
#rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
df <- rii[,c("aggrRule", "CohensKappa20")] #"qualityEff", "kts", "KTC"


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_4.", exportFormat),
  width = 920,
  height = 850,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = aggrRule)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray80", color = "gray75", scale = "width") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(limits = c("control", "baseline")) +
  scale_fill_manual(values = c("darkorange", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color=NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line = element_line(),
    axis.title.x = element_blank(),
    legend.position = "NA",
  )
dev.off()




# Figure 5 _____________________________________________________________________
# Varying panel size
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 &
    #ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)
#rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
rii$nReviewersPerProp <- factor(rii$nReviewersPerProp)
#rii$nReviewersPerProp <- factor(
#  rii$nReviewersPerProp,
#  levels = c("1", as.character(unique(rii$nReviewersPerProp)))
#)
df <- rii[,c("aggrRule", "baseline", "nReviewersPerProp", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)
control <- subset(df, df$aggrRule == "control" & df$nReviewersPerProp == 5)
control$nReviewersPerProp <- "1" # so that the control boxplot is shown first
df <- subset(df, df$aggrRule == "mean")


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_5.", exportFormat),
  width = 1320,
  height = 850,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = nReviewersPerProp, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(
    data = rbind(df, control),
    fill = "gray70", color = "gray60", scale = "width"
  ) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  geom_boxplot( # control condition
    data = control, color = "black", alpha = 0.9, width = 0.3
  ) +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_discrete(limits = as.character(1:12)) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    x = "panel size", y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    #panel.border = element_rect(fill="transparent", color="gray50"),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line = element_line(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "NA",
    #legend.background = element_rect(fill = "transparent",color=NA),
    #legend.box.background = element_rect(fill = "transparent",color=NA),
    #text = element_text(size = 15)
  )
dev.off()




# Figure 6 _____________________________________________________________________
# Granularity of the grading scale
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    #ri$scale == 5 &
    ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 & ########
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$scale <- sapply(rii$scale, FUN = function(x){paste0("L=", as.character(x))})
rii$scale <- factor(
  rii$scale,
  levels = c("L=2", "L=3", "L=4", "L=5", "L=7", "L=10")
)
df <- rii[,c("aggrRule", "baseline", "scale", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_6.", exportFormat),
  width = 1520,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #geom_boxplot( # control condition
  #  data = control, color = "black", alpha = 0.9, width = 0.3
  #) +
  facet_grid(cols = vars(scale), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()




# Figure 7 _____________________________________________________________________
# Aggregation rule
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    #ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

df <- rii[,c("aggrRule", "baseline", "CohensKappa20")]
df$baseline[df$aggrRule == "control"] <- 2
df$baseline <- as.factor(df$baseline)
df$boost <- "discrim. boosting" # Boosting vs non-boosting
nonBoostingRules <- c("control", "median", "lowest score", "highest score")
df$boost[df$aggrRule %in% nonBoostingRules] <- "non discrim. boosting"
df$boost <- factor(
  df$boost, levels = c("non discrim. boosting", "discrim. boosting"))
df$aggrRule <- factor(
  df$aggrRule,
  levels = c(
    "control", "median", "lowest score", "highest score",
    "mean", "trimmed mean", "hypermean", "gloomy mean", "sunny mean",
    "Borda count", "majority judgment"
  )
)

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_7.", exportFormat),
  width = 1320,
  height = 900,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = baseline)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  facet_grid(
    cols = vars(boost),
    scales = "free_x",
    drop = TRUE,
    space = "free"#, switch = "x"
  ) +
  scale_y_continuous(expand = c(0.015,0)) +
  scale_x_discrete(position = "bottom") +#"top") +
  scale_fill_manual(values = c("white", "darkorange", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.line = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "NA",
  )
dev.off()




# Figure 8 _____________________________________________________________________
# Diversity in interpretating the grading scale
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    #ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$commonUnderstGrades <- sapply(
  rii$commonUnderstGrades,
  FUN = function(x){paste0("U=", as.character(x))}
)
rii$commonUnderstGrades <- as.factor(rii$commonUnderstGrades)
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "commonUnderstGrades", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_8.", exportFormat),
  width = 1220,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #geom_boxplot( # control condition
  #  data = control, color = "black", alpha = 0.9, width = 0.3
  #) +
  facet_grid(cols = vars(commonUnderstGrades), scales = "free_x", switch = "x")+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()








# Robustness ___________________________________________________________________
#
# Reviewer competence
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    #ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$competence <- sapply(
  rii$competence,
  FUN = function(x){paste0("C=", as.character(x))}
)
rii$competence <- as.factor(rii$competence)
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "competence", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_competence.", exportFormat),
  width = 1420,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #geom_boxplot( # control condition
  #  data = control, color = "black", alpha = 0.9, width = 0.3
  #) +
  facet_grid(cols = vars(competence), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()

#
# True merit distribution
rii <- subset(
  ri,
  #ri$tqd == "top skewed" &
    ri$scale == 5 & 
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$tqd <- factor(rii$tqd, levels = unique(rii$tqd))
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "tqd", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_tqd.", exportFormat),
  width = 1220,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #geom_boxplot( # control condition
  #  data = control, color = "black", alpha = 0.9, width = 0.3
  #) +
  facet_grid(cols = vars(tqd), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()


#
# Truth noise
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.05 &
    #ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$truthNoise <- sapply(
  rii$truthNoise,
  FUN = function(x){paste0("truth noise = ", as.character(x))}
)
rii$truthNoise <- as.factor(rii$truthNoise)
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "truthNoise", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_truthNoise.", exportFormat),
  width = 1220,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #geom_boxplot( # control condition
  #  data = control, color = "black", alpha = 0.9, width = 0.3
  #) +
  facet_grid(cols = vars(truthNoise), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()


#
# Funding rate (k)
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$tqd <- factor(rii$tqd, levels = unique(rii$tqd))
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c(
  "aggrRule", "baseline",
  "CohensKappa5", "CohensKappa10", "CohensKappa20", "CohensKappa50"
)]

df <- reshape2::melt(df, id.vars = c("aggrRule", "baseline"))
df$variable <- as.character(df$variable)
df$variable[df$variable == "CohensKappa5"] <- "k=5"
df$variable[df$variable == "CohensKappa10"] <- "k=10"
df$variable[df$variable == "CohensKappa20"] <- "k=20"
df$variable[df$variable == "CohensKappa50"] <- "k=50"
df$variable <- factor(df$variable, levels = c(
  "k=5", "k=10", "k=20", "k=50"
))

df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE | df$variable != "k=20"] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_K.", exportFormat),
  width = 1420,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = value, x = aggrRule, fill = condition)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  facet_grid(cols = vars(variable), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "choice performance (all levels of k)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line.y = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),#element_text(angle = 30, hjust = 0),
    axis.ticks.x = element_blank(),
    legend.position = "NA",
  )
dev.off()



#### Extra #####################################################################
# Control vs baseline vs language enhancement
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)
riiEnhanced <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 10 &
    ri$glh == 0 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule == "majority judgment" &
    ri$discreteMerit == FALSE
)
#ggplot(riiEnhanced, aes(x = aggrRule,y = CohensKappa20)) + geom_boxplot()

rii <- rbind(rii, riiEnhanced)
rii$aggrRule <- as.character(rii$aggrRule)
rii$aggrRule[rii$aggrRule == "mean"] <- "baseline"
rii$aggrRule[rii$aggrRule == "majority judgment"]<-"grading scale\nenhancements"
rii$aggrRule <- as.factor(rii$aggrRule)
#rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
df <- rii[,c("aggrRule", "CohensKappa20")] #"qualityEff", "kts", "KTC"


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_4extra.", exportFormat),
  width = 1120,
  height = 850,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = CohensKappa20, x = aggrRule, fill = aggrRule)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray80", color = "gray75", scale = "width") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(
    limits = c("control", "baseline","grading scale\nenhancements")) +
  scale_fill_manual(values = c("darkorange", "gray30", "white")) +
  labs(
    #title = "choice performance (k=20)",
    y = "choice performance\n(Cohen's kappa, k=20)"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "gray96"),
    plot.background = element_rect(fill = "transparent", color=NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.line = element_line(),
    axis.title.x = element_blank(),
    legend.position = "NA",
  )
dev.off()





























################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Old figures:





# Figure 3: Baseline (1/2)______________________________________________________
#
#
# Selecting the variables we need from the baseline runs:
# Subsetting the runs from the baseline parameter configuration:
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.1 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
df <- rii[,c("aggrRule", "Spearman")] #"qualityEff", "kts", "KTC"

###
df1 <- df[df$aggrRule %in% c("mean", "null"),]
df1$panel <- 1
df2 <- df
df2$panel <- 2
df <- rbind(df1, df2)
###

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_3.", exportFormat),
  width = 1100,
  height = 1100,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = Spearman, x = aggrRule, fill = aggrRule)) +
  geom_violin(fill = "gray70", color = "gray60") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
  geom_boxplot( # overplotting the control condition
    data = df[df$aggrRule == "null",],
    fill = "gray30", color = "black", width = 0.6) +
  #facet_wrap("panel", nrow = 1, scales = "free_x") + ###################
  facet_grid(cols = vars(panel), scales = "free_x", space = "free") + ###
  ggtitle(
    "ranking performance",
    subtitle = expression(paste("(Spearman's ", rho, ")"))
  ) +
  scale_x_discrete(position = "bottom") +
  scale_y_continuous(
    #limits = c(0.4,0.9),
    #breaks = seq(from = 0, to = 1, by = 0.25),
    expand = c(0,0)
  ) +
  scale_fill_viridis(
    begin = 0, discrete = TRUE, option = colorScheme, direction = -1) +
  #coord_flip() +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(), ######
    strip.text.x = element_blank(),     ######
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "NA",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )
dev.off()




# Figure 4: Baseline (2/2)______________________________________________________
#
#
# Selecting the variables we need from the baseline runs:
rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
dft <- rii[,c(
  "aggrRule",
  "CohensKappa10", "CohensKappa20", "CohensKappa50"
  #"RankEff10", "RankEff20", "RankEff50"#,
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = "aggrRule")

#dft$aggrRule <- as.factor(dft$aggrRule)
dft$variable <- as.character(dft$variable)
#dft$variable[dft$variable == "RankEff10"] <- "k=10"
#dft$variable[dft$variable == "RankEff20"] <- "k=20"
#dft$variable[dft$variable == "RankEff50"] <- "k=50"
dft$variable[dft$variable == "CohensKappa10"] <- "k=10"
dft$variable[dft$variable == "CohensKappa20"] <- "k=20"
dft$variable[dft$variable == "CohensKappa50"] <- "k=50"
#dft$aggrRule <- factor(
#  dft$aggrRule,
#  levels = rev(levels(dft$aggrRule))
#)

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_4.", exportFormat),
  width = 1600,
  height = 1100,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(dft, aes(y = value, x = aggrRule, fill = aggrRule)) +
  geom_violin(fill = "gray70", color = "gray60") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
  geom_boxplot( # overplotting the control condition
    data = dft[dft$aggrRule == "null",],
    fill = "gray30", color = "black", width = 0.6) +
  ggtitle(
    "choice performance",
    subtitle = "(Cohen's Kappa)") +
  scale_x_discrete(
    position = "bottom", #top
    limits = rev(levels(dft$aggrRule))) +
  scale_y_continuous(
    #limits = c(0,1),
    #breaks = seq(from = 0, to = 1, by = 0.2),
    expand = c(0,0)
  ) +
  #facet_wrap("variable", ncol = 1, strip.position = "left") +
  facet_wrap("variable", nrow = 1) +#, strip.position = "left") +
  #facet_grid(row="variable") +
  scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
  #coord_flip() +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.left = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "NA",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )
dev.off()



# Figure 5 _____________________________________________________________________
# Panel size by reviewer error (epsilon)
#
# 
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.1 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
dft <- rii[,c(
  "aggrRule", "nReviewersPerProp", "competence", 
  "Spearman", "CohensKappa20"#"RankEff20","RankEff30", RankEff40","RankEff50",
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = c("aggrRule", "nReviewersPerProp", "competence"))

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_5.", exportFormat),
  width = 1700,
  height = 2500,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  dft,
  aes(
    x = as.factor(nReviewersPerProp),
    y = as.factor(competence),
    fill = value
  )
) +
  geom_tile(size = 1) +
  facet_grid(
    aggrRule ~ variable,
    labeller = labeller(
      variable = as_labeller(c(
        "Spearman" = "ranking\nperformance",
        "CohensKappa20" = "choice performance\n(Cohen's Kappa, k=20)"
      ))
    )
  ) +
  scale_fill_viridis(discrete = FALSE, option = colorScheme) +
  scale_x_discrete(
    expand = c(0, 0),
    breaks = unique(dft$nReviewersPerProp)[c(TRUE, FALSE)]) +
  #scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +#, breaks = unique(rii$competence)) +
  xlab("panel size (N)") +
  #ylab(expression(paste("reviewer error (", epsilon, ")"))) +
  ylab("average reviewer competence (C)") +
  labs(fill = "performance     \n") +
  guides(fill = guide_colorbar(barheight = 0.5)) +
  theme(
    panel.background = element_rect(fill="transparent"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.right = element_text(angle = 0, hjust = 0),
    axis.line = element_blank(),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15),
    legend.text = element_text(size = 10, angle = 35, vjust = 1.5, hjust = 1)
  )
dev.off()



# Figure 6 _____________________________________________________________________
# grade language granularity (L) (top-skewed merit)
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    #ri$scale == 5 &
    ri$glh == 0.1 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
dft <- rii[,c(
  "aggrRule", "scale",
  "Spearman", "CohensKappa20"#,"RankEff20","RankEff30","RankEff40", RankEff50",
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = c("aggrRule", "scale"))

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_6.", exportFormat),
  width = 1650,
  height = 2000,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(dft, aes(y = value, x = aggrRule, fill = aggrRule)) +
  geom_violin(fill = "gray70", color = "gray60") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
  geom_boxplot( # overplotting the control condition
    data = dft[dft$aggrRule == "null",],
    fill = "gray30", color = "black", width = 0.6) +
  ggtitle( "high merit condition") +
  scale_x_discrete(
    position = "bottom", #top
    limits = rev(levels(dft$aggrRule))) +
  scale_y_continuous(
    #limits = c(0,1),
    #breaks = seq(from = 0, to = 1, by = 0.2),
    expand = c(0,0)
  ) +
  facet_grid(
    scale ~ variable,
    labeller = labeller(
      scale = as_labeller(c(
        "2" = "L=2", "5" = "L=5", "10" = "L=10", "20" = "L=20"
      )),
      variable = as_labeller(c(
        "Spearman" = "\nranking performance",
        "CohensKappa20" = "choice performance\n(Cohen's Kappa, k=20)"
      ))
    )
  ) +
  scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.right = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "NA",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )
dev.off()



# Figure 7 _____________________________________________________________________
# grade language granularity (L) (bottom-skewed merit)
rii <- subset(
  ri,
  ri$tqd == "bottom skewed" &
    #ri$scale == 5 &
    ri$glh == 0.1 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
dft <- rii[,c(
  "aggrRule", "scale",
  "Spearman", "CohensKappa20"#"RankEff20","RankEff30","RankEff40","RankEff50",
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = c("aggrRule", "scale"))

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_7.", exportFormat),
  width = 1650,
  height = 2000,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(dft, aes(y = value, x = aggrRule, fill = aggrRule)) +
  geom_violin(fill = "gray70", color = "gray60") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
  geom_boxplot( # overplotting the control condition
    data = dft[dft$aggrRule == "null",],
    fill = "gray30", color = "black", width = 0.6) +
  ggtitle( "low merit condition") +
  scale_x_discrete(
    position = "bottom", #top
    limits = rev(levels(dft$aggrRule))) +
  scale_y_continuous(
    #limits = c(0,1),
    #breaks = seq(from = 0, to = 1, by = 0.2),
    expand = c(0,0)
  ) +
  #facet_wrap("variable", ncol = 1, strip.position = "left") +
  #facet_wrap("variable", ncol = 2) +#, strip.position = "left") +
  #facet_grid(rows = "scale", cols = "variable") +
  facet_grid(
    scale ~ variable,
    labeller = labeller(
      scale = as_labeller(c(
        "2" = "L=2", "5" = "L=5", "10" = "L=10", "20" = "L=20"
      )),
      variable = as_labeller(c(
        "Spearman" = "\nranking performance",
        "CohensKappa20" = "choice performance\n(Cohen's Kappa, k=20)"
      ))
    )
  ) +
  scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
  #coord_flip() +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    #strip.text = element_text(angle = 45, hjust = 0, vjust = 0),
    strip.text.y.right = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "NA",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )
dev.off()







# Figure 8 _____________________________________________________________________
# grade language heterogeneity (h) and competence (c)
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    #ri$glh == 0.1 &
    ri$nReviewersPerProp == 5 &
    #ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
dft <- rii[,c(
  "aggrRule", "glh", "competence", 
  "Spearman", "CohensKappa20"#"RankEff20","RankEff30","RankEff40","RankEff50",
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = c("aggrRule", "glh", "competence"))

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_8.", exportFormat),
  width = 1700,
  height = 2200,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  dft,
  aes(
    x = as.factor(glh),
    y = as.factor(competence),
    fill = value
  )
) +
  geom_tile(size = 1) +
  facet_grid(
    aggrRule ~ variable,
    labeller = labeller(
      variable = as_labeller(c(
        "Spearman" = "ranking\nperformance",
        "CohensKappa20" = "choice performance\n(Cohen's Kappa, k=20)"
      ))
    )
  ) +
  scale_fill_viridis(discrete = FALSE, option = colorScheme) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +#, breaks = unique(rii$competence)) +
  xlab("differences in understanding\nthe grading language (D)") +
  #ylab(expression(paste("reviewer error (", epsilon, ")"))) +
  ylab("average reviewer competence (C)") +
  labs(fill = "performance     \n") +
  guides(fill = guide_colorbar(barheight = 0.5)) +
  theme(
    panel.background = element_rect(fill="transparent"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.right = element_text(angle = 0, hjust = 0),
    axis.line = element_blank(),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15),
    legend.text = element_text(size = 10, angle = 35, vjust = 1.5, hjust = 1)
  )
dev.off()




#_______________________________________________________________________________
# 
# Appendices
#_______________________________________________________________________________
#
#
#
# Figure A1: thresholds_________________________________________________________
#
#
# Note that this plot is based on survey data. Survey data cannot be
# distributed and is thus not included in this repository.
# Loading survey data:
surveyDir <- "./data/survey.RData"
ifelse(
  file.exists(surveyDir),
  load(surveyDir),
  warning("figure_A1 could not be plotted because survey data were not found.")
)

# and plotting it:
if(file.exists(surveyDir)) {
  figureParameters <- list(
    filename = paste0("./outputGraphics/figure_A1.", exportFormat),
    width = 1600,
    height = 900,
    units = "px",
    res = 300
  )
  if(exportFormat == "png") {do.call(png, figureParameters)} else {
    do.call(tiff, figureParameters)}
  
  par(mfrow=c(1,2))
  
  hist(
    s$i$q34,
    xlim = c(0,100), ylim = c(0,140),
    yaxs="i", col = "darkorange1", cex.main = 0.8, border = FALSE,
    xaxp = c(0, 100, 2), ylab = "frequency", xlab = "",
    main = "first threshold"
  )
  
  hist(
    s$i$q33,
    xlim = c(0,100), ylim = c(0,140),
    yaxs="i", col = "darkorange1", cex.main = 0.8, border = FALSE,
    xaxp = c(0, 100, 2), ylab = "", xlab = "",
    main = "fourth threshold"
  )
  dev.off()
  
}


rm(s)




# Figure B1____________________________________________________________
#
# Alternative measures.
rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$scale == 5 &
    ri$glh == 0.1 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))

rii$peerReviewErr <- 1 - rii$qualityEff
df <- 
  rii[,c("aggrRule", "peerReviewErr", "kts", "KTC", "Spearman", "RankEff20")]
df <- melt(df, id.vars = "aggrRule")

# Recoding for plotting:
df$variable <- as.character(df$variable)
df$variable[df$variable == "peerReviewErr"] <- "peer review error\n(normalized)"
df$variable[df$variable == "kts"] <- "\nrank similarity"
df$variable[df$variable == "KTC"] <- "ranking performance\n(Kendall)"
df$variable[df$variable == "Spearman"] <- "ranking performance\n(Spearman)"
  #expression(paste("ranking performance\n(Spearman's ",rho, ")"))
df$variable[df$variable == "RankEff20"] <- "choice performance\n(k=20)"
df$variable <- factor(
  df$variable,
  levels = c(
    #expression(paste("ranking performance\n(Spearman's ",rho, ")")),
    "ranking performance\n(Spearman)",
    "choice performance\n(k=20)",
    "ranking performance\n(Kendall)",
    "\nrank similarity",
    "peer review error\n(normalized)"
  )
)

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_B1.", exportFormat),
  width = 1400,
  height = 2200,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(y = value, x = aggrRule, fill = aggrRule)) +
  geom_violin(fill = "gray70", color = "gray60") +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
  geom_boxplot( # overplotting the control condition
    data = df[df$aggrRule == "null",],
    fill = "gray30", color = "black", width = 0.6) +
  facet_wrap("variable", ncol = 2) +
  scale_y_continuous(
    #limits = c(0,1),
    expand = c(0,0)
  ) +
  scale_fill_viridis(
    begin = 0.2, discrete = TRUE, option = colorScheme, direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.left = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "NA",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )
dev.off()







# Extra checks _________________________________________________________________
#
#
# Here we vary the number of proposals and the variability in reviewer
# error/competence. These results are not reported in the paper, but can be
# replicated by executing this script.

if(FALSE){ ###############

# Clearing environment and loading resources:
rm(list = ls())
library(ggplot2)
library(reshape2)
library(viridis)
source("simulation.r")

# We define a function that runs 200 repetitions with the baseline parameter
# configuration, and where the number of proposals and the variability in
# reviewer error/competence can be arbitrarily changed:
smallBattery <- function(nSubmissions = 100, sigma = 0.2) {
  
  tqd <- c ("top skew")
  scale <- c(5)
  glh <- c(0.1)#0:4/20#0:8/40
  #nSubmissions <- c(100)
  nReviewersPerProp <- 5
  reviewerCompetence <- c(0.8)
  battery <- expand.grid(
    tqd = tqd,
    scale = scale,
    glh = glh,
    nSubmissions = nSubmissions,
    nReviewersPerProp = nReviewersPerProp,
    reviewerError = 1 - reviewerCompetence
  )
  
  # Then, the parameters that vary within the simulation:
  aggrRule = c(
    "control",
    "mean",
    "hypermean",
    "lowestScore",
    "median",
    "majorityJudgement",
    "bordaCount")
  nAccepted <- c(10,20,30,40,50)
  nAccepted <- nAccepted[nAccepted < nSubmissions]
  
  # Finally, we set some global parameters and initialize the object where
  # results will be stored.
  
  # This is the number of indipendent repetitions per condition:
  nRepetitions <- 200
  runCounter <- 1
  
  # We create a unique random seed for each simulation run we're going to run:
  randomSeeds <- sample(
    -999999999:999999999,
    size = nrow(battery) * nRepetitions,
    replace = FALSE
  )
  
  # Last bit of preparation work: we prepare the data.structure where we will be
  # saving the results from all the simulations.
  temp = data.frame(seed = rep(NA, nrow(battery) * nRepetitions))
  ri <- list(
    "control" = temp,
    "mean" = temp,
    "hypermean" = temp,
    "lowestScore" = temp,
    "median" = temp,
    "majorityJudgement" = temp,
    "bordaCount" = temp
  )
  rm(temp)
  
  
  
  # Here we run the actual simulation battery (it can take a long time):
  for (b in 1:nrow(battery)) { # For all parameter combinations...
    print(paste0(
      "Running parameter combination ", b,
      " of ", nrow(battery), ". Time: ", Sys.time() 
    ))
    
    for (rep in 1:nRepetitions){ # and for as many times as we need repetitions:
      
      alpha <- beta <- 1
      if (battery$tqd[b] == "symmetric bell") {
        alpha <- beta <- 3
      }
      if (battery$tqd[b] == "top skew") {
        alpha <- 5
        beta <- 2
      }
      if (battery$tqd[b] == "bottom skew") {
        alpha <- 2
        beta <- 5
      }
      
      # .. we run the simulation according to the specified parameters:
      r <- simulation (
        criteria = cbind.data.frame(
          name    = c("q1"),
          alpha   = alpha,
          beta    = beta,
          scale   = battery$scale[b],
          gradeLanguage = c("asymmetric"),
          glh     = battery$glh[b],
          weights = c(1)
        ), 
        nSubmissions = battery$nSubmissions[b],
        nReviewersPerProp = battery$nReviewersPerProp[b],
        nPropPerReviewer = battery$nSubmissions[b],# complete review network
        reviewerError = battery$reviewerError[b],
        reviewerVariability = sigma,
        aggrRule = aggrRule,
        nAccepted = nAccepted,
        seed = randomSeeds[runCounter]
      )
      
      # Saving the results to the results list.
      for (rule in aggrRule) {
        
        # We save the independent variables:
        df <- data.frame(
          seed = r$parameters$seed,
          timestamp = r$parameters$timestamp,
          aggrRule = rule,
          alpha = r$parameters$criteria$alpha,
          beta = r$parameters$criteria$beta,
          scale = r$parameters$criteria$scale,
          gradeLanguage = r$parameters$criteria$gradeLanguage,
          glh = r$parameters$criteria$glh,
          nSubmissions = r$parameters$nSubmissions,
          nReviewersPerProp = r$parameters$nReviewersPerProp,
          nPropPerReviewer = r$parameters$nPropPerReviewer,
          reviewerError = r$parameters$reviewerError
        )
        
        # And then we add the outcome variables:
        results <- r$results[[rule]]$outcomeMetrics
        
        df$qualityEff <- results$qualityEfficacy 
        
        for (n in 1:length(r$parameters$nAccepted)){
          df[1,paste0("RankEff",r$parameters$nAccepted[n])] <- 
            results$rankingEfficacy[n]
          
          df[1,paste0("KTDtop",r$parameters$nAccepted[n])] <- 
            results$ktdTop[n]
          
          df[1,paste0("spearmanTop",r$parameters$nAccepted[n])] <- 
            results$spearmanTop[n]
        }
        df$KTD <- results$ktd
        df$KTC <- results$ktc
        df$Spearman <- results$spearman
        
        # Last, we append df to the results:
        ifelse(
          runCounter == 1,
          {
            df[(nrow(df) + 1):(nrow(battery) * nRepetitions),] <- NA
            ri[[rule]] <- df
          },
          ri[[rule]][runCounter,] <- df
        )
      }
      
      runCounter <- runCounter + 1
    }
  }
  
  
  # Merging data into one data.frame____________________________________________
  for (d in 1:length(ri)){
    ifelse(d == 1, temp <- ri[[d]], temp <- rbind(temp, ri[[d]]))
  }
  ri <- temp
  rm(temp)
  
  # we transform the normalized Kendall distance into similarity
  ri$kts <- 1 - ri$KTD
  ri$KTD <- NULL
  
  # Recoding some of the variables:
  ri$aggrRule[ri$aggrRule == "bordaCount"] <- "Borda count"
  #ri$aggrRule[ri$aggrRule == "bordaCountExtended"] <- "Borda count ext."
  ri$aggrRule[ri$aggrRule == "excludeExtremes"] <- "mean excl.extremes"
  ri$aggrRule[ri$aggrRule == "lowestScore"] <- "lowest score"
  ri$aggrRule[ri$aggrRule == "majorityJudgement"] <- "majority judgment"
  ri$aggrRule[ri$aggrRule == "control"] <- "null"
  ri$aggrRule[is.na(ri$aggrRule)] <- "null"
  #ri$aggrRule[ri$aggrRule == "null-panel"] <- "null"
  
  ri$tqd <- "uniform"
  ri$tqd[ri$alpha == 2 & ri$beta == 5] <- "bottom skewed"
  ri$tqd[ri$alpha == 5 & ri$beta == 2] <- "top skewed"
  ri$alpha <- ri$beta <- NULL
  ri$competence <- 1 - ri$reviewerError
  
  return(ri)
}

# Functions to plot ranking and choice performance:
plotRankPerf <- function(ri){
  
  ri$aggrRule <- factor(
    ri$aggrRule,
    levels = rev(c(
      "median", "mean", "hypermean",
      "majority judgment","lowest score", "Borda count", "null"))
  )
  colorScheme = "A" # we'll use this palette from Viridis
  
  rii <- subset(
    ri,
    ri$tqd == "top skewed" &
      ri$scale == 5 &
      ri$glh == 0.1 &
      ri$nReviewersPerProp == 5 &
      ri$competence == 0.8
  )
  rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
  df <- rii[,c("aggrRule", "Spearman")] #"qualityEff", "kts", "KTC"
  
  df1 <- df[df$aggrRule %in% c("mean", "null"),]
  df1$panel <- 1
  df2 <- df
  df2$panel <- 2
  df <- rbind(df1, df2)

  ggplot(df, aes(y = Spearman, x = aggrRule, fill = aggrRule)) +
    geom_violin(fill = "gray70", color = "gray60") +
    geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
    geom_boxplot( # overplotting the control condition
      data = df[df$aggrRule == "null",],
      fill = "gray30", color = "black", width = 0.6) +
    #facet_wrap("panel", nrow = 1, scales = "free_x") + ###################
  facet_grid(cols = vars(panel), scales = "free_x", space = "free") + ###
    ggtitle(
      "ranking performance",
      subtitle = expression(paste("(Spearman's ", rho, ")"))
    ) +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(
      limits = c(0,1),
      #breaks = seq(from = 0, to = 1, by = 0.25),
      expand = c(0,0)
    ) +
    scale_fill_viridis(
      begin = 0, discrete = TRUE, option = colorScheme, direction = -1) +
    #coord_flip() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=12),
      panel.background = element_rect(fill="gray96"),
      plot.background = element_rect(fill="transparent", color=NA),
      panel.border = element_rect(fill="transparent", color="gray50"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(), ######
      strip.text.x = element_blank(),     ######
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "NA",
      legend.background = element_rect(fill = "transparent",color=NA),
      legend.box.background = element_rect(fill = "transparent",color=NA),
      text = element_text(size = 15)
    )
}
plotChoicePerf <- function(ri){
  ri$aggrRule <- factor(
    ri$aggrRule,
    levels = rev(c(
      "median", "mean", "hypermean",
      "majority judgment","lowest score", "Borda count", "null"))
  )
  colorScheme = "A" # we'll use this palette from Viridis
  
  rii <- subset(
    ri,
    ri$tqd == "top skewed" &
      ri$scale == 5 &
      ri$glh == 0.1 &
      ri$nReviewersPerProp == 5 &
      ri$competence == 0.8
  )
  #rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
  
  vars <- c("aggrRule", "RankEff10", "RankEff20", "RankEff50")
  ifelse(
    ri$nSubmissions[1] < 11,
    warning("nSubmissions must be set to 11 or higher."),
    ifelse(
      ri$nSubmissions[1] > 50,
      vars <- c("aggrRule", "RankEff10", "RankEff20", "RankEff50"),
      ifelse(
        ri$nSubmissions[1] > 20,
        vars <- c("aggrRule", "RankEff10", "RankEff20"),
        vars <- c("aggrRule", "RankEff10")
      )
    )
  )
  
  
  dft <- rii[,vars]
  dft <- melt(dft, id.vars = "aggrRule")
  
  dft$variable <- as.character(dft$variable)
  dft$variable[dft$variable == "RankEff10"] <- "k=10"
  dft$variable[dft$variable == "RankEff20"] <- "k=20"
  dft$variable[dft$variable == "RankEff50"] <- "k=50"
  
  
  
  
  ggplot(dft, aes(y = value, x = aggrRule, fill = aggrRule)) +
    geom_violin(fill = "gray70", color = "gray60") +
    geom_boxplot(color = "black", alpha = 0.9, width = 0.6) +
    geom_boxplot( # overplotting the control condition
      data = dft[dft$aggrRule == "null",],
      fill = "gray30", color = "black", width = 0.6) +
    ggtitle(
      "choice performance",
      subtitle = paste0(
        "(proportion of best k correctly chosen from ", ri$nSubmissions[1],")")
      ) +
    scale_x_discrete(
      position = "bottom", #top
      limits = rev(levels(dft$aggrRule))) +
    scale_y_continuous(
      limits = c(0,1),
      #breaks = seq(from = 0, to = 1, by = 0.2),
      expand = c(0,0)
    ) +
    #facet_wrap("variable", ncol = 1, strip.position = "left") +
    facet_wrap("variable", nrow = 1) +#, strip.position = "left") +
    #facet_grid(row="variable") +
    scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
    #coord_flip() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=12),
      plot.title.position = "panel",
      panel.background = element_rect(fill="gray96"),
      plot.background = element_rect(fill="transparent", color=NA),
      panel.border = element_rect(fill="transparent", color="gray50"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines"),
      strip.background = element_rect(fill="transparent"),
      #strip.text = element_text(angle = 45, hjust = 0, vjust = 0),
      strip.text.y.left = element_text(angle = 0),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 50, hjust = 1),
      legend.position = "NA",
      legend.background = element_rect(fill = "transparent",color=NA),
      legend.box.background = element_rect(fill = "transparent",color=NA),
      text = element_text(size = 15)
    )
}


# now we run and plot the baseline and then some other configurations to compare
# it with. For each configuration we will re-plot ranking and choice 
# performance.
#
# Baseline:
ri <- smallBattery()
plotRankPerf(ri)
plotChoicePerf(ri)


# Lower number of proposals (nSubmissions < 100)
ri <- smallBattery(nSubmissions = 50)
plotRankPerf(ri)
plotChoicePerf(ri)

# Sigma lower than the baseline (sigma < 0.2)
ri <- smallBattery(sigma = 0.1)
plotRankPerf(ri)
plotChoicePerf(ri)

# Sigma higher than the baseline (sigma > 0.2)
ri <- smallBattery(sigma = 0.4)
plotRankPerf(ri)
plotChoicePerf(ri)


} ###############


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Loading the results data file:
# (can take a few seconds)
load(file = "./output/ri.RData")

ri$aggrRule[ri$aggrRule == "highestScore"] <- "highest score" ##################


ri$aggrRule <- factor(
  ri$aggrRule,
  levels = rev(c(
    "mean", "trimmed mean", "hypermean", "median", "majority judgment", 
    "Borda count", "lowest score", "null", "highest score"))
)


#colorScheme = "A" # we'll use this palette from Viridis
#
#
# Selecting the variables we need from the baseline runs:
# Subsetting the runs from the baseline parameter configuration:
#ri$family <- NA
ri$family[ri$aggrRule %in% c("null", "highest score", "lowest score")] <-
  "benchmark\nrules"
ri$family[ri$aggrRule %in% c("mean", "trimmed mean", "hypermean")] <-
  "cardinal\nrules"
ri$family[ri$aggrRule %in% c("median","majority judgment","Borda count")] <-
  "ordinal\nrules"
ri$family <- factor(
  ri$family,
  levels = c("cardinal\nrules", "ordinal\nrules", "benchmark\nrules")
)

palette <- viridisLite::viridis(
  n = 11, begin = 0.2, end = 1, alpha = 1, direction = -1, option = "A")

#ri$color[ri$aggrRule == "mean"] <- palette[1] 
#ri$color[ri$aggrRule == "trimmed mean"] <- palette[2] 
#ri$color[ri$aggrRule == "hypermean"] <- palette[3] 
#ri$color[ri$aggrRule == "median"] <- palette[6] 
#ri$color[ri$aggrRule == "majority judgment"] <- palette[7] 
#ri$color[ri$aggrRule == "Borda count"] <- palette[8] 
#ri$color[ri$aggrRule == "lowest score"] <- palette[11] 
#ri$color[ri$aggrRule == "null"] <- palette[12] 
#ri$color[ri$aggrRule == "highest score"] <- palette[13] 



rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    ri$aggrRule %in% c("mean", "null", "lowest score", "highest score") & ###
    ri$scale == 5 &
    ri$glh == 0.05 &                     ###
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(
  rii$aggrRule,
  levels = rev(
    levels(ri$aggrRule)[levels(ri$aggrRule) %in% unique(rii$aggrRule)])
)

# Selecting the variables we need from the baseline runs:
#rii$aggrRule <- factor(rii$aggrRule, levels = rev(levels(rii$aggrRule)))
dft <- rii[,c(
  "aggrRule", "family",# "color",
  "CohensKappa10", "CohensKappa20", "CohensKappa50"
  #"RankEff10", "RankEff20", "RankEff50"#,
  #"KTDtop10", "KTDtop20", "KTDtop30", "KTDtop40", "KTDtop50",
  #"spearmanTop10", "spearmanTop20", "spearmanTop30",
  #"spearmanTop40", "spearmanTop50"
)]
dft <- melt(dft, id.vars = c("aggrRule", "family"))

dft$variable <- as.character(dft$variable)
dft$variable[dft$variable == "CohensKappa10"] <- "k=10"
dft$variable[dft$variable == "CohensKappa20"] <- "k=20"
dft$variable[dft$variable == "CohensKappa50"] <- "k=50"
#dft$aggrRule <- factor(
#  dft$aggrRule,
#  levels = levels(ri$aggrRule))
#)

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_4.", exportFormat),
  width = 1600,
  height = 1100,
  units = "px",
  res = 300
)
#if(exportFormat == "png") {do.call(png, figureParameters)} else {
#  do.call(tiff, figureParameters)}

ggplot(dft, aes(y = value, x = family, fill = aggrRule, color = aggrRule)) +
  geom_violin(
    #fill = "gray60"#, color = "gray60",
    position =  position_dodge()
  ) +
  geom_boxplot(
    color = "black", alpha = 0.2, width = 0.4,
    position = position_dodge(width = 0.9)
    #position_dodge2(width = 0.9, preserve = "single")
  ) +
  #geom_boxplot( # overplotting the control condition
  #  data = dft[dft$aggrRule == "null",],
  #  fill = "gray30", color = "black", width = 0.6) +
  ggtitle(
    "choice performance",
    subtitle = "(Cohen's Kappa)") +
  scale_x_discrete(
    position = "bottom"#,
    #limits = levels(dft$family)
  ) +
  scale_y_continuous(
    #limits = c(0,1),
    #breaks = seq(from = 0, to = 1, by = 0.2),
    expand = c(0,0)
  ) +
  facet_wrap("variable", nrow = 1, scales = "free_x") +
  scale_fill_manual(values = palette[c(1, 9, 10, 11)]) +
  scale_color_manual(values = palette[c(1, 9, 10, 11)]) +
  #scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.left = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    #axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )


# ______________________________________________________________________________

rii <- subset(
  ri,
  ri$tqd == "top skewed" &
    #ri$aggrRule %in% c("mean", "null", "lowest score", "highest score") & ###
    #ri$scale == 5 &
    ri$glh == 0.05 &                     ###
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none"
)
rii$aggrRule <- factor(
  rii$aggrRule,
  levels = rev(
    levels(ri$aggrRule)[levels(ri$aggrRule) %in% unique(rii$aggrRule)])
)


#if(exportFormat == "png") {do.call(png, figureParameters)} else {
#  do.call(tiff, figureParameters)}

ggplot(
  rii,
  aes(y = CohensKappa20, x = family, fill = aggrRule, color = aggrRule)
) +
  geom_violin(position =  position_dodge()) +
  geom_boxplot(
    color = "black", alpha = 0.2, width = 0.4,
    position = position_dodge(width = 0.9)
  ) +
  scale_x_discrete(position = "bottom") +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap("scale", ncol = 1) +#, scales = "free_x") +
  scale_fill_manual(values = palette[c(1:3, 5:7, 9:11)]) +
  scale_color_manual(values = palette[c(1:3, 5:7, 9:11)]) +
  #scale_fill_viridis(begin = 0, discrete = TRUE, option = colorScheme) +
  theme(
    plot.title = element_text(size=14),
    plot.subtitle = element_text(size=12),
    plot.title.position = "panel",
    panel.background = element_rect(fill="gray96"),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_rect(fill="transparent", color="gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y.left = element_text(angle = 0),
    axis.line = element_blank(),
    axis.title = element_blank(),
    #axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "right",
    legend.background = element_rect(fill = "transparent",color=NA),
    legend.box.background = element_rect(fill = "transparent",color=NA),
    text = element_text(size = 15)
  )








