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




# Figure 1 and A1: merit distribution___________________________________________
seq <- seq(from = 0, to = 1, by = 0.001)
df <- rbind(
  data.frame(
    condition = rep("high", times = length(seq)),
    x = seq,
    y = dbeta(seq, shape1 = 5, shape2 = 2)
  ),
  data.frame(
    condition = rep("low", times = length(seq)),
    x = seq,
    y = dbeta(seq, shape1 = 2, shape2 = 5)
  ),
  data.frame(
    condition = rep("bimodal", times = length(seq)),
    x = seq,
    y = 1/2 * (dbeta(seq, shape1=2, shape2=5) + dbeta(seq, shape1=5, shape2=2)
    )
  )
)
df$condition <- factor(
  df$condition, levels = c("high", "low", "bimodal"))


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_A1.", exportFormat),
  width = 1300,
  height = 580, #600,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(data = df, aes(x = x, y = y)) +
  geom_path(color = "darkorange1") +
  facet_grid(cols = vars(condition)) +
  scale_y_continuous(limits = c(0, max(df$y +0.05)), expand = c(0,0)) +
  scale_x_continuous(
    limits = c(0,1), expand = c(0,0),
    breaks = c(0,0.5,1), labels = c(0,0.5,1)
  ) +
  labs(title = "reference distribution", y = "density") +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_rect(fill = "gray90"),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "gray97"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(),
    axis.title.x = element_blank()
  )

dev.off()


# Figure 1
df$x <- df$x * 100

figureParameters <- list(
  filename = paste0("./outputGraphics/figure_1.", exportFormat),
  width = 1300,
  height = 580, #600,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(data = df, aes(x = x, y = y)) +
  geom_path(color = "darkorange1") +
  facet_grid(cols = vars(condition)) +
  scale_y_continuous(limits = c(0, max(df$y +0.05)), expand = c(0,0)) +
  scale_x_continuous(
    limits = c(0,100), #expand = c(0,0),
    breaks = c(0,50,100), labels = c(0,50,100)
  ) +
  labs(
    title = "reference distribution", y = "frequency", x = "underlying category"
  ) +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_rect(fill = "gray90"),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "gray97"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line()
  )

dev.off()





# Figure 2 and A2: Grading languages____________________________________________
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
  width = 500,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  d[2,],
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
  ylab("\nunderlying scale") +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(0:5/5),
    labels = sapply(c(0:5 / 5) * 100, FUN = "paste0", "%")
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.3, 0.9))) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
      color = "darkorange1", linetype = "dotted", size = 0.5),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

dev.off()


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_A2.", exportFormat),
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
  ylab("\nunderlying scale") +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:5/5)) +
  scale_x_discrete(expand = expansion(mult = c(0.3, 0.9)), position = "top") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
      color = "darkorange1", linetype = "dotted", size = 0.5),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank()
  )

dev.off()
rm(asymm, gl, cl, labz, ls, padd, pointer, ps, t, d)




# Figure A3: Grading languages__________________________________________________
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
    color = "darkorange1", linetype = "dotted", size = 0.5
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
    x = "underlying scale", y = ""
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
    axis.line.x = element_line(
      color = "darkorange1", linetype = "dotted", size = 0.5),
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
  filename = paste0("./outputGraphics/figure_A3.", exportFormat),
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
  ri$tqd == "high" &
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
  ri$tqd == "high" &
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
    #title = "correctness (k=20)",
    y = "correctness\n(Cohen's kappa)"
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
  ri$tqd == "high" &
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
    #title = "correctness (k=20)",
    x = "panel size (N)", y = "correctness\n(Cohen's kappa)"
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
  ri$tqd == "high" &
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
    #title = "correctness (k=20)",
    y = "correctness\n(Cohen's kappa)"
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
  ri$tqd == "high" &
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
df$boost <- "booster" # Boosting vs non-boosting
nonBoosterRules <- c("median", "lowest score", "highest score")
df$boost[df$aggrRule %in% nonBoosterRules] <- "non-booster"
df$boost[df$aggrRule == "control"] <- "control"
df$boost <- factor(
  df$boost, levels = c("control", "non-booster", "booster"))
df$aggrRule <- as.character(df$aggrRule)
df <- df[df$aggrRule != "trimmed mean",]
df$aggrRule[df$aggrRule == "control"] <- ""
df$aggrRule <- factor(
  df$aggrRule,
  levels = c(
    "", "control", "median", "lowest score", "highest score",
    "mean", "trimmed mean", "hypermean", "gloomy mean", "sunny mean",
    "Borda count", "majority judgment"
  )
)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_7.", exportFormat),
  width = 1120,
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
    y = "correctness\n(Cohen's kappa)"
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
  ri$tqd == "high" &
    ri$scale == 5 &
    #ri$commonUnderstGrades == 0.95 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$glh <- sapply(
  rii$glh,
  FUN = function(x){paste0("\u03D1=", as.character(x))}
)
rii$glh <- as.factor(rii$glh)
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "glh", "CohensKappa20")]
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
  facet_grid(cols = vars(glh), scales = "free_x", switch = "x")+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "correctness (k=20)",
    y = "correctness\n(Cohen's kappa)"
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






# Figure 9 _____________________________________________________________________
# Implicit noise (lambda)
rii <- subset(
  ri,
  ri$tqd == "high" &
    ri$scale == 5 &
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    #ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c("mean", "control") &
    ri$discreteMerit == FALSE
)

rii$reviewerError <- sapply(
  rii$reviewerError,
  FUN = function(x){paste0("\u03BB=", as.character(x))}
)
rii$reviewerError <- as.factor(rii$reviewerError)
#rii$scale <- factor(rii$scale, levels = c("L=2", "L=5", "L=10"))
df <- rii[,c("aggrRule", "baseline", "reviewerError", "CohensKappa20")]
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_9.", exportFormat),
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
  facet_grid(cols = vars(reviewerError), scales = "free_x", switch = "x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(position = "top", limits = c("control", "mean")) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  labs(
    #title = "correctness (k=20)",
    y = "correctness\n(Cohen's kappa)"
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




# Figure 10 ____________________________________________________________________
# Funding rate (k)
rii <- subset(
  ri,
  ri$tqd == "high" &
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
df$variable[df$variable == "CohensKappa5"] <- "K=5"
df$variable[df$variable == "CohensKappa10"] <- "K=10"
df$variable[df$variable == "CohensKappa20"] <- "K=20"
df$variable[df$variable == "CohensKappa50"] <- "K=50"
df$variable <- factor(df$variable, levels = c(
  "K=5", "K=10", "K=20", "K=50"
))

df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE | df$variable != "K=20"] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_10.", exportFormat),
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
    #title = "correctness (all levels of k)",
    y = "correctness\n(Cohen's kappa)"
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




# Figure 11 ____________________________________________________________________
# reference distribution (aka "true merit distribution" or "merit condition")
rii <- subset(
  ri,
  #ri$tqd == "high" &
  ri$scale == 5 & 
    ri$glh == 0.05 &
    ri$truthNoise == 0 &
    ri$nReviewersPerProp == 5 &
    ri$competence == 0.8 &
    ri$ruleVariant == "none" &
    ri$aggrRule %in% c(
      "control", "lowest score", "gloomy mean",
      "mean", "sunny mean", "highest score") &
    ri$discreteMerit == FALSE
)
rii$tqd[rii$tqd == "bottom skewed"] <- "low"
rii$tqd[rii$tqd == "high"] <- "high"
rii$tqd[rii$tqd == "bimodal"] <- "bimodal"

rii$tqd <- factor(rii$tqd, levels = c("high", "low", "bimodal"))
df <- rii[,c("aggrRule", "baseline", "tqd", "CohensKappa20")]

df$aggrRule <- factor(
  df$aggrRule,
  levels = c(
    "control", "lowest score", "gloomy mean",
    "mean", "sunny mean", "highest score")
)
df$condition <- 1 # for determining the fill color of the boxplots.
df$condition[df$baseline == FALSE] <- 2
df$condition[df$aggrRule == "control"] <- 3
df$condition <- as.factor(df$condition)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_11.", exportFormat),
  width = 1800,
  height = 850,
  units = "px",
  res = 300
)
#if(exportFormat == "png") {do.call(png, figureParameters)} else {
#  do.call(tiff, figureParameters)}

ggplot(
  df,
  aes(y = CohensKappa20, x = tqd, fill = condition)
) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_violin(fill = "gray70", color = "gray60", scale = "width", width = 0.8) +
  geom_boxplot(color = "black", alpha = 0.9, width = 0.3) +
  #scale_x_discrete(breaks = levels(df$tqd)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("darkorange", "white", "gray30")) +
  facet_grid(cols = vars(aggrRule)) +
  #facet_wrap(df$aggrRule) +
  #facet_grid(cols = vars(df$aggrRule)) +
  labs(
    y = "correctness\n(Cohen's kappa)"
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
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "NA"
  )

#dev.off()




# Figure B1 ____________________________________________________________________
# Survey: grading standards of SFI reviewers.
#
# Attempts to load the survey (not shared with the repository). Returns a
# message if it can't.
ifelse(
  file.exists("./data/survey.RData"),
  load("./data/survey.RData"),
  warning("Can't load survey responses. Figure B1 cannot be printed")
)


th <- qbeta(1:4 / 5, shape1 = 2, shape2 = 1) * 100
df <- rbind(
  data.frame(
    x = s$i$q34,
    th = rep(
      "1st threshold",
      times = length(s$i$q34)
    )),
  data.frame(
    x = s$i$q33,
    th = rep(
      "4th threshold",
      times = length(s$i$q33)
    ))
)


figureParameters <- list(
  filename = paste0("./outputGraphics/figure_B1.", exportFormat),
  width = 1200,
  height = 680,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(x = x)) +
  geom_histogram(
    breaks = 0:10 * 10,
    color = "black",
    position = "dodge",
    fill = "darkorange", alpha = 0.8
  ) +
  #geom_vline(xintercept = th, linetype = 2, color = "black") +
  facet_grid(cols = vars(th)) +
  scale_fill_viridis_d(name = "", begin = 0.85, end = 0.4, option = "A") +
  scale_x_continuous(breaks = 0:10 * 10, labels = function(x){paste0(x, "%")}) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "quality percentage", y = "frequency") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95"),
    legend.position = "NA",
    legend.background = element_rect(fill = "gray95"),
    strip.background = element_blank(),#element_rect(fill = "gray95"),
    #axis.title = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 7))

dev.off()
