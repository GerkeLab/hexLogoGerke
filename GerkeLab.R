library(pROC)
library(ggplot2)

# this file contains light edits to 
# https://github.com/GuangchuangYu/hexSticker
# where the main intent was to swap .png exports to .pdf
source("sticker.R")

# simulate data for low pAUC marker
set.seed(125)

ncases <- ncontrols <- 10000 #in each group
# generate a vector of outcomes
y <- c(rep(1, ncases), rep(0, ncontrols))
xoffset <- c(rbeta(ncases, 1.5, .3), rbeta(ncontrols, .4, .5))
x <- scale(xoffset + rnorm(ncases+ncontrols, sd=.3))

rocfit <- roc(response=y, predictor=as.numeric(x))
pauc <- auc(rocfit, partial.auc=c(1,.8))
smoothed <- smooth(rocfit, method="density")

# the original plot in base R
# plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab=NA, ylab=NA)
# polygon(list(x=c(0,.2,.2,0), y=c(0,0,1,1)), col="lightgray", lwd=.5)
# lines(1-smoothed$sp, smoothed$se, col="darkorange2", lwd=2)
# t1ind <- which.min(abs(smoothed$sp-.8))
# polyvec <- list(x=c(1-smoothed$sp[length(smoothed$sp):t1ind], .2),
#                 y=c(smoothed$se[length(smoothed$se):t1ind], 0))
# polygon(polyvec, col="darkblue", density=15, angle=125)
# abline(0,1,lty=2,col="darkgray")

# using ggplot2
x <- 1-smoothed$sp
y <- smoothed$se
t1ind <- which.min(abs(smoothed$sp-.8))

p1 <- ggplot(NULL, aes(x=x, y=y)) +
   geom_abline(slope=1, linetype="dashed", alpha=.5) + 
   geom_line(colour="darkorange2", size=.75) + 
   geom_polygon(aes(x=c(0,.2,.2,0), y=c(0,0,1,1)), alpha=.2) +
   geom_polygon(aes(x=c(x[length(x):t1ind], .2), y=c(y[length(y):t1ind], 0)), colour="navyblue", fill="skyblue2", alpha=.4) +
   theme_bw() +
   theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank()) 

p2 <- ggplot(NULL, aes(x=1, y=1)) +
  geom_label(aes(x=1, y=1, label="L"), size=15, 
             label.size=.75, label.r=unit(0, "lines"), 
             label.padding=unit(0.5, "lines")) + 
  geom_text(aes(x=2, y=1, label="A"), size=15) + 
  geom_text(aes(x=3, y=1, label="B"), size=15) + 
  geom_segment(aes(x=1.08, xend=1.92, y=1, yend=1), size = .75,
               arrow = arrow(length = unit(0.5, "cm")),
               lineend="round") +
  geom_curve(aes(x=1.08, xend=2.92, y=1, yend=1), size = .75,
             arrow = arrow(length = unit(0.5, "cm")),
             lineend="round", curvature=.5, ncp=100) +
  theme_void() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) 

sysfonts::font_add_google("Fjalla One")
sticker(subplot=p1, s_x=1, s_y=1, s_width=1, s_height=1,
        subplot2=p2, s2_x=1, s2_y=.5, s2_width=1.2, s2_height=.3, 
        package="GERKE", p_x=1, p_y=1.6, p_color="gray60",
        p_family="Fjalla One",
        h_fill="white", h_color="gray60")
