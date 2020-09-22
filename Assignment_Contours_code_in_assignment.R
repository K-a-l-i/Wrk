########################
# Author: J. Baxter
# Last update: 26/03/2019
# MST 301 Practical/Assignment: Contours
########################
rm(list=ls())

library(datasets)
library(grDevices)
library(graphics)
library(lattice)


## Set the working directory.
# I set my working directory in all my Rscipt files. Please remember this is 
# on a linux machine and hence the specification may differ on your Windows PC's

setwd("/home/stjb/lyxdocs/mst3/Paper_1_IDT/projects/Project7Contours/")

########################
# Introduction: Topography
# Drawing pretty pictures of mountains/volcanos
# Just for demonstration purposes!
########################
# initial figure
wireframe(volcano,shade = TRUE)
# dev.print(device=postscript,file="Maunga_topography_color_default.eps")

# rotate and view from abova
wireframe(volcano, shade = TRUE, aspect = c(61/87, 0.4),screen = list(z = -120, x = -45))
# dev.print(device=postscript,file="Maunga_topography_color_1.eps")

# grey scale
# wireframe(volcano, shade = TRUE, aspect = c(61/87, 0.4),screen = list(z = -120, x = -45),light.source = c(0,0,10), distance = .2, shade.colors = function(irr, ref, height, w = .5) grey(w * irr + (1 - w) * (1 - (1-ref)^.4)))

# Create the 3D profile, the left panel in figure 1, figure 1a.
# Persp: Lots of colour
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"

fcol <- fill
zi <- volcano[ -1,-1] + volcano[ -1,-61] +
           volcano[-87,-1] + volcano[-87,-61]  ## / 4
fcol[-i1,-i2] <-
    terrain.colors(20)[cut(zi, quantile(zi, seq(0,1, len = 21)),
                           include.lowest = TRUE)]
par(mar=rep(.5,4))
# now plot the exaggerated relief (figure ):
persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,
      ltheta = -120, shade = 0.4, border = NA, box = FALSE)
# dev.print(device=postscript,file="Maunga_topography_color.eps")

########################
# An example of contours
########################
# eg from: http://svn.r-project.org/R/trunk/src/library/graphics/demo/image.R
x <- 10*(1:nrow(volcano))
x.at <- seq(100, 800, by=100)
y <- 10*(1:ncol(volcano))
y.at <- seq(100, 600, by=100)
# Using Terrain Colors
image(x, y, volcano, col=terrain.colors(100),axes=FALSE)
# dev.print(device=postscript,file="Land_absorbs_light.eps")
# add lines at various heights and labels
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="Maunga Whau Volcano", sub = "col=terrain.colors(100)", font.main=4)
# dev.print(device=postscript,file="Maunga_contours_terrain.eps")

########################
# Univariate normal constant density
########################
x<-seq(-4,4,length=100)
plot(x,dnorm(x),type="l", main="Density of a N(0,1) random variable", xlab="X", ylab=expression(paste("Density: ",f[X](x))),lwd=3)
abline(h=0.2,lwd=2, lty=2, col="blue")
abline(v=c(-1.175159,1.175159),lwd=2, lty=3,col="red")
mtext(text=c(expression(x[1]),expression(x[2])), side=1, at=c(-sqrt(-2*log(0.2*sqrt(2*pi))),sqrt(-2*log(0.2*sqrt(2*pi)))),srt=90)
# dev.print(file="constant_density_univariate.eps")
sqrt(-2*log(0.2*sqrt(2*pi)))

########################
# Distance: Discussed in more details in the Honours multivariate course!
########################
library(ellipse)
plot(ellipse(0.0), type = 'l',main="All points are the same Euclidian distance\n from the center")
abline(h=0, v=0, lty=3)
# dev.print(device=postscript,file="euclidian_eg_1.eps")
plot(ellipse(0.8), type = 'l', main="All points are the same Mahalanobis distance\n from the center")
abline(h=0, v=0, lty=3)
# dev.print(device=postscript,file="mahalanobis_eg_1.eps")

########################
# Plotting ellipses
# Note: These are plots of confidence intervals/regions (As per Hons!)
########################
# A generic function returning an ellipse or other outline of a confidence region for two parameters.
plot(ellipse(0.0), type = 'l', main="Plotting an ellipse", xaxt="n", yaxt="n", xlab="", ylab="")
abline(h=0, v=0, lty=3,col="grey")
text(sqrt(qchisq(0.95, 2))/2,0,expression(x[1]),col="grey")
text(0,sqrt(qchisq(0.95, 2))/2,expression(x[2]),col="grey")
## label points
text(0,0,labels="(h,k)",col="black")
text(sqrt(qchisq(0.95, 2))-0.2,0,labels="(h+a,k)",col="black")
text(-sqrt(qchisq(0.95, 2))+0.2,0,labels="(h-a,k)",col="black")
text(0,sqrt(qchisq(0.95, 2))-0.2,0,labels="(h,k+b)",col="black")
text(0,-sqrt(qchisq(0.95, 2))+0.2,0,labels="(h,k-b)",col="black")
## Create a box around the ellipse
## default is a 95% CI
#right
segments(sqrt(qchisq(0.95, 2)),-sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),lty=4,col="lightgrey")
#left
segments(-sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),lty=4,col="lightgrey")
#top
segments(-sqrt(qchisq(0.95, 2)),-sqrt(qchisq(0.95, 2)),-sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),lty=4,col="lightgrey")
#bottom
segments(-sqrt(qchisq(0.95, 2)),-sqrt(qchisq(0.95, 2)),sqrt(qchisq(0.95, 2)),-sqrt(qchisq(0.95, 2)),lty=4,col="lightgrey")
# dev.print(device=postscript,file="plot_ellipse_1.eps")

########################
# Example of a bivariate normal density: mu = c(10,30), sigma = (5,0.5,0.5,1)
########################
source(file="/home/stjb/lyxdocs/mst3/Paper_1_IDT/2017/projects/project3/Rinstructions/bivar.R")
bivardensityMst2Plot(10,30,5,1,0.5)
# dev.print(device=postscript,file="Bivariatenormal_10_30_5_1_0pt5.eps")

########################
# Example of bivariate normal density: mu = c(10,30), sigma = various
# with constant density: 0.04
########################
bivardensityMst2PlotPlane(10,30,5,1,0.5,0.04)
a <- bivardensityMst2PlotPlane(10,30,5,1,0.5,0.04)
# dev.print(device=postscript,file="Bivariatenormal_plane_10_30_5_1_0pt5_0pt4.eps")

