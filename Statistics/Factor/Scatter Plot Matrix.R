## CGU Scatter Plot Matrix

#Set Working Directory
setwd("D:/Dropbox/Dropbox/R/WD")

# Load Library
library(ggplot2)
library(Hmisc)

#Or Install
install.packages("ggplot2")
install.packages("Hmisc")

#Load data into dat
dat <- read.csv("https://www.dropbox.com/s/hry1pa4tyyfiomn/Test.csv?dl=1")
View(dat)

## Scatter Plot Matrix with histogram diagonals and r/CI95 on the top panels (Run top to bottom from here)
## Histograms on the diagonal (Thanks to Ane Handles)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="lavender", ...)
}
## Correlations & 95% CIs on the upper panels (Thanks to Ane Handles) + p-values 
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  prefix <- "r = "
    rc <- cor.test(x,y)
  rci <- rc$conf.int
  p <- cor.test(x, y)$p.value
  txt4 <- format(c(p, 0.123456789), digits = digits)[1]
  txt4 <- paste(",\np= ", txt4, sep = "")
  if(p<0.01) txt4 <- paste(",\np= ", "<0.01", sep = "")
  txt2 <- format(c(rci, 0.123456789), digits=digits)[1]
  txt3 <- format(c(rci, 0.123456789), digits=digits)[2]
    prefix2 <- "\nCI = "
  txt <- paste(prefix, txt, prefix2, txt2, ", ", txt3, sep="", txt4)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}
## Actual Scatter Plot Matrix
pairs(dat[1:6], 
      lower.panel=panel.smooth, cex = .8, pch = 21, bg="steelblue",
      diag.panel=panel.hist, cex.labels = 1.2, font.labels=2, 
      upper.panel=panel.cor)


