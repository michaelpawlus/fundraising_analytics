setwd("C:/Users/pawlusm/Desktop")

library(readr)
library(ggplot2)

anon <- read.csv("anon_usa3.csv", stringsAsFactors = FALSE)

## need recency (date), frequency (con_giving), monetary (ttl_giving) for RFM
## need 3 fiscal years for velocity


## get our column names for future reference
names(anon)

## convert to date field from character to date then numeric for sorting
anon$date1 <- strptime(anon$latest_date, "%m/%d/%Y")

anon$date2 <- as.numeric(anon$date1)


## subset to only donors
anon <- anon[!(is.na(anon$date2)), ]


## recency score ##

# create your quintile function

ApplyRecencyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon$date2, probs = seq(0, 1, by = 0.20))), 
      labels=c(1:5), include.lowest=TRUE)
}

## apply the function to score results

anon$recency <- sapply(anon$date2, ApplyRecencyQuintiles)

anon$recency <- as.numeric(as.character(anon$recency))


## frequency quintiles

anon.s <- anon[anon$cons_giving>0,]

anon.r <- anon[anon$cons_giving==0, ]

ApplyFrequencyQuintiles <- function(x) {
  cut(x, breaks=c(unique(quantile(anon.s$cons_giving, probs = seq(0, 1, by = 0.25)))), 
      labels=c(1:4), include.lowest=TRUE)
}

anon.s$frequency  <- sapply(anon.r$cons_giving, ApplyFrequencyQuintiles)

anon.s$frequency <- as.numeric(as.character(anon.s$frequency))

anon.r$frequency  <- 0

anon <- rbind(anon.r, anon.s)


## monetary

ApplyMonetaryQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon$ttl_giving, probs = seq(0, 1, by = 0.20))), 
      labels=c(1:5), include.lowest=TRUE)
}

anon$monetary <- sapply(anon$ttl_giving, ApplyMonetaryQuintiles)

anon$monetary <- as.numeric(as.character(anon$monetary))


## rfm score

anon$rfm <- anon$recency + anon$monetary + anon$frequency

## check for patterns

anon$log_tg <- log(anon$ttl_giving + 1)

anon.sub <- anon[ , which(names(anon) %in% c("log_tg","recency","frequency","monetary","rfm"))]
pairs(anon.sub, diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth)

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE) 
{if (smooth ){
  if (scale) {
    pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
  }
  else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
  } #else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
}

else      #smooth is not true
{ if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale)
} else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor) }
} #end of else (smooth)

}   #end of function





#####  velocity scoring


anon$velocity <- log(anon$fy16+1)/(((log(anon$fy15+1)+log(anon$fy14+1))/2)+1)



#### plot for prospects


anon$vel2 <- anon$velocity
anon$vel2[anon$velocity>2] <- 2

anon$aff2 <- anon$affinity
anon$aff2[anon$affinity>10] <- 10

sp <- ggplot(anon, aes(vel2, low_rating)) + geom_point(aes(size = log_tg, color = aff2))

sp + facet_wrap( ~ rfm, ncol=4)


## examples

anon[anon$rfm == 8 & anon$low_rating == 50000 & anon$vel2>1,]

anon[anon$rfm == 7 & anon$low_rating == 100000,]
