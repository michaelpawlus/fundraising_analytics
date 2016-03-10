setwd("C:/Users/pawlusm/Desktop")

library(readr)
library(ggplot2)
library(RCurl)

##anon <- read.csv("anon_usa3.csv", stringsAsFactors = FALSE)

x <- getURL("https://raw.githubusercontent.com/michaelpawlus/fundraising_analytics/master/rfm_scoring/rfm_sample_data_set.csv")
anon <- read_csv(x)

## get our column names for future reference
#names(anon)

## convert to date field from character to date then numeric for sorting
anon$date1 <- strptime(anon$latest_date, "%m/%d/%Y")

anon$date2 <- as.numeric(anon$date1)


## subset to only donors
anon <- anon[!(is.na(anon$date2)), ]


## recency score ##

# create your quintile function

ApplyRecencyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon$date2, probs = seq(0, 1, by = 0.20))), 
      labels=FALSE, include.lowest=TRUE)
}

## apply the function to score results

anon$recency <- sapply(anon$date2, ApplyRecencyQuintiles)

#### alternate approach

#anon2 <- anon

#library(data.table)
#anon2 <- setDT(anon2)[, quintile := cut(anon2$date2, quantile(anon2$date2, probs=0:5/5), include.lowest=TRUE, labels=FALSE)]

## frequency quintiles

anon.s <- anon[anon$cons_giving>0,]

anon.r <- anon[anon$cons_giving==0, ]

ApplyFrequencyQuintiles <- function(x) {
  cut(x, breaks=c(unique(quantile(anon.s$cons_giving, probs = seq(0, 1, by = 0.25)))), 
      labels=FALSE, include.lowest=TRUE)
}

anon.s$frequency  <- sapply(anon.s$cons_giving, ApplyFrequencyQuintiles)

anon.r$frequency  <- 0

anon <- rbind(anon.r, anon.s)


## monetary

ApplyMonetaryQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon$ttl_giving, probs = seq(0, 1, by = 0.20))), 
      labels=FALSE, include.lowest=TRUE)
}

anon$monetary <- sapply(anon$ttl_giving, ApplyMonetaryQuintiles)


## rfm score

anon$rfm <- anon$recency + anon$monetary + anon$frequency

## check for patterns

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.5/strwidth(txt)
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




anon$log_tg <- log(anon$ttl_giving + 1)

anon.sub <- anon[ , which(names(anon) %in% c("log_tg","recency","frequency","monetary","rfm"))]
pairs(anon.sub, diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth, cex.labels = 0.67)

#jpeg("rfm_pairs.jpeg", width = 6, height = 3, units = 'in', res = 300)
#pairs(anon.sub, diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth, cex.labels = 0.67)
#dev.off()


#####  velocity scoring


anon$velocity <- log(anon$fy16+1)/(((log(anon$fy15+1)+log(anon$fy14+1))/2)+1)



#### plot for prospects

## check for outliers

qplot(velocity, data=anon, geom="histogram", binwidth = 1) +  theme_bw()

#vo <- qplot(velocity, data=anon, geom="histogram", binwidth = 1) +  theme_bw()
#ggsave(vo, file="velo_outs.jpeg", scale = 0.5, pointsize = 10, dpi = 300)

qplot(affinity, data=anon, geom="histogram", binwidth = 1) +  theme_bw()

#ao <- qplot(affinity, data=anon, geom="histogram", binwidth = 1) +  theme_bw()
#ggsave(ao, file="aff_outs.jpeg", scale = 0.5, pointsize = 10, dpi = 300)

summary(anon$affinity)


anon$vel2 <- anon$velocity
anon$vel2[anon$velocity>2] <- 2

anon$aff2 <- anon$affinity
anon$aff2[anon$affinity>10] <- 10

sp <- ggplot(anon, aes(vel2, low_rating)) + geom_point(aes(size = log_tg, color = aff2))

sp + facet_wrap( ~ rfm, ncol=4) +  theme_bw() + theme(text = element_text(size=5)) + scale_size(range = c(1, 3))

## for printing

#sp <- ggplot(anon, aes(vel2, low_rating)) + geom_point(aes(size = log_tg, color = aff2))

#sp <- sp + facet_wrap( ~ rfm, ncol=4) +  theme_bw() + theme(text = element_text(size=5)) + scale_size(range = c(1, 3))

#ggsave(sp, file="rfm_facets.jpeg", scale = 0.5, pointsize = 10, dpi = 300)

## examples

anon[anon$rfm == 8 & anon$low_rating == 50000 & anon$vel2>1,]

anon[anon$rfm == 7 & anon$low_rating == 100000,]
