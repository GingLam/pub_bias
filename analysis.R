library(truncnorm)
library(ggplot2)
library(dplyr)

# set working directory to source file


###### under extreme bias ######

### Table 1: a list of nominal and actual p-values ###

## some particular values to account for

# conventional critical vals for p-value 0.05
qnorm(0.95) # 1.645 

# conventional critical vals for p-value 0.01
qnorm(0.99) # 2.326 

# actual critical vals for p-value 0.05 under extreme bias (i.e. truncated below 1.645)
qtruncnorm(0.95, a=1.645) # 2.807 

# actual critical vals for p-value 0.01 under extreme bias (i.e. truncated below 2.326)
qtruncnorm(0.99, a=2.326) # 3.719 

# make table 1
tscore <- c(1.645, seq(1.7, 2.3, 0.1), 
            2.326, seq(2.4, 2.8, 0.1), 
            2.807, seq(2.9, 3.7, 0.1),
            3.719, seq(3.8, 4.2, 0.1))
nominal <- round(1-pnorm(tscore), 3)
actual <- round(1-ptruncnorm(tscore, a=qnorm(0.95), mean=0, sd=1), 3)
#actual2 <- round((1-pnorm(tscore))/(1-pnorm(1.645)), 3)
actual.01 <- round(1-ptruncnorm(tscore, a=qnorm(0.99), mean=0, sd=1), 3)
tbl <- data.frame(cbind(tscore, nominal, actual, actual.01))
tbl

# numerically verify how many times is actual p-value of nominal p-value? --- 20 times
nominal2 <- 1-pnorm(tscore) # without rounding
actual2 <- 1-ptruncnorm(tscore, a=1.645, mean=0, sd=1)
tbl2 <- data.frame(cbind(tscore, nominal2, actual2))
tbl2['times'] <- tbl2['actual2']/tbl2['nominal2']
tbl2


### Figure 1: relative PDF of Truncated Normal Distributions under absolute pub bias ###
pdf("plots/two_truncnorm.pdf")

# make empty plot
plot (c(0.5,6), c(0,1.5), type="n", xlab="t-score", ylab="Relative probability density", main="", 
      xaxt='n', yaxt='n', ylim=c(0, 1.5), yaxs = "i" , xlim=c(0.5, 6)) 

# interaction point of two densities
x <- uniroot(function(x) dtruncnorm(x, a=qnorm(0.95))-dtruncnorm(x, a=qnorm(0.95), mean=3), 
             interval=c(2, 3))$root # 2.467973

# add axes
axis(1, c(1, 2, x, 3, 4, 5, 6), c(1, 2, 2.468, 3, 4, 5, 6), cex.axis=0.9)
axis(3, c(1, 1.645, 2, 2.326, 3, 4, 5, 6), c(1, 1.645, 2, 2.326, 3, 4, 5, 6), cex.axis=0.9)
axis(2, c(0, 0.5, 1, 1.5), cex.axis=0.9, las=2)

# add density lines
s <- seq(1.645,6,0.01)
lines(s, dtruncnorm(s, a=qnorm(0.95), mean=0, sd=1), lty=1, lwd=2)
lines(s, dtruncnorm(s, a=qnorm(0.95), mean=3, sd=1), lty=5, lwd=2)

# add shaded areas
polygon(c(s[s>=qnorm(0.99)], qnorm(0.99) ),  
        c(dtruncnorm(s[s>=qnorm(0.99)], a=qnorm(0.95), mean=0, sd=1), 0), 
        col=adjustcolor("grey", alpha.f = 0.4), border=NA)
polygon(c(s[s>=qnorm(0.99)], qnorm(0.99) ),  
        c(dtruncnorm(s[s>=qnorm(0.99)], a=qnorm(0.95), mean=3, sd=1), 0), 
        col=adjustcolor("grey", alpha.f = 0.4), border=NA)

# add vertical lines
abline(v=qnorm(0.95), col='grey', lwd=2)
abline(v=qnorm(0.99), col='grey', lwd=2)

# add vertical line at intersection point

segments(x, 0, x, dtruncnorm(x, a=qnorm(0.95)), lty=3, lwd=2)

# add legend
legend("topright", c("Null model", "Alternative model"), 
       lty=c(1, 5, 3), lwd=c(2, 2, 2), cex=0.9, seg.len=4, bty='n')

dev.off()



### Figure 2: new relative PDFs when sample size increases by 4 times ###
# note: X-axis is now unstandardized effect size, NOT t-scores

pdf('plots/larger_sample_size.pdf')

# draw density line for null model
t <- seq(qnorm(0.95, sd=0.5), 6, 0.01)
plot(t, dtruncnorm(t, sd=0.5, a=qnorm(0.95, sd=0.5)), type='l', 
     xlim=c(0.5, 6), ylim=c(0, 1.5), lwd=2, 
     xlab='X (unstandardized effect size = t-value/2)', ylab='Relative probability density',
     xaxt='n', yaxt='n', yaxs = "i" )

# interaction point - 1.749643
x <- uniroot(function(x) dtruncnorm(x, sd=0.5, a=qnorm(0.95, sd=0.5))-dtruncnorm(x, mean=3, sd=0.5, a=qnorm(0.95, sd=0.5)), interval=c(1,3))$root

# add axes
axis(1, c(1, x, 2, 3, 4, 5, 6), c(1, 1.750, 2, 3, 4, 5, 6), cex.axis=0.9)
axis(3, c(qnorm(0.95, sd=0.5), 1, 2, 3, 4, 5, 6), c(0.822, 1, 2, 3, 4, 5, 6), cex.axis=0.9)
axis(2, c(0, 0.5, 1, 1.5), cex.axis=0.9, las=2)

# add density line for alternative model
lines(t, dtruncnorm(t, mean=3, sd=0.5, a=qnorm(0.95, sd=0.5)), lty=5, lwd=2)

# add vertical line for truncation point
abline(v=0.822, col='grey', lwd=2)

# mark intersection point
segments(x, 0, x, dtruncnorm(x, a=qnorm(0.95, sd=0.5), sd=0.5), lwd=2)

legend('topright', 
       legend=c("Null model", "Alternative model"), 
       lty=c(1,5), lwd=c(2,2),
       seg.len=4, bty='n', cex=0.9)
dev.off()


# where do the two truncated normal intersect?
a <- qnorm(0.95) # 1.645
uniroot(function(x) dtruncnorm(x, a, mean=3, sd=1)-dtruncnorm(x, a, mean=0, sd=1), interval=c(1.645,6))

a <- qnorm(0.95, sd=0.5) # 0.822
uniroot(function(x) dtruncnorm(x, a, mean=3, sd=0.5)-dtruncnorm(x, a, mean=0, sd=0.5), interval=c(1,2))



### Table 2: adjusted critical values ###

# normal-truncated normal mixture model - ONE TAIL
cutoff <- qnorm(1-0.05)
cutoff

# define PDF
pdf.mix <- function(x, p, cutoff) {
  return((1-p)*dnorm(x)+p*dtruncnorm(x,a=cutoff,mean=0,sd=1))
}

# define equation to look for root
eqn.mix <- function(x, p, pvalue, cutoff) {
  return((1-p)*(1-pnorm(x))+p*(1-ptruncnorm(x,a=cutoff,mean=0,sd=1))-pvalue)
}

# define function to return t-value corresponding to the given p-value and cutoff
qmixture <- function(p, pvalue, cutoff) {
  uniroot(eqn.mix, p=p, pvalue=pvalue, cutoff=cutoff, lower=1, upper=5)
}

# for example
qmixture(0, 0.05, 1)$root # cutoff value does not matter because truncnormal component has 0 weight
qnorm(1-0.05)
qmixture(0, 0.01, 1)$root
qnorm(1-0.01)

qmixture(0.1, 0.05, qnorm(1-0.05))$root
qmixture(0.5, 0.05, qnorm(1-0.05))$root

qmixture(1, 0.1, qnorm(1-0.05))$root
qmixture(1, 0.05, qnorm(1-0.05))$root
qmixture(1, 0.01, qnorm(1-0.05))$root

# verify by integrating over the customized pdf
integrate(pdf.mix, p=0, cutoff=cutoff, lower=-Inf, upper=qmixture(0, 0.05, cutoff)$root)
integrate(pdf.mix, p=0.1, cutoff=cutoff, lower=-Inf, upper=qmixture(0.1, 0.05, cutoff)$root)
integrate(pdf.mix, p=0.5, cutoff=cutoff, lower=-Inf, upper=qmixture(0.5, 0.05, cutoff)$root)
integrate(pdf.mix, p=1, cutoff=cutoff, lower=-Inf, upper=qmixture(1, 0.01, cutoff)$root)


# get critical values to achieve desired p-value in one-tailed test ###
cutoff <- qnorm(1-0.05)
cutoff

# define values for mixture weight
p <- seq(0,1,0.01)

t.onetail.0.05 <- rep(NA, length(p))
for (i in 1:length(p)) {
  t.onetail.0.05[i] <- qmixture(p[i], 0.05, cutoff)$root  
}

t.onetail.0.01 <- rep(NA, length(p))
for (i in 1:length(p)) {
  t.onetail.0.01[i] <- qmixture(p[i], 0.01, cutoff)$root  
}



# normal-truncatednormal mixture model - *TWO TAIL*

# pdf is not needed - skip

eqn.mix.2tail <- function(x, p, pvalue, cutoff) {
  return((1-p)*2*(1-pnorm(x))+p*2*(1-ptruncnorm(x, a=cutoff, mean=0, sd=1))-pvalue)
}

qmixture.2tail <- function(p, pvalue, cutoff) {
  uniroot(eqn.mix.2tail, p=p, pvalue=pvalue, cutoff=cutoff, lower=1, upper=6)
}

# for example
qmixture.2tail(0, 0.1, 1)$root # cutoff value does not matter because truncnormal component has 0 weight
qnorm(1-0.1/2)
qmixture.2tail(0, 0.05, 1)$root
qnorm(1-0.05/2)
qmixture.2tail(0, 0.01, 2)$root
qnorm(1-0.01/2)

qmixture.2tail(0.1, 0.05, qnorm(1-0.025))$root
qmixture.2tail(0.5, 0.05, qnorm(1-0.025))$root

qmixture.2tail(1, 0.01, qnorm(1-0.025))$root
qmixture.2tail(1, 0.05, qnorm(1-0.025))$root
qmixture.2tail(1, 0.1, qnorm(1-0.025))$root


# get critical value in two-tail test for pvalue=0.05 and 0.01
cutoff <- qnorm(1-0.025)
cutoff

p <- seq(0,1,0.01) # values for mixture weight

t.twotail.0.05 <- rep(NA, length(p))
for (i in 1:length(p)) {
  t.twotail.0.05[i] <- qmixture.2tail(p[i], 0.05, cutoff)$root  
}

t.twotail.0.01 <- rep(NA, length(p))
for (i in 1:length(p)) {
  t.twotail.0.01[i] <- qmixture.2tail(p[i], 0.01, cutoff)$root  
}


### Figure 3: plot one-tail and two-tail critical values together ###
pdf("plots/adj_critical_vals.pdf")

# plot line and selected points on the line
plot(p, t.onetail.0.05, type='l', xlab="Severity of publication bias", 
     ylab="Adjusted critical value", main="", ylim=c(1.5, 3.8))
points(p[seq(1, 101, 5)], t.onetail.0.05[seq(1, 101, 5)], pch=16)
lines(p, t.onetail.0.01)
points(p[seq(1, 101, 5)], t.onetail.0.01[seq(1, 101, 5)], pch=1)
lines(p, t.twotail.0.05)
points(p[seq(1, 101, 5)], t.twotail.0.05[seq(1, 101, 5)], pch=4)
lines(p, t.twotail.0.01)
points(p[seq(1, 101, 5)], t.twotail.0.01[seq(1, 101, 5)], pch=2)

legend('bottomright', pch=c(16, 1, 4, 2), lty=c(1, 1, 1, 1), cex=1, bty='n',
       legend=c("p=0.05, one-tail","p=0.01, one-tail", "p=0.05, two-tail","p=0.01, two-tail"))
dev.off()




### TABLE 2: adjusted critical values ###
# first two columns
cutoff <- qnorm(1-0.05)
cutoff
p <- c(0, 0.01, 0.05, seq(0.1, 1, 0.05))
vals <- cbind(p, 
              round(sapply(p, function(p) qmixture(p, 0.05, cutoff)$root), 3),
              round(sapply(p, function(p) qmixture(p, 0.01, cutoff)$root), 3))
vals

# last two columns
cutoff <- qnorm(1-0.025)
cutoff
p <- c(0, 0.01, 0.05, seq(0.1, 1, 0.05))
vals <- cbind(vals, 
              round(sapply(p, function(p) qmixture.2tail(p, 0.05, cutoff)$root), 3),
              round(sapply(p, function(p) qmixture.2tail(p, 0.01, cutoff)$root), 3))
vals




########################################################
### analyze t-statistics in JQC and Criminology 2016 ###
########################################################

# read data
df.crim <- read.csv("data/crim_data.csv", stringsAsFactors=F, encoding="utf-8")
head(df.crim)

# count t-values per article
count <- data.frame(table(df.crim$title))
count

# 23 articles, among which 19 article have t-values
nrow(count)
nrow(count[count$Freq>1, ])
no_t_vals <- count[count$Freq==1, "Var1"]
no_t_vals
df.crim <- df.crim[!(df.crim$title %in% no_t_vals), ]
df.crim$title <- factor(df.crim$title)

# distribution of number of t-values per article
hist(count[count$Freq>1, "Freq"], breaks=20, 
     main="Number of t-values per article in Criminology", xlab="")

# bar plot of number of t-scores per article
count2 <- count[count$Freq>1, ] # remove articles without t-values
count2 <- count2[order(count2$Freq, decreasing = T), ]
barplot(count2$Freq, names.arg = count2$Var1, xaxt="n", 
        main="Number of t-values per article \n in Criminology")


### grouped bar plot: IVs and controls ###
counts <- data.frame(table(df.crim$title, df.crim$hypo))

# side-by-side
ggplot(counts, aes(factor(Var1), Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1", name="", labels=c("Control", "IV")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

# stacked
ggplot(counts, aes(factor(Var1), Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_fill_brewer(palette = "Set1", name="", labels=c("Control", "IV")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


### get t-values ###

# put test values in a new column called VALUE
df.crim$VALUE <- NA

### option 1: use coef/SE ###
df.crim$se
sum((!is.na(df.crim$se) & as.numeric(df.crim$se)==0))

# function to calculate t-value - divide coef by se
get.tval <- function(row) {
  if (is.na(row['se'])) {
    return(NA)
  } else if (as.numeric(row['se'])==0) {
    return(NA)
  } else {
    return(abs(as.numeric(row['coef'])/as.numeric(row['se'])))
  }
}
df.crim$VALUE <- apply(df.crim, 1, get.tval)

# make sure no Inf value from dividing by 0
which(df.crim$VALUE==Inf) 

# how many t-scores filled? 1111
sum(!is.na(df.crim$VALUE)) 
nrow(df.crim) #total: 1458


### option 2: convert odds ratio ###
# formula: SE = (log(odds ratio upper bound) - log(odds ratio lower bound))/3.92, where 3.92 = 1.96*2 
#          for 95% confidence interval
colnames(df.crim)

# function to construct t-value from odds ratios
convertOR <- function(row) {
  if (!is.na(row['OR']) & !is.na(row['X95..CI..lb.'])) {
    coef <- log(as.numeric(row['OR']))
    se <- (log(as.numeric(row['X95..CI..ub.'])) - log(as.numeric(row["X95..CI..lb."])))/1.96/2
    if (se == 0) {
      print("se == 0")
      return(NA)
    } else {
      return(abs(coef/se))
    }
  } else if (!is.na(row['OR']) & !is.na(row['X99.9..CI..lb.'])) {
    coef <- log(as.numeric(row['OR']))
    # qnorm(1-0.001/2) = 3.29
    se <- (log(as.numeric(row['X99.9..CI..ub.'])) - log(as.numeric(row["X99.9..CI..lb."])))/3.29/2
    if (se == 0) {
      print("se == 0")
      return(NA)
    } else {
      return(abs(coef/se))
    }
  } else {
    return(NA)
  }
}
df.crim[(!is.na(df.crim$OR) & !is.na(df.crim$X95..CI..lb.)), "VALUE"] <- apply(df.crim[(!is.na(df.crim$OR) & !is.na(df.crim$X95..CI..lb.)), ], 1, convertOR)
df.crim[(!is.na(df.crim$OR) & !is.na(df.crim$X99.9..CI..lb.)), "VALUE"] <- apply(df.crim[(!is.na(df.crim$OR) & !is.na(df.crim$X99.9..CI..lb.)), ], 1, convertOR)
sum(!is.na(df.crim$VALUE)) # filled: 1324
nrow(df.crim) # total:1458


### option 3: copy existing t-values, z-values - use accurate t-scores whenever available 
df.crim[!is.na(df.crim$t.value), "VALUE"] <- abs(df.crim[!is.na(df.crim$t.value), "t.value"])
df.crim[!is.na(df.crim$z.value), "VALUE"] <- abs(df.crim[!is.na(df.crim$z.value), "z.value"])
sum(!is.na(df.crim$VALUE)) # filled: 1420 
nrow(df.crim) # total:1458


sum(is.na(df.crim$VALUE))
df.crim[which(is.na(df.crim$VALUE)), ] # skimmed, all due to zero se 
sum(!is.na(df.crim$VALUE))


# draw histogram of IVs only, exclude control variables 
x <- df.crim[(!is.na(df.crim$VALUE) & df.crim$hypo==1), "VALUE"]
length(x) # 979
range(x)

# are there t-values of zero?
which(df.crim$VALUE==0)  # either coef is 0 or odds ratio is 1
for (i in which(df.crim$VALUE==0)) {
  cat(i, df.crim[i, "coef"], df.crim[i, "se"], df.crim[i, "OR"], "\n")
}


total <- length(x)
total 

sum(x<1.96) # 411
sum(x<1.96)/total # 42%

sum(x >= 1.96) # 568
sum(x >= 1.96)/total # 58%

sum(x>=1.96 & x<2.766) # 148
148/total # 15%
148/568 # 26%

sum(x>=2.766) # 420
sum(x>=2.766)/total # 43%

sum(x>=2.766 & x<3.227)
sum(x>=3.227)


### create visualizations ### 

final.crim <- data.frame(pos=1:3, x=c("<1.96", "[1.96, 2.766)", ">=2.766"), freq=c(sum(x<1.96), sum(x>=1.96 & x<2.766), sum(x>=2.766)))
final.crim$pct <- sapply(final.crim$freq, function(p) paste(round(p/length(x)*100, 0), "%", sep=""))
final.crim

# bar plot
barplot(final.crim$freq, names.arg = final.crim$x)

# bar plot version 2
ggplot(final.crim, aes(x = reorder(x, pos), y = freq)) +
  geom_bar(stat = "identity", fill="black") +
  #geom_text(size = 3, col="white", position=position_stack(vjust = 1)) +
  geom_text(aes(label = pct, y = freq+20), size = 10) +
  scale_y_continuous(limits = c(0,610), expand = c(0, 0)) +
  xlab("") + 
  ylab("Frequency") +
  theme_minimal() +
  theme(text = element_text(size=20))

# density plot 
hist(x, breaks=10) 
d <- density(x)
plot(d) # too skewed

# too skewed, so topcode at 95th percentile
quantile(x, probs=0.95)
x2 <- x[x < as.numeric(quantile(x, probs=0.95))]
d <- density(x2)
plot(d)

# Figure 4(a): density plot, overlaid with histogram
pdf("plots/dens_plot_crim.pdf")
par(yaxs="i",las=1)
hist(x2, prob=T, xlab="t-statistics", main="", col="grey",border="white", ylim=c(0, 0.31))
box(bty="l")
lines(density(x2), lwd=2)
abline(v=1.960, lty=3, lwd=2)
abline(v=2.766, lty=3, lwd=2)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="black")
dev.off()




### repeat above steps the above for JQC ###
df.jqc <- read.csv("data/jqc_data.csv", stringsAsFactors=F)
head(df.crim)

# count t-values per article
count <- data.frame(table(df.jqc$title))
count

# 30 articles, 20 article with t-values
nrow(count)
nrow(count[count$Freq>1, ])
no_t_vals <- count[count$Freq==1, "Var1"]
no_t_vals
hist(count[count$Freq>1, "Freq"], breaks=20, main="Number of t-values per article in Criminology", xlab="")

df.jqc <- df.jqc[!(df.jqc$title %in% no_t_vals), ]
df.jqc$title <- factor(df.jqc$title)
levels(df.jqc$title)

# bar plot of number of t-scores per article
count2 <- count[count$Freq>1, ]
count2 <- count2[order(count2$Freq, decreasing = T), ]
barplot(count2$Freq, names.arg = count2$Var1, xaxt="n", main="Number of t-values per article \n in JQC")

### grouped bar plot: IVs and controls ###
counts <- data.frame(table(df.jqc$title, df.jqc$hypo))

# side-by-side
ggplot(counts, aes(factor(Var1), Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1", name="", labels=c("Control", "IV")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

# stacked
ggplot(counts, aes(factor(Var1), Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_fill_brewer(palette = "Set1", name="", labels=c("Control", "IV")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


### get t-values ###
nrow(df.jqc)

# put test values in a new column called VALUE
df.jqc$VALUE <- NA

# option 1: coef/SE
df.jqc$se
sum((!is.na(df.jqc$se) & as.numeric(df.jqc$se)==0))  # only one!
df.jqc$VALUE <- apply(df.jqc, 1, get.tval)
which(df.jqc$VALUE==Inf) # None
sum(!is.na(df.jqc$VALUE)) # filled: 1432
nrow(df.jqc) # total: 1669


# option 2: convert CI for coef
# formula: SE = (log(odds ratio upper bound) - log(odds ratio lower bound))/3.92, where 3.92 = 1.96*2
colnames(df.jqc)
table(df.jqc$title)
df.jqc[!is.na(df.jqc$X95..CI..lb.), ]
df.jqc[!is.na(df.jqc$X99.9..CI..lb.), ]


# function to construct t-value from CI
convertCI <- function(row) {
  if (!is.na(row['X95..CI..lb.'])) {
    coef <- as.numeric(row['coef'])
    se <- (as.numeric(row['X95..CI..ub.']) - as.numeric(row["X95..CI..lb."]))/1.96/2
    if (se == 0) {
      print("se == 0")
      return(NA)
    } else {
      return(abs(coef/se))
    }
  } else {
    return(NA)
  }
}
df.jqc[(!is.na(df.jqc$X95..CI..lb.)), "VALUE"] <- apply(df.jqc[(!is.na(df.jqc$X95..CI..lb.)), ], 1, convertCI)
sum(!is.na(df.jqc$VALUE)) # filled: 1442
nrow(df.jqc) # total: 1669


# option 3: copy existing t-values, z-values
df.jqc[!is.na(df.jqc$t.value), "VALUE"] <- abs(df.jqc[!is.na(df.jqc$t.value), "t.value"])
df.jqc[!is.na(df.jqc$z.value), "VALUE"] <- abs(df.jqc[!is.na(df.jqc$z.value), "z.value"])
sum(!is.na(df.jqc$VALUE)) # filled: 1668
nrow(df.jqc) # total: was 1655, now 1669


sum(is.na(df.jqc$VALUE))
df.jqc[is.na(df.jqc$VALUE), ] # se 0
sum(!is.na(df.jqc$VALUE))



# draw histogram of IVs only
x <- df.jqc[(!is.na(df.jqc$VALUE) & df.jqc$hypo==1), "VALUE"]
length(x) # 1115
range(x)
sum(is.na(x))
quantile(x, probs=c(0.25, 0.5, 0.75, 0.95, 0.99))
as.numeric(quantile(x, probs=0.90))


which(df.jqc$VALUE==0)  # t-score is 0 because coef is 0
for (i in which(df.jqc$VALUE==0)) {
  cat(i, df.jqc[i, "coef"], df.jqc[i, "se"], df.jqc[i, "OR"], "\n")
}

total <- length(x)
total

sum(x<1.96) #544
sum(x<1.96)/total # 49%

sum(x>=1.96) # 571
sum(x>=1.96)/total # 51%

sum(x>=1.96 & x<2.766) # 123
123/total # 11%
123/571 # 22%

sum(x>=2.766) # 448
sum(x>=2.766)/total # 40%

sum(x>=2.766 & x<3.227) # 49
sum(x>=3.227) # 399
399/total # 36%


### create visualizations ###
final.jqc <- data.frame(pos=1:3, x=c("<1.96", "[1.96, 2.766)", ">=2.766"), freq=c(sum(x<1.96), sum(x>=1.96 & x<2.766), sum(x>=2.766)))
final.jqc$pct <- sapply(final.jqc$freq, function(p) paste(round(p/length(x)*100, 0), "%", sep=""))
final.jqc

# histogram
ggplot(final.jqc, aes(x = reorder(x, pos), y = freq)) +
  geom_bar(stat = "identity", fill="black") +
  #geom_text(size = 3, col="white", position=position_stack(vjust = 1)) +
  geom_text(aes(label = pct, y = freq+20), size = 10) +
  scale_y_continuous(limits = c(0,610), expand = c(0, 0)) +
  xlab("") + 
  ylab("Frequency") +
  theme_minimal() +
  theme(text = element_text(size=20))


# overlay density plot with bar plot
x2 <- x[x < as.numeric(quantile(x, probs=0.90))] # topcode at 90th percentile
length(x2) # 1003

h <- hist(x, breaks=10)
d <- density(x2)
plot(d)

# Figure 4(b)
pdf('plots/dens_plot_jqc.pdf')
par(yaxs="i", las=1)
hist(x2, prob=T, xlab="t-statistics", main="", col="grey",border="white", ylim=c(0, 0.31), xaxt='n')
box(bty="l")
lines(density(x2), lwd=2)
abline(v=1.960, lty=3, lwd=2)
abline(v=2.766, lty=3, lwd=2)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="black")
axis(side=1, at=seq(0,20, 4), labels=seq(0,20,4))
dev.off()




# get descriptive statistics
nrow(df.crim)
nrow(df.jqc)

# only IVs
iv.crim <- df.crim[df.crim$hypo==1, ]
iv.jqc <- df.jqc[df.jqc$hypo==1, ]
nrow(iv.crim)
nrow(iv.jqc)

# further remove rows with invalid SE
sum(is.na(iv.crim$VALUE) & iv.crim$hypo==1)
sum(is.na(iv.jqc$VALUE) & iv.jqc$hypo==1)

iv.crim <- df.crim[(!is.na(df.crim$VALUE) & df.crim$hypo==1), ]
iv.jqc <- df.jqc[(!is.na(df.jqc$VALUE) & df.jqc$hypo==1), ]
nrow(iv.crim)
nrow(iv.jqc)


# mean, median number of IV t-values per article
y <- table(iv.crim$title)
range(y)
mean(y)
median(y)

y <- table(iv.jqc$title)
range(y)
mean(y)
median(y)


# does every publication have some significant results?
iv.crim$sig <- iv.crim$VALUE >= 1.96
mat <- aggregate(iv.crim$sig, by=list(Category=iv.crim$title), FUN=sum)
mat['total'] <- table(iv.crim$title)
range(mat$x)

iv.jqc$sig <- iv.jqc$VALUE >= 1.96
mat <- aggregate(iv.jqc$sig, by=list(Category=iv.jqc$title), FUN=sum)
mat['total'] <- table(iv.jqc$title) # just to make sure sig < total
range(mat$x)

