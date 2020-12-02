setwd("/home/eric/Dropbox/mydocs/consulting/oas-us2020/data/")

d <- read.csv(file = "eua12-18dfed.csv", stringsAsFactors = FALSE)

d <- within(d, {
    S <- sdem+srep;
    vdemsh <- vdem*100/(vdem+vrep);
    vrepsh <- vrep*100/(vdem+vrep);
    sdemsh <- sdem*100/S;
    srepsh <- srep*100/S;
    res <- srepsh - vrepsh
    })
summary(d$res)

# read 2020 district-level preliminary data
d2 <- read.csv(file = "eua2020dfdf.csv", stringsAsFactors = FALSE)
# aggregate state votes
d2 <- within(d2, {
    vdem <- ave(vdem, as.factor(st), FUN=sum, na.rm=FALSE);
    vrep <- ave(vrep, as.factor(st), FUN=sum, na.rm=FALSE);
    voth <- ave(voth, as.factor(st), FUN=sum, na.rm=FALSE)
    });
d2 <- d2[duplicated(d2$st)==FALSE,]
d2$disn <- d2$nota <- NULL
# shares
d2 <- within(d2, {
    vrepsh <- vrep*100/(vdem+vrep);
    vdemsh <- vdem*100/(vdem+vrep);
})
# sort by state
d <-   d[order( d$st),]
d2 <- d2[order(d2$st),]
# merge d2 into d
sel <- which(d$yr==2020)
d[sel, c("vdemsh","vrepsh")] <- d2[,c("vdemsh","vrepsh")]
d[sel,]
rm(d2)
# sort by order
d <- d[order(d$ord),]

y <- 2012:2020
#png(filename = "../plots/vs12-20.png")
#pdf(file = "../plots/vs12-20.pdf")
#pdf(file = paste("../plots/vs", y, ".pdf", sep = ""))
plot(c(0,100),c(0,100), type = "n", main = "Votes and seats since 2012 (black circumferences are 2020)", #main = "Votes and seats since 2012 (excluding single-seat states)"
     xlab = "Democratic percentage of the state's two-party vote", ylab = "Democratic percentage of the state's seats" )
abline(a = 0, b = 1, lty = 2)
abline(v = seq(0,100,10), col = "gray")
abline(h = seq(0,100,10), col = "gray")
abline(v = 50, col = "gray60")
abline(h = 50, col = "gray60")
#
arrows(0,10,0,30, length = .05, col = "gray40")
text(-2,20, labels = "Democrats more overrepresented", srt = 90, cex = .75, col = "gray40")
arrows(100,90,100,70, length = .05, col = "gray40")
text(102,80, labels = "Democrats more underrepresented", srt = 90, cex = .75, col = "gray40")
arrows(40,100,20,100, length = .05, col = "gray40")
text(30,102, labels = "More Republican state", srt = 0, cex = .75, col = "gray40")
arrows(60,0,80,0, length = .05, col = "gray40")
text(70,-2, labels = "More Democratic state", srt = 0, cex = .75, col = "gray40")
#
sel <- which(d$S>=1 & d$yr %in% y)
#d$col <- rgb(0, 0, 175, alpha = 155, maxColorValue = 255)
d$col <- ifelse(d$vdemsh > 50, rgb(0, 0, 175, alpha = 125, maxColorValue = 255), rgb(175, 0, 0, alpha = 125, maxColorValue = 255))
#
points(d$vdemsh[sel], d$sdemsh[sel], pch = 19, cex = (d$S[sel]/10)^0.5, col = d$col[sel]) # rgb(0, 0, 175, alpha = 155, maxColorValue = 255)
#
# add state names
#sel <- which(d$st %in% c("WI") & d$yr==2020)
#sel <- which(d$st %in% c("AZ","MI","NV","NC","NJ","NY","PA","WI"))
#sel <- which(d$S>=1 & d$yr %in% y)
#text(d$vdemsh[sel], d$sdemsh[sel], labels = d$st[sel], cex = .67, pos = 3)
#
# mark 2020 circumference in black
sel <- which(d$S>=1 & d$yr == 2020)
points(d$vdemsh[sel], d$sdemsh[sel], pch = 1, cex = (d$S[sel]/10)^0.5, lwd = 1.5)
#
polygon(x = c(85,85,103,103), y = c(33,7,7,33), col = "white")
points(92, 20, pch = 1, cex = (1/10)^0.5)#, col = d$col[1])
points(92, 15, pch = 1, cex = (10/10)^0.5)#, col = d$col[1])
points(92, 10, pch = 1, cex = (50/10)^0.5)#, col = d$col[1])
text(92, 20, labels = "1 seat", cex = .75, pos = 4)
text(92, 15, labels = "10", cex = .75, pos = 4)
text(92, 10, labels = "50", cex = .75, pos = 4)
text(94, 31, labels = "State's", cex = .75)
text(94, 28, labels = "Congressional", cex = .75)
text(94, 25, labels = "apportionment", cex = .75)
#
# Add labels to select points
## # click in the plot where arrow should be drawn for state label, see also function identify()
## p <- locator(2)
## p
## xx
p <- list(x=c(69.23400, 67.66867), y=c(64.61253, 65.48611))
lines(p$x, p$y)
text(p$x[1], p$y[1], "NY", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(53.38503, 52.01536), y=c(25.51963, 26.61161))
lines(p$x, p$y)
text(p$x[1], p$y[1], "PA", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(54.16769, 52.21103), y=c(30.97954, 30.76114))
lines(p$x, p$y)
text(p$x[1], p$y[1], "NC", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(52.21103, 50.25437), y=c(21.15171, 22.46209))
lines(p$x, p$y)
text(p$x[1], p$y[1], "NC", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(55.537, 53.581), y=c(34, 34.911))
lines(p$x, p$y)
text(p$x[1], p$y[1], "MI", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(57.885, 55.74), y=c(38.405, 37.531))
lines(p$x, p$y)
text(p$x[1], p$y[1], "WI", cex = .67, pos = 4, offset = 0.25)
p <- list(x=c(47.39, 49.55), y=c(75.72, 75.19))
lines(p$x, p$y)
text(p$x[1], p$y[1], "NV", cex = .67, pos = 2, offset = 0.25)
#dev.off()

sel <- which(d$st=="NY"); d[sel,]
x
    

y <- 2012
#png(filename = paste("../plots/vs", y, ".png", sep = ""))
#pdf(file = paste("../plots/vs", y, ".pdf", sep = ""))
plot(c(0,100),c(-50,50), type = "n", main = y, # main = paste(y, "(excluding single-seat states)"),
     xlab = "%Republican of the two-party vote", ylab = "Republican %seats - %votes", axes = FALSE )
#
axis(1, at = seq(0,100,10), labels = TRUE)
#axis(1, at = seq(0,100,20))
axis(2, at = seq(-50,50,10), labels = c(seq(-50,0,10),paste("+", seq(10,50,10), sep = "")))
#
abline(v = seq(0,100,10), col = "gray")
abline(h = seq(-50,50,10), col = "gray")
abline(v = 50, col = "gray50")
abline(h = 0, col = "red")
#
arrows(0,10,0,30, length = .05, col = "red")
text(-2,20, labels = "More overrepresented", srt = 90, cex = .75, col = "red")
sel <- which(d$S>0 & d$yr==y)
arrows(0,-10,0,-30, length = .05, col = "red")
text(-2,-20, labels = "More underrepresented", srt = 90, cex = .75, col = "red")
sel <- which(d$S>0 & d$yr==y)
arrows(40,-50,20,-50, length = .05, col = "red")
text(30,-52, labels = "More Democratic state", srt = 0, cex = .75, col = "red")
arrows(60,-50,80,-50, length = .05, col = "red")
text(70,-52, labels = "More Republican state", srt = 0, cex = .75, col = "red")
#
sel <- which(d$S>0 & d$yr %in% y)
d$col <- rgb(0, 0, 175, alpha = 155, maxColorValue = 255)
#d$col <- ifelse(d$vdemsh > 50, rgb(0, 0, 175, alpha = 155, maxColorValue = 255), rgb(175, 0, 0, alpha = 155, maxColorValue = 255))
#
points(d$vrepsh[sel], d$res[sel], pch = 19, cex = (d$S[sel]/10)^0.5, col = d$col[sel]) # rgb(0, 0, 175, alpha = 155, maxColorValue = 255)
# 
polygon(x = c(86,86,104,104), y = c(-17,-43,-43,-17), col = "white")
points(93, -30, pch = 1, cex = (1/10)^0.5, col = d$col[1])
points(93, -35, pch = 1, cex = (10/10)^0.5, col = d$col[1])
points(93, -40, pch = 1, cex = (50/10)^0.5, col = d$col[1])
text(93, -30, labels = "1", cex = .75, pos = 4)
text(93, -35, labels = "10", cex = .75, pos = 4)
text(93, -40, labels = "50", cex = .75, pos = 4)
text(95, -19, labels = "State's", cex = .75)
text(95, -22, labels = "Congressional", cex = .75)
text(95, -25, labels = "seats", cex = .75)
#text(d$vdemsh[sel], d$sdemsh[sel], labels = d$st[sel], cex = .67, pos = 3)
#dev.off()


round(table(d$S)/4)
d$st[which(d$S==29)]
d$yr[which(d$S==29)]

###################
## end vote seat ##
###################


###############################
## turnout in pres elections ##
###############################

setwd("/home/eric/Dropbox/mydocs/consulting/oas-us2020/data/")

d <- read.csv(file = "turnout-pres-1828-on.csv", stringsAsFactors = FALSE)

# drop before 1920
d <- d[d$yr>=1920,]
R <- nrow(d)
tail(d)


#pdf("../plots/turnout20-20.pdf", height = 5, width = 7)
#png("../plots/turnout20-20.png", height = 350, width = 480)
clr <- "brown"
plot(d$yr, c(rep(45,(R-1)),75),  type="n", xlab = "", ylab = "Participation as % voting-eligible population", main = "Turnout in presidential races")
abline(h = seq(0,100,5), col = "gray")
lines(d$yr[-R], d$pct[-R], col = clr)
points(d$yr[-R], d$pct[-R], col = clr, pch = 19)
polygon(c(2016,2020,2020), c(d$pct[d$yr==2016],d$lo[d$yr==2020],d$hi[d$yr==2020]), border = "white", col = rgb(1,0,0, alpha = .25))
lines(c(2020,2020), c(d$lo[d$yr==2020],d$hi[d$yr==2020]), col = "red", lwd = 1.5)
points(c(2020,2020), c(d$lo[d$yr==2020],d$hi[d$yr==2020]), pch = "-", col = "red", lwd= 2)
#dev.off()

#pdf("../plots/mega-voters-20-20.pdf", height = 5, width = 7)
#png("../plots/mega-voters-20-20.png", height = 350, width = 480)
plot(d$yr, c(rep(0,(R-1)),250000000),  type="n", axes = FALSE, xlab = "", ylab = "Millions", main = "Voting-eligible vs. voting populations 1932-2020")
axis(1, at = seq(1920,2020,10))
axis(2, at = seq(0,250,50)*1000000, label = seq(0,250,50))
abline(h = seq(0,250,50)*1000000, col = "gray")
lines(d$yr, d$reg, col = "red")
lines(d$yr, d$votes, col = "darkgreen")
text(d$yr[d$yr==2012], d$reg[d$yr==2012], label = "Electorate", col = "red", pos = 3)
text(d$yr[d$yr==1996], d$votes[d$yr==1996], label = "Voters", col = "darkgreen", pos = 1)
## # add reg lines
## sel <- which(d$yr >= 1932  &  d$yr <= 1968)
## tmp <- lm(reg ~ yr, data = d, subset = sel)
## tmp <- predict.lm(tmp, newdata = d[sel, c("yr","reg")])
## lines(d$yr[sel], tmp, lty = 2)
## ### tmp <- round( (tmp[2] - tmp[1]) / tmp[1] *100 / 4, 1)
## ### tmp
## ### text(x = d$yr[as.integer(median(sel))], d$reg[as.integer(median(sel))], label = expression(paste(Delta, "/yr = 1.7%", sep = "")), pos = 3, srt = 25)
## #
## sel <- which(d$yr >= 1968  &  d$yr <= 2020)
## tmp <- lm(reg ~ yr, data = d, subset = sel)
## tmp <- predict.lm(tmp, newdata = d[sel, c("yr","reg")])
## lines(d$yr[sel], tmp, lty = 2)
## ## tmp <- round( (tmp[2] - tmp[1]) / tmp[1] *100 / 4, 1)
## ## tmp
## ## text(x = d$yr[as.integer(median(sel))], d$reg[as.integer(median(sel))], label = paste(tmp, "%", sep = ""), pos = 3, srt = 25)
## #
## sel <- which(d$yr >= 1932  &  d$yr <= 1992)
## tmp <- lm(votes ~ yr, data = d, subset = sel)
## tmp <- predict.lm(tmp, newdata = d[sel, c("yr","votes")])
## lines(d$yr[sel], tmp, lty = 2)
## #
## sel <- which(d$yr >= 1992  &  d$yr <= 2020)
## tmp <- lm(votes ~ yr, data = d, subset = sel)
## tmp <- predict.lm(tmp, newdata = d[sel, c("yr","votes")])
## lines(d$yr[sel], tmp, lty = 2)
#dev.off()



