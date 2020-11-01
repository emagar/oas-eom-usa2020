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

y <- 2018
plot(c(0,100),c(0,100), type = "n", main = paste(y, "(excluding single-seat states)"),
     xlab = "Democratic % of two-party vote", ylab = "Democratic % seats" )
abline(a = 0, b = 1, lty = 2)
abline(v = seq(0,100,10), col = "gray")
abline(h = seq(0,100,10), col = "gray")
abline(v = 50, col = "gray60")
abline(h = 50, col = "gray60")
sel <- which(d$S>1 & d$yr==y)
points(d$vdemsh[sel], d$sdemsh[sel], pch = 19, cex = (d$S[sel]/25)^0.5)
text(d$vdemsh[sel], d$sdemsh[sel], labels = d$st[sel], cex = .67, pos = 3)

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
sel <- which(d$S>0 & d$yr==y)
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
