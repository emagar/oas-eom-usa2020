setwd("/home/eric/Dropbox/mydocs/consulting/oas-us2020/data/")

d <- read.csv(file = "eua12-18dfed.csv", stringsAsFactors = FALSE)

d <- within(d, {
    S <- sdem+srep;
    vdemsh <- vdem*100/(vdem+vrep);
    sdemsh <- sdem*100/S;
    res <- sdemsh - vdemsh
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

y <- 2018
plot(c(0,100),c(-50,50), type = "n", main = paste(y, "(excluding single-seat states)"),
     xlab = "Democratic % of two-party vote", ylab = "Democratic %seats - %votes" )
abline(v = seq(0,100,10), col = "gray")
abline(h = seq(-50,50,10), col = "gray")
abline(v = 50, col = "gray60")
abline(h = 0, col = "gray60")
sel <- which(d$S>1 & d$yr==y)
points(d$vdemsh[sel], d$res[sel], pch = 19, cex = (d$S[sel]/25)^0.5)
#text(d$vdemsh[sel], d$sdemsh[sel], labels = d$st[sel], cex = .67, pos = 3)


round(table(d$S)/4)
d$st[which(d$S==29)]
d$yr[which(d$S==29)]
