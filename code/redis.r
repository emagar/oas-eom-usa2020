setwd("/home/eric/Dropbox/mydocs/consulting/oas-us2020/data/")

d <- read.csv(file = "state-branches.csv", stringsAsFactors = FALSE)
d[1,]
d <- within(d, {
    S <- cong.seats.d+cong.seats.r;
    datlarge <- as.numeric(mode=="no-redist")
    dindep <- as.numeric(mode=="indep-comm")
    })

# redistricting process type
d <- within(d, {
    red <- ifelse(datlarge==1, "at large",
           ifelse(datlarge==0 & dindep==1, "bipart comm",
           ifelse(datlarge==0 & dindep==0 & type=="dg", "div gov",
           ifelse(datlarge==0 & dindep==0 & type=="unif-d", "trifecta dem",
           ifelse(datlarge==0 & dindep==0 & type=="unif-r", "trifecta rep", NA)))))})
d$red <- as.factor(d$red)


# summary by state
table(d$red)
#
# redistricting seats at stake (prior to re-apportionment, but chg will be mg) by process type 
tapply(d$cong.seats.d,                  d$red, sum)
tapply(d$cong.seats.r,                  d$red, sum)
tapply(d$cong.seats.d + d$cong.seats.r, d$red, sum)

