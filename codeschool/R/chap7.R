## TryR Real-World Data

# 7.1 Some Real World Data
piracy <- read.csv("reqd_files/pirates.csv")
print(piracy)
gdp <- read.table("reqd_files/gdp.txt", sep=" ", header=TRUE)
print(gdp)
countries <- merge(x = gdp, y = piracy)
print(countries)
# plot(countries$GDP, countries$Pirates) ## pdf graph
cor.test(countries$GDP, countries$Pirates)
linearModel <- lm(countries$Pirates ~ countries$GDP)
# abline(linearModel) ## more reasonable prediction of pirates

# 7.2 ggplot2
## http://stackoverflow.com/questions/5522830/installing-ggplot-package-ggplot-is-not-available-and-subscript-out-of-boun
## http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile
options(repos=c("http://cran.cnr.Berkeley.edu","http://cran.stat.ucla.edu"))
installed.packages()
# install.packages("ggplot2", dependencies = TRUE)
# help(package = "ggplot2")
weights <- c(300, 200, 100, 250, 150)
prices <- c(9000, 5000, 12000, 7500, 18000)
chests <- c('gold', 'silver', 'gems', 'gold', 'gems')
types <- factor(chests)
library(ggplot2)
# qplot(weights, prices, color = types) ## colored graph
