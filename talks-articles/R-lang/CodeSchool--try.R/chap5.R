## TryR Factors

# 5.1 Creating Factors
treasure <- c('gold', 'silver', 'gems', 'gold', 'gems')
uniqTypes <- factor(treasure)
print(uniqTypes)
as.integer(uniqTypes)
levels(uniqTypes)

# 5.2 Plots with Factors
weights <- c(300, 200, 100, 250, 150)
prices <- c(9000, 5000, 12000, 7500, 18000)
# plot(weights, prices) ## just plot the factors
# plot(weights, prices, pch=as.integer(uniqTypes))  ## better with symbols
# legend("topright", c("gems", "gold­", "silver"), pch=1:3)  ## add a legend to plot
# legend("topright", levels(uniqTypes), pch=1:length(levels(uniqTypes))) ## better, no hard-code
