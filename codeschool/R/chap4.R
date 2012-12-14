## TryR Summary Statistics

# 4.1 Mean
marks <- c(70, 85, 64, 99, 91)
mean(marks)
# barplot(marks)  ## barplot the vector
# abline(h = mean(marks))  ## draw a line at mean on barplot


# 4.2 Median
marks[6] <- -1000
mean(marks)
median(marks)
# barplot(marks)  ## barplot the vector
# abline(h = median(marks))  ## draw a line at mean on barplot


# Standard Deviation
marks <- c(50, 25, 45, 60, 88)
mean(marks)
median(marks)
sd(marks)
barplot(marks)  ## barplot the vector
sdAboveMean <- mean(marks) + sd(marks)
sdAboveMean
abline(h = mean(marks))
abline(h = sdAboveMean )  ## draw a line at mean on barplot
