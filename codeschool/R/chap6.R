## TryR Data Frames

# 6.1 Data Frames
student <- c('alice', 'bob', 'eve')
class <- c(5, 10, 1)
rollNumber <- c(11, 22, 12)
yearbook <- data.frame(student, class, rollNumber)
print(yearbook)

# 6.2 Data Frame Access
yearbook[['class']]
yearbook[[2]]
yearbook$rollNumber

# 6.3 Loading Data Frames
list.files('reqd_files')
read.csv('reqd_files/targets.csv')
read.table("reqd_files/infantry.txt", sep="\t")
read.table("reqd_files/infantry.txt", sep="\t", header=T)

# Merging Data Frames
targets <- read.csv('reqd_files/targets.csv')
infantry <- read.table("reqd_files/infantry.txt", sep="\t", header=T)
merge(x = targets, y = infantry) 
