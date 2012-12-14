## TryR Vector

## 2.1 Vectors
c(1, 5, 10)
c('a', 'b', 'k')
c(1, T, 'true')

## 2.2 Sequence Vectors
1:10
seq(1,10)
seq(1,10,0.5)
seq(1,10,5)
10:1

## 2.3 Vector Access
arr <- c(0, 5, 10, 15, 20)
arr[3]
arr[1] <- 1
arr[-1]
arr[-4]
arr[c(2,4)]
arr[2:4]
arr[4:7] <- c(11,22,33,44)
arr

## 2.4 Vector Names {kind of key->val}
abc <- 1:3
names(abc) <- c('F', 'S', 'T')
abc
abbrev <- c('United States', 'United Kingdom')
names(abbrev) <- c('us', 'uk')
abbrev
abbrev['us']

## 2.5 Plotting one Vector
# barplot(arr) ## creates Bar Chart for Vector, here it saves as Rplot.pdf
# barplot(abc) ## creates Bar Chart with key-names for Vector, here it saves as Rplot.pdf

## 2.6 Vector Math
arr
arr + 1
arr / 2
arr2 <- arr * 2
c(1,2,3) + c(1,4,6)
c(1,2,3) - c(1,4,6)
c(1,2,3) == c(1,4,6)
c(1,2,3) < c(1,4,6)
sin(arr)

## 2.7 Scatter Plots
angles <- seq(0,10,0.1)
sinAngles <- sin(angles)
# plot(angles, sinAngles) ## creates X-Y Plot from Vector X,Y, here it saves as Rplot.pdf

## 2.8 NA Values
arr3 <- c(1, 2, NA, 4, 5)
sum(arr3)
sum(arr3, na.rm = TRUE)
