## TryR Matrices

## 3.1 Matrices
matrix(0, 3, 4)
a <- 1:12
print(a)
matrix(a, 3, 4)
dim(a) <- c(2,6)
print(a)

## 3.2 Matrix Access
a[1, 4]
a[2, 1]
a[1,]
a[,1]
a[,2:3]
a[1,4] <- 100
print(a)

## 3.3 Matrix Plotting
# contour(a)  ## 2D Contour Map of Matrix
# persp(a)  ## 3D Perspective Map of Matrix
# persp(a, expand = 0.5)  ## 3D Perspective Map of Matrix with % coverage
# image(a)  ## Image Map of Matrix
# print(volcano)  ## built-in present matrix
