#!/usr/bin/env python3
## source: https://machinelearningmastery.com/singular-value-decomposition-for-machine-learning/

from numpy import array
from numpy import diag
from numpy import dot
from numpy import zeros
from scipy.linalg import svd

# Singular-value decomposition
def convert_to_svd(matrix):
    U, s, VT = svd(matrix)
    return U,s,VT


# Reconstruct SVD
def reconstruct_source_from_svd(U,s,VT, row_size, col_size, truncate=False, n_elements=2):
    """
    ## truncate,n_elements flow is also available via TruncatedSVD
    from sklearn.decomposition import TruncatedSVD

    # svd
    svd = TruncatedSVD(n_components=2)
    svd.fit(A)
    result = svd.transform(A)
    print(result)
    """
    # create m x n Sigma matrix
    Sigma = zeros((row_size, col_size))
    if row_size == col_size:
        Sigma = diag(s)
    elif row_size < col_size:
        # populate Sigma with m x m diagonal matrix
        Sigma[:row_size, :row_size] = diag(s)
    elif row_size > col_size:
        # populate Sigma with n x n diagonal matrix
        Sigma[:col_size, :col_size] = diag(s)
    if truncate == True:
        Sigma = Sigma[:, :n_elements]
        VT = VT[:n_elements, :]
        # transform
        T = U.dot(Sigma)
        print(T)
        T = A.dot(VT.T)
        print(T)
    # reconstruct matrix
    return U.dot(Sigma.dot(VT))

if __name__ == "__main__":
    # define main data matrix
    A = array([
        [1, 200, 3, 4],
        [3, 400, 500, 6],
        [500, 6, 7, 800],
    ])
    print("Main Matrix:\n%s" % A)
    print("rows: %s, cols: %s" % (A.shape[0], A.shape[1]))
    U,s,VT = convert_to_svd(A)
    print("\nSVD:\n%s" % s)
    print("%s singular-values" % len(s))
    ReA = reconstruct_source_from_svd(U,s,VT, 3, 4)
    print("\nReconstructed Matrix:\n %s" % A)
