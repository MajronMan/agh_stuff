import numpy as np

# Ax = b
# A = D + R
# (D+R)x = b
# D x = b - R x
# x = D ** (-1) (b - R x)

# warunek wystarczajacy to ze A jest przekatniowo dominujaca
# promien spektralny to najwieksza wartosc wlasna

def jacobi(A, b, N=25, x=None):
    D = np.diag(A)
    R = A - np.diagflat(D)
    Di = 1 / D

    if x is None:
        xk = np.zeros(len(A[0]))
    else:
        xk = x
    for i in range(N):
        xk = Di * b - Di * np.dot(R, xk)
    return xk


def gauss_seidel(A, b):
    pass

A = np.array([[2.0,1.0],[5.0,7.0]])
b = np.array([11.0,13.0])
guess = np.array([1.0,1.0])

sol = jacobi(A, b, 25, guess)

print(sol)
