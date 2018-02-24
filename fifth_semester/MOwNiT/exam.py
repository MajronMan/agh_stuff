import cmath
import numpy as np

def lag(x0, n, f, fp, fpp, eps):
    while abs(f(x0)) > eps:
        G = fp(x0)/f(x0)
        H = G**2 - fpp(x0)/f(x0)
        st = cmath.sqrt((n-1)*(n*H-G**2))
        b = G + st if abs(G+st) > abs(G-st) else G - st
        a = n/b
        x0 = x0-a
    return x0

def sr(A, b, n=25):
    L = np.tril(A)
    U = A - L
    x = np.zeros(b.shape)
    for i in range(n):
        x = np.dot(np.linalg.inv(L), b-np.dot(U, x))
    return x

# print(lag(0.1, 5, lambda x: x**2 + 1, lambda x: 2*x, lambda x: 2, 0.000001))

A = np.array([[4.0, -2.0, 1.0], [1.0, -3.0, 2.0], [-1.0, 2.0, 6.0]])
b = np.array([1.0, 2.0, 3.0])

print(np.linalg.solve(A, b))
print(sr(A, b))
