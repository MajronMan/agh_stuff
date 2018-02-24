from typing import Optional, Tuple

import numpy as np
import math
import matplotlib.pyplot as plt
from scipy.linalg import lu


def agh_superfast_matrix_multiply(a: np.matrix, b: np.matrix) -> np.matrix:
    """Perform totally ordinary multiplication of matrices.

    :param a: matrix with dimensions _ by _
    :param b: matrix with dimensions _ by _
    :return:  matrix with dimensions _ by _
    """

    (r1, c1) = a.shape
    (r2, c2) = b.shape
    if c1 != r2:
        raise ValueError('Dimensions do not match')
    res = np.asmatrix(np.zeros([r1, c2]))
    for i in range(0, r1):
        for j in range(0, c2):
            for k in range(0, c1):
                res[i, j] += a[i, k] * b[k, j]
    return res


def swapRows(a: np.matrix, i: int, j: int) -> np.matrix:
    r = np.matrix(a)
    r[i, :], r[j, :] = a[j, :], a[i, :]
    return r


def findMaxRow(a: np.matrix, pos: int) -> int:
    (n, _) = a.shape
    max_element = abs(a[pos, pos])
    max_row = pos

    for i in range(pos + 1, n):
        potential = abs(a[i, pos])
        if potential > max_element:
            max_element = potential
            max_row = i

    return max_row


def zero_column(a: np.matrix, pos: int, extended: np.matrix = None) -> np.matrix:
    (n, _) = a.shape
    res = np.matrix(a)

    for i in range(pos + 1, n):
        if res[pos, pos] == 0:
            maxRow = findMaxRow(res, pos)
            res = swapRows(res, pos, maxRow)
            if extended is not None:
                extended = swapRows(extended, pos, maxRow)
            if res[pos, pos] == 0:
                continue

        c = - res[i, pos] / res[pos, pos]
        for j in range(pos, n):
            res[i, j] += c * res[pos, j]

        if extended is not None:
            for j in range(extended.shape[1]):
                extended[i, j] += c * extended[pos, j]

    return res


def agh_superfast_gauss(a: np.matrix, pivot: bool = False, extended: np.matrix = None) -> np.matrix:
    (n1, n2) = a.shape
    if n1 != n2:
        raise ValueError('Only square matrices here')
    n = n1
    res = np.matrix(a, float)

    for i in range(n):
        if pivot:
            maxRow = findMaxRow(res, i)
            res = swapRows(res, i, maxRow)
            if extended is not None:
                extended = swapRows(extended, i, maxRow)
        res = zero_column(res, i, extended)

    return res


def agh_superfast_solve(a: np.matrix, b: np.matrix, pivot: bool = False) -> np.matrix:
    (n1, n2) = a.shape
    (n3, n4) = b.shape
    if n4 != 1 or n1 != n2 or n2 != n3:
        raise ValueError('Matrices dimensions do not match pattern (n, n), (n, 1)')
    n = n1
    y = np.matrix(b, dtype=float)
    g = agh_superfast_gauss(a, pivot, y)
    x = np.asmatrix(np.matrix(np.zeros(n)).transpose())

    for i in range(n - 1, -1, -1):
        x[i, 0] = y[i, 0] / g[i, i]
        for j in range(i - 1, -1, -1):
            y[j, 0] -= g[j, i] * x[i, 0]

    return x


def test_gauss():
    matrices = [
        np.matrix([
            [3, 2, 7, 11],
            [17, 3, 13, 5],
            [7, 23, 11, 17],
            [11, 7, 5, 2]
        ]),
        np.matrix([
            [0, 1, 2],
            [3, 0, 4],
            [5, 6, 7]
        ]),
        np.matrix([[0.0001, -5.0300, 5.8090, 7.8320],
                   [2.2660, 1.9950, 1.2120, 8.0080],
                   [8.8500, 5.6810, 4.5520, 1.3020],
                   [6.7750, -2.253, 2.9080, 3.9700]])
    ]

    for a in matrices:
        _, u = lu(a, permute_l=True)
        a1 = agh_superfast_gauss(a, True)
        print(list(map(np.linalg.det, [a, u, a1])))
        print(a1)
        print("Jest ok" if np.allclose(a1, u) else "Cos nie tak")
        print("-" * 100)


def pivot_matrix(a: np.matrix) -> np.matrix:
    r = np.asmatrix(np.zeros(a.shape))
    for i in range(r.shape[0]):
        maxRow = findMaxRow(a, i)
        r = swapRows(r, maxRow, i)
    return r


def so_lu(a: np.matrix) -> Optional[Tuple[np.matrix, np.matrix]]:
    (n1, n2) = a.shape
    if n1 != n2:
        raise ValueError('Only square matrices here')
    n = n1

    L = np.asmatrix(np.zeros((n, n)))
    U = np.matrix(a)
    np.fill_diagonal(L, 1)  # fill the diagonal of L with 1

    for i in range(n - 1):
        for j in range(i + 1, n):
            L[j, i] = U[j, i] / U[i, i]
            U[j, i] = 0
            for k in range(i + 1, n):
                U[j, k] = U[j, k] - L[j, i] * U[i, k]

    return (L, U)


def agh_superfast_lu(a: np.matrix) -> Optional[Tuple[np.matrix, np.matrix]]:
    (n1, n2) = a.shape
    if n1 != n2:
        raise ValueError('Only square matrices here')
    n = n1
    L = np.asmatrix(np.zeros((n, n)))
    U = np.asmatrix(np.zeros((n, n)))
    P = pivot_matrix(a)
    PA = np.dot(P, a)

    for j in range(n):
        # All diagonal entries of L are set to unity
        L[j, j] = 1.0

        # LaTeX: u_{ij} = a_{ij} - \sum_{k=1}^{i-1} u_{kj} l_{ik}
        for i in range(j + 1):
            s1 = sum(U[k, j] * L[i, k] for k in range(i))
            U[i, j] = P[i, j] - s1

        # LaTeX: l_{ij} = \frac{1}{u_{jj}} (a_{ij} - \sum_{k=1}^{j-1} u_{kj} l_{ik} )
        for i in range(j, n):
            s2 = sum(U[k, j] * L[i, k] for k in range(j))
            L[i, j] = (P[i, j] - s2) / U[j, j]

    return (L, U)


# test_gauss()

A = np.matrix([[0.0001, -5.0300, 5.8090, 7.8320],
               [2.2660, 1.9950, 1.2120, 8.0080],
               [8.8500, 5.6810, 4.5520, 1.3020],
               [6.7750, -2.253, 2.9080, 3.9700]])
b = np.asmatrix(np.matrix([9.5740, 7.2190, 5.7300, 6.2910]).transpose())

(l, u) = so_lu(A)
print(l)
print(u)
(l, u) = lu(A, permute_l=True)
print(l)
print(u)
