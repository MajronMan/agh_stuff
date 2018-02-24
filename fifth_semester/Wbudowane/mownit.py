from typing import Callable, Tuple, ClassVar, Dict
import matplotlib.pyplot as plt
import numpy as np


class SparseMatrix:
    values: ClassVar[Dict[Tuple[int, int], float]]
    shape: ClassVar[Tuple[int, int]]

    def __init__(self, values: Dict[Tuple[int, int], float], shape: Tuple[int, int]):
        self.values = values.copy()
        self.shape = shape

    def get(self, at):
        return self.values.get(at) or 0

    def set(self, key, val):
        self.values[key] = val

    def __str__(self):
        s = "--- Sparse Matrix ---\n"
        for i in range(self.shape[0]):
            for j in range(self.shape[1]):
                a = self.values.get((i, j))
                if a is None:
                    a = 0
                s += "\t%f" % a
            s += "\n"
        s += "---------------------"
        return s

    def __add__(self, other):
        if self.shape != other.shape:
            raise (ArithmeticError("Shapes do not match"))

        res = SparseMatrix({}, self.shape)

        for i in range(self.shape[0]):
            for j in range(self.shape[1]):
                a = self.values.get((i, j))
                b = other.values.get((i, j))
                if a or b:
                    a = a or 0
                    b = b or 0
                    res.set((i, j), a + b)
        return res

    def __mul__(self, other):
        if isinstance(other, SparseMatrix):
            n1, k1 = self.shape
            n2, k2 = other.shape
            if k1 != n2:
                raise (ArithmeticError("Wrong shapes"))
            res = SparseMatrix({}, (n1, k2))

            for i in range(n1):
                for j in range(k2):
                    el = 0
                    for k in range(k1):
                        el += self.get((i, k)) * other.get((k, j))
                    if el != 0:
                        res.set((i, j), el)
            return res
        if isinstance(other, (int, float, complex)):
            res = SparseMatrix(self.values, self.shape)
            for k in res.values:
                res.values[k] = res.values[k] * other
            return res

        raise ArithmeticError("Cannot multiply by %s" % type(other))

    def __rmul__(self, other):
        if isinstance(other, (int, float, complex)):
            return self * other

        raise ArithmeticError("Cannot multiply by %s" % type(other))


def newton(f: Callable[[float], float], df: Callable[[float], float], x0: float, e: float) -> Tuple[float, float]:
    xn = x0
    delta = abs(f(x0))
    while delta > e:
        xn = xn - f(xn) / df(xn)
        delta = abs(f(xn))
    return (xn, f(xn))


def jacobi(A, b, n, x=None):
    if x is None:
        x = np.zeros(A.shape[0])
    D = np.diag(A)
    R = A - np.diagflat(D)

    for i in range(n):
        x = (b - np.dot(R, x)) / D
    return x


def jacobi_steps_error(A, b, n):
    x = None
    res = []
    r = np.linalg.solve(A, b)
    for i in range(n):
        x = jacobi(A, b, 1, x)
        err = square_error(x, r)
        res += [err]
    return res


def square_error(m1, m2):
    res = 0
    for (a, b) in zip(m1, m2):
        res += (a - b) ** 2
    return np.sqrt(res)


if __name__ == "__main__":
    a = SparseMatrix({
        (0, 0): 1,
        (0, 1): 2,
        (1, 0): 3,
        (1, 1): 4,
        (2, 0): 4,
        (2, 1): 5,
        (3, 0): 5,
        (3, 1): 1
    }, (4, 2))
    b = SparseMatrix({
        (0, 0): 1,
        (0, 1): 2,
        (0, 2): 3,
        (1, 0): 4,
        (1, 1): 5,
        (1, 2): 6
    }, (2, 3))

    m1 = np.matrix([[1, 2],
                    [3, 4],
                    [4, 5],
                    [5, 1]])

    m2 = np.matrix([[1, 2, 3],
                    [4, 5, 6]])
    print(a * b)
    print(m1 * m2)

    print(2 * SparseMatrix({(1, 1): 7}, (3, 3)))

    A2 = np.array([[15, -7.3], [7.1, 12.0]])
    b2 = np.array([11.0, -133.0])
    A3 = np.array([[4.0, -2.0, 1.0], [1.0, -3.0, 2.0], [-1.0, 2.0, 6.0]])
    b3 = np.array([1.0, -12.5, 31.0])
    A4 = np.array([[-44.0, -2.0, 11.0, 1], [3.35, 123.0, -3.0, 2.0], [-1, -1.3, 23.0, 6.0], [5.4, -4.2, 1, -78]])
    b4 = np.array([110.0, -123.5, 323.1, 51])

    n = 20
    t = np.arange(0, n, 1)
    plt.plot(t, jacobi_steps_error(A2, b2, n), 'rs', t, jacobi_steps_error(A3, b3, n), 'g^', t,
             jacobi_steps_error(A4, b4, n), 'bd')
    plt.show()
