from typing import List, Tuple, Optional
import numpy as np
from matplotlib import pyplot as plt


def rmse(x: List[float], y: List[float]) -> float:
    r = 0
    for (a, b) in zip(x, y):
        r += (a - b) ** 2
    return r


def lin_reg(data: List[Tuple[float, float]]) -> Tuple[float, float]:
    d = np.array(data)
    m = d.shape[0]
    p = np.sum(d[:, 0])
    q = np.sum(d[:, 1])
    r = np.sum(d[:, 0] * d[:, 1])
    s = np.sum(d[:, 0] ** 2)
    d = (m + 1) * s - p ** 2
    a = ((m + 1) * r - p * q) / d
    b = (s * q - p * r) / d
    return (a, b)


class LinearRegressor():
    def __init__(self):
        self._coeffs = None  # type: Optional[Tuple[float, float]]

    def fit(self, data: List[Tuple[float, float]]) -> None:
        self._coeffs = lin_reg(data)

    def predict(self, x: List[float]) -> List[float]:
        return [self.coeffs[0] * xi + self.coeffs[1] for xi in x]

    @property
    def coeffs(self) -> Tuple[float, float]:
        if self._coeffs is None:
            raise Exception('You need to call `fit` on the model first.')

        return self._coeffs


X = [1, 2, 3, 4, 5, 6, 7, 8, 9]
Y = [0.5, 3, 2.6, 4.4, 5.3, 6.9, 7.01, 8.12, 10]
agressor = LinearRegressor()
agressor.fit(list(zip(X, Y)))
print(rmse(agressor.predict(X), Y))


def plot_data(x: List[float], y: List[float]) -> None:
    lr = LinearRegressor()
    lr.fit(list(zip(X, Y)))
    plt.scatter(x, y)
    plt.plot(x, agressor.predict(x))
    plt.show()





y = [392.5, 46.2, 15.7, 422.2, 119.4, 170.9, 56.9, 77.5, 214, 65.3, 20.9, 248.1, 23.5, 39.6, 48.8, 6.6, 134.9, 50.9,
     4.4, 113, 14.8, 48.7, 52.1, 13.2, 103.9, 77.5, 11.8, 98.1, 27.9, 38.1, 0, 69.2, 14.6, 40.3, 161.5, 57.2, 217.6,
     58.1, 12.6, 59.6, 89.9, 202.4, 181.3, 152.8, 162.8, 73.4, 21.3, 92.6, 76.1, 39.9, 142.1, 93, 31.9, 32.1, 55.6,
     133.3, 194.5, 137.9, 87.4, 209.8, 95.5, 244.6, 187.5]
x = [108, 19, 13, 124, 40, 57, 23, 14, 45, 10, 5, 48, 11, 23,
     7, 2, 24, 6, 3, 23, 6, 9, 9, 3, 29,
     7, 4, 20, 7, 4, 0, 25, 6, 5, 22, 11, 61, 12, 4, 16, 13, 60, 41,
     37, 55, 41, 11, 27, 8, 3, 17, 13,
     13, 15, 8, 29, 30, 24, 9, 31, 14, 53, 26]

plot_data(x, y) # do bani

