import numpy as np
import cmath
def w(k, n):
    return cmath.exp(-1j*2 * cmath.pi * k / n)

def fourierowi_to_wisi(x: np.ndarray) -> np.ndarray:
    N = len(x)
    n = np.arange(N)
    k = n.reshape((N, 1))
    M = np.exp(-1j * 2 * cmath.pi * k * n / N)
    return np.dot(M, x)

def bo_nie_zyje(x: np.ndarray) -> np.ndarray:
    N = len(x)
    if N == 2:
        return fourierowi_to_wisi(x)
    x_e = bo_nie_zyje(x[::2])
    x_o = bo_nie_zyje(x[1::2])
    factor = np.exp(-2j * cmath.pi * np.arange(N)/N)
    return np.concatenate((x_e + factor[:N//2] * x_o , x_e + factor[N//2:] * x_o )  )


print(np.allclose(bo_nie_zyje(np.array([1,2,3,4, 5, 6, 7, 8]).transpose()), np.fft.fft([1,2,3,4, 5, 6, 7, 8])))