import numpy as np
import matplotlib.pyplot as plt
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg
import time

RATE = 44100
CHUNK = 1024


def f(x):
    return 10 * np.sin(2 * np.pi * 1000 * x) + 5 * np.sin(2 * np.pi * 3000 * x)


x = np.linspace(0, CHUNK / RATE, CHUNK)
y = f(x)

freqs = np.fft.fftfreq(y.size, x[1] - x[0])

app = QtGui.QApplication([])

w = QtGui.QMainWindow()
cw = pg.GraphicsLayoutWidget()

w.show()
w.resize(1200, 800)
w.setCentralWidget(cw)
w.setWindowTitle('Fourier')

p = cw.addPlot(row=0, col=0)
p2 = cw.addPlot(row=1, col=0)
p2.setLogMode(y=True)
fi = 0
lim = 22


while True:
    fi += 0.00001
    y = f(x + fi)
    FFT = (np.abs(np.fft.fft(y)) * np.blackman(CHUNK)).clip(min=1)
    FFT = np.where(FFT > 11, FFT, 1)
    p.plot(x, y, clear=True)
    p2.plot(freqs, FFT, clear=True)
    pg.QtGui.QApplication.processEvents()
    time.sleep(0.01)
