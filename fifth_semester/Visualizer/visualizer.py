import struct

import numpy as np
import matplotlib.pyplot as plt
import pyaudio
import threading
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg

class AudioRecorder():
    FORMAT = pyaudio.paInt16
    RATE = 44100
    CHUNK = 1024
    CHANNELS = 1

    def __init__(self):
        self.stream = self.init_pyaudio()

    def init_pyaudio(self):
        p = pyaudio.PyAudio()
        return p.open(format=self.FORMAT,
                      channels=self.CHANNELS,
                      rate=self.RATE,
                      input=True,
                      frames_per_buffer=self.CHUNK)

    def parameters(self):
        x = np.linspace(0, self.CHUNK / self.RATE, self.CHUNK)
        frequencies = np.fft.fftfreq(self.CHUNK, x[1] - x[0])
        return x, frequencies

    def read(self):
        raw = self.stream.read()
        return np.array(struct.unpack("%dh" % self.CHUNK, raw))

class AudioTransformer():
    SMOOTHING = 0.75
    WIDTH = 1200
    HEIGHT = 800

    def __init__(self, recorder = AudioRecorder()):
        self.recorder = recorder
        self.x, self.frequencies = recorder.parameters()
        self.y = np.zeros(x.shape)
        self.FFT = np.abs(np.fft.fft(y)).clip(min=1)

    def transform_next(self):
        new_data = self.recorder.read()
        self.y = self.y * self.SMOOTHING
        self.y += (1 - self.SMOOTHING) * new_data

        transformed = (np.abs(np.fft.fft(y)) * np.blackman(len(y))).clip(min=1)
        self.FFT = self.FFT * self.SMOOTHING 
        self.FFT += (1 - self.SMOOTHING) * transformed
        self.FFT = np.where(self.FFT > 11, self.FFT, 1)
        
        return (self.x, self.y, self.frequencies, self.FFT)


class Plotter():
    WIDTH = 800
    HEIGHT = 600
    def __init__(self, transformer = AudioTransformer(), visualization=None, show_signal=False, show_transform=False, show_visualization=True):
        self.running = True
        self.transformer = transformer
        self.visualizer = Visualizer(visualization)
        self.show_visualization = show_visualization
        self.show_transform = show_transform
        self.show_signal = show_signal

    def stop(self):
        self.running = False
                
    def init_widget(self):
        app = QtGui.QApplication([])
        app.aboutToQuit.connect(self.stop)
        w = QtGui.QMainWindow()
        widget = pg.GraphicsLayoutWidget()
        w.show()
        w.resize(self.WIDTH, self.HEIGHT)
        w.setCentralWidget(widget)
        w.setWindowTitle('VISUALIZER')
        return widget

    def plot(self):
        widget = self.init_widget()

        if self.show_signal:
            signal_plot = widget.addPlot(row=0, col=0)
            signal_plot.setRange(yRange=[-2 ** 15, 2 ** 15])
        if self.show_transform:
            transform_plot = widget.addPlot(row=1, col=0)
            transform_plot.setRange(yRange=[0, 6])
        if self.show_visualization:
            visualization_plot = widget.addPlot(row=2, col=0)
            visualization_plot.setRange(yRange=[0, 10000])

        while self.running:
            x, y, frequencies, FFT = self.transformer.transform_next()

            if self.show_signal:
                signal_plot.plot(x, y, clear=True)
            if self.show_transform:
                transform_plot.plot(frequencies, FFT, clear=True)
            if self.show_visualization:
                self.visualizer.visualize(x, y, frequencies, FFT, visualization_plot)

            pg.QtGui.QApplication.processEvents()


class Visualizer():
    def __init__(self, visualization):
        self.visualization = visualizations[visualization]

    def prepareFFT(self, xRange, yRange, binNumber, lines):
        FFT = np.zeros(self.CHUNK)
        mem = np.zeros((self.CHUNK, self.CHUNK))
        vx = np.arange(binNumber)
        vy = np.arange(0, yRange)
        q = np.zeros((lines, binNumber))
        return FFT, mem, vx, vy, q

    def signal_power(self, fft):
        p = np.sum(fft)
        return p / (self.CHUNK * 20000)

    def calculate_mean(self, binNumber, fft):
        mean = np.zeros(binNumber)
        middle = fft.shape[0] // 2
        delta = middle // binNumber
        
        for i in range(binNumber):
            mean[i] = np.mean(fft[middle + delta * i: middle + delta * (i + 1)])
        return mean 

    def visualize(x, y, frequencies, FFT, plot):
        pass

if __name__ == "__main__":
    try:
        vs = Visualizer()
        x, y, xf = vs.initialize_vars()
        runFlag = [True]
        plotting = threading.Thread(target=vs.plot, args=(
            x, y, xf, runFlag, False, False, True))

        plotting.start()
        plotting.join(1000)
        runFlag[0] = False
    except ValueError:
        pass
