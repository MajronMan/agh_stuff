import numpy as np
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg

import struct
import pyaudio
import random
from scipy.fftpack import fft

import sys
import time


class AudioStream(object):
    def __init__(self):

        # pyqtgraph stuff
        pg.setConfigOptions(antialias=True)
        self.traces = dict()
        self.app = QtGui.QApplication(sys.argv)
        self.win = pg.GraphicsWindow(title='Spectrum Analyzer')
        self.win.setWindowTitle('Spectrum Analyzer')
        self.win.setGeometry(5, 115, 1910, 1070)

        wf_xlabels = [(0, '0'), (2048, '2048'), (4096, '4096')]
        wf_xaxis = pg.AxisItem(orientation='bottom')
        wf_xaxis.setTicks([wf_xlabels])

        wf_ylabels = [(0, '0'), (127, '128'), (255, '255')]
        wf_yaxis = pg.AxisItem(orientation='left')
        wf_yaxis.setTicks([wf_ylabels])

        sp_xlabels = [
            (np.log10(10), '10'), (np.log10(100), '100'),
            (np.log10(1000), '1000'), (np.log10(22050), '22050')
        ]
        sp_xaxis = pg.AxisItem(orientation='bottom')
        sp_xaxis.setTicks([sp_xlabels])

        self.waveform = self.win.addPlot(
            title='WAVEFORM', row=1, col=1, axisItems={'bottom': wf_xaxis, 'left': wf_yaxis},
        )
        self.spectrum = self.win.addPlot(
            title='SPECTRUM', row=2, col=1, axisItems={'bottom': sp_xaxis},
        )
        self.visualization = self.win.addPlot(
            title='VISUALIZATION', row=3, col=1, axisItems={'bottom': sp_xaxis},
        )


        # pyaudio stuff
        self.FORMAT = pyaudio.paInt16
        self.CHANNELS = 1
        self.RATE = 44100
        self.CHUNK = 1024
        self.SMOOTHING = 0.75

        self.p = pyaudio.PyAudio()
        self.stream = self.p.open(
            format=self.FORMAT,
            channels=self.CHANNELS,
            rate=self.RATE,
            input=True,
            output=True,
            frames_per_buffer=self.CHUNK,
        )
        # waveform and spectrum x points
        self.x, self.fx = self.getXandFX()
        self.y, self.fy = self.getYandFY()

        self.vis2x, self.vis2y, self.max_sig, = self.getVis2Params()
        self.max_intensity = 0

    def getVis2Params(self):
        return np.arange(0, self.CHUNK), np.arange(0, self.CHUNK), 0


    def getXandFX(self):
        x = np.linspace(0, self.CHUNK / self.RATE, self.CHUNK)
        freq = np.fft.fftfreq(self.CHUNK, x[1] - x[0])
        return x, freq

    def getYandFY(self):
        y = np.zeros(self.CHUNK)
        fy = np.zeros(self.CHUNK)
        return y, fy

    def start(self):
        if (sys.flags.interactive != 1) or not hasattr(QtCore, 'PYQT_VERSION'):
            QtGui.QApplication.instance().exec_()


    def set_plotdata(self, name, data_x, data_y):
        if name in self.traces:
            if name == 'visualization1':
                pen = pg.mkPen(color=(random.randint(1, 255), random.randint(1,255), random.randint(1, 255)))
                self.traces[name].setPen(pen)
                self.traces[name].setData(data_x, data_y)
            elif name == 'visualization2':
                pen = self.visualize2()
                self.traces[name].setPen(pen)
                self.traces[name].setData(data_x, data_y)
            else:
                self.traces[name].setData(data_x, data_y)
        else:
            if name == 'waveform':
                self.traces[name] = self.waveform.plot(pen='c', width=3)
                self.waveform.setYRange(-2**15, 2**15, padding=0)
                self.waveform.setXRange(0, max(self.x), padding=0.005)
            if name == 'spectrum':
                pen = pg.mkPen(color=(0, 150, 0))
                self.traces[name] = self.spectrum.plot(pen=pen, width=3)
                # self.spectrum.setLogMode(x=True, y=True)
                # self.spectrum.setYRange(0, 6, padding=0)
                # self.spectrum.setXRange(
                #     np.log10(20), np.log10(self.RATE / 2), padding=0.005)

            if name == 'visualization1':
                self.traces[name] = self.visualization.plot(pen='c')

            if name == 'visualization2':
                self.traces[name] = self.visualization.plot(pen='c')
                self.visualization.setYRange(0, self.CHUNK)
                self.visualization.setXRange(0, self.CHUNK)

    def visualize2(self):
        new_max_sig = np.max(self.y)
        
        if new_max_sig > self.max_sig:
            self.max_sig = new_max_sig
            beginning = np.argmax(self.fy[self.CHUNK//4:self.CHUNK//2])
            beginning = 4 * beginning
            rise = self.CHUNK - self.vis2y[beginning] 
            self.vis2y[beginning] = self.CHUNK

            val = rise
            for i in range(beginning+1, self.CHUNK):
                self.vis2y[i] = min(self.CHUNK*2, self.vis2y[i] + val)
                val *= 0.9
            val = rise
            for i in range(beginning-1, 0, -1):
                self.vis2y[i] = min(self.CHUNK*2, self.vis2y[i] + val)
                val *= 0.9

        for i in range(self.CHUNK):
            self.vis2y[i] *= 0.95

        intensity = np.mean(self.fy)
        self.max_intensity = max(intensity, self.max_intensity)
        r = int(255 * intensity / self.max_intensity)
        
        self.max_intensity *= 0.999
        self.max_sig *= 0.9
        return pg.mkPen(color=(r, 30, 255-r))


    def update(self):
        wf_data = self.stream.read(self.CHUNK)
        wf_data = struct.unpack("%dh" % self.CHUNK, wf_data)
        self.y = self.y * self.SMOOTHING
        self.y += np.array(wf_data) * (1 - self.SMOOTHING)
        self.set_plotdata(name='waveform', data_x=self.x, data_y=self.y)

        transformed = (np.abs(np.fft.fft(self.y)) * np.blackman(self.CHUNK)).clip(min=1)
        self.fy = self.fy * self.SMOOTHING
        self.fy += (1 - self.SMOOTHING) * transformed
        self.fy = np.where(self.fy > 11, self.fy, 1)
        self.set_plotdata(name='spectrum', data_x=self.fx, data_y=self.fy)

        self.set_plotdata("visualization2", self.vis2x, self.vis2y)

    def animation(self):
        timer = QtCore.QTimer()
        timer.timeout.connect(self.update)
        timer.start(20)
        self.start()


if __name__ == '__main__':

    audio_app = AudioStream()
    audio_app.animation()