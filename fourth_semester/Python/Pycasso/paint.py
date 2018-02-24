import tkinter as tk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
from PIL import ImageDraw


class Paint(object):
    DEFAULT_PEN_SIZE = 25
    DEFAULT_COLOR = 'black'

    def __init__(self, img_ref, data_ref):
        self.root = tk.Tk()
        self.pen_button = tk.Button(self.root, text='pen', command=self.use_pen)
        self.pen_button.grid(row=0, column=0)

        self.eraser_button = tk.Button(self.root, text='eraser', command=self.use_eraser)
        self.eraser_button.grid(row=0, column=1)

        self.clear_button = tk.Button(self.root, text='clear', command=self.clear)
        self.clear_button.grid(row=0, column=2)

        self.canvas = tk.Canvas(self.root, bg='white', width=600, height=600)
        self.canvas.grid(row=1, columnspan=5)

        self.data = data_ref
        self.ind = [x for x in range(0, 10)]
        self.width = .5
        figure = Figure(figsize=(6, 6), dpi=100)
        ax = figure.add_subplot(111)

        self.subplot = ax.bar(self.ind, self.data, self.width)
        ax.set_xticks([x for x in range(0, 10)])
        ax.set_yticks([x / 10 for x in range(0, 10)])

        self.results = FigureCanvasTkAgg(figure, master=self.root)
        self.results.show()
        self.results.get_tk_widget().grid(row=1, column=6)

        self.image = img_ref
        self.draw = ImageDraw.Draw(self.image)
        self.clear()
        self.update_plot()

        self.old_x = None
        self.old_y = None
        self.line_width = self.DEFAULT_PEN_SIZE
        self.color = self.DEFAULT_COLOR
        self.eraser_on = False
        self.active_button = self.pen_button
        self.canvas.bind('<B1-Motion>', self.paint)
        self.canvas.bind('<ButtonRelease-1>', self.reset)

        self.tick()
        self.root.mainloop()

    def tick(self):
        self.update_plot()
        self.root.after(100, self.tick)

    def update_plot(self):
        for rect, h in zip(self.subplot, self.data):
            rect.set_height(h)
        self.results.draw()

    def use_pen(self):
        self.activate_button(self.pen_button)

    def use_eraser(self):
        self.activate_button(self.eraser_button, eraser_mode=True)

    def clear(self):
        self.canvas.delete("all")
        self.draw.rectangle([0, 0, 600, 600], fill='white')

    def activate_button(self, some_button, eraser_mode=False):
        self.active_button.config(relief=tk.RAISED)
        some_button.config(relief=tk.SUNKEN)
        self.active_button = some_button
        self.eraser_on = eraser_mode

    def paint(self, event):
        if self.eraser_on:
            paint_color = 'white'
            self.line_width = self.DEFAULT_PEN_SIZE * 2
        else:
            paint_color = 'black'
            self.line_width = self.DEFAULT_PEN_SIZE
        if self.old_x and self.old_y:
            self.canvas.create_line(self.old_x, self.old_y, event.x, event.y,
                                    width=self.line_width, fill=paint_color,
                                    capstyle=tk.ROUND, smooth=tk.TRUE, splinesteps=36)
            ellipse_width = self.line_width // 2

            self.draw.ellipse([self.old_x - ellipse_width, self.old_y - ellipse_width, self.old_x + ellipse_width,
                               self.old_y + ellipse_width], fill=paint_color, outline=paint_color)
            self.draw.ellipse([event.x - ellipse_width, event.y - ellipse_width, event.x + ellipse_width,
                               event.y + ellipse_width], fill=paint_color, outline=paint_color)
            self.draw.line([self.old_x, self.old_y, event.x, event.y], fill=paint_color, width=self.line_width)

        self.old_x = event.x
        self.old_y = event.y

    def reset(self, _):
        self.old_x, self.old_y = None, None
