import tkinter as tk
from tkinter import filedialog
from PIL import ImageTk, Image
from style_transfer import Net

IMAGE = 0
LABEL = 1
WIDGET = 2


class Mixer:
    def __init__(self):
        self.root = tk.Tk()
        self.im_width = self.root.winfo_screenwidth() // 4

        self.content_button = tk.Button(self.root, text='Add content', command=self.add_image('content'))
        self.content_button.grid(row=0, column=0)

        self.style_button = tk.Button(self.root, text='Add style', command=self.add_image('style'))
        self.style_button.grid(row=0, column=1)

        self.mix_button = tk.Button(self.root, text='Mix', command=self.mix)
        self.mix_button.grid(row=0, column=2)

        self.intense_slider = tk.Scale(self.root, from_=250, to=5000, label='Intensity', orient='horizontal')
        self.intense_slider.grid(row=2, column=2)
        self.intense_slider.set(1000)

        self.iter_slider = tk.Scale(self.root, from_=1, to=20, label='Iterations', orient='horizontal')
        self.iter_slider.grid(row=2, column=1)
        self.iter_slider.set(10)

        newimg = Image.new('RGB', [self.im_width, self.im_width])
        self.images = {
            'content': [None, tk.Label(self.root), None],
            'style': [None, tk.Label(self.root), None],
            'result': [None, tk.Label(self.root), None]
        }
        for i, which in enumerate(['content', 'style', 'result']):
            self.images[which][LABEL].grid(row=1, column=i)
            self.set_image(which, newimg)

        self.root.mainloop()

    def set_image(self, which, image):
        img = ImageTk.PhotoImage(self.resize_to_my_width(image))
        self.images[which][IMAGE] = image
        self.images[which][LABEL].configure(image=img)
        self.images[which][WIDGET] = img  # so it doesn't get garbage collected

    def resize_to_my_width(self, image):
        h = int(image.size[1] / image.size[0] * self.im_width)
        return image.resize([self.im_width, h])

    @staticmethod
    def open_image():
        image = None
        image_path = filedialog.askopenfilename()
        try:
            image = Image.open(image_path)
            image.load()
        except IOError:
            image = Mixer.open_image()
        finally:
            return image

    def add_image(self, which):
        return lambda: self.set_image(which, self.open_image())

    def mix(self):
        net = Net(self.intense_slider.get(), self.iter_slider.get(), self.images['content'][IMAGE],
                  self.images['style'][IMAGE])
        self.set_image('result', net.mix_images())


if __name__ == '__main__':
    gd = Mixer()
