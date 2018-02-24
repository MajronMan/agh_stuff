import seaborn
from PIL import Image
imgx = 512
imgy = 512
image = Image.new("RGB", (imgx, imgy))

# drawing area
xa = -1.0
xb = 1.0
ya = -1.0
yb = 1.0

maxIt = 20 # max iterations allowed
h = 1e-6 # step size for numerical derivative
eps = 1e-3 # max error allowed

# put any complex function here to generate a fractal for it!
def f(z):
    return z ** 6 + z ** 3 - 1.0


def newtons_fractal(f):
    palette = [tuple(map(lambda x: int(255*x), t)) for t in seaborn.color_palette('deep', maxIt)]
    # draw the fractal
    for y in range(imgy):
        zy = y * (yb - ya) / (imgy - 1) + ya
        for x in range(imgx):
            zx = x * (xb - xa) / (imgx - 1) + xa
            z = complex(zx, zy)
            i = 0
            for i in range(maxIt):
                # complex numerical derivative
                dz = (f(z + complex(h, h)) - f(z)) / complex(h, h)
                z0 = z - f(z) / dz  # Newton iteration
                if abs(z0 - z) < eps:  # stop when close enough to any root
                    break
                z = z0
            image.putpixel((x, y), palette[i])

    image.show()
    image.save("newtonFr.png", "PNG")

newtons_fractal(f)