from lazpaint import tools, image, layer, dialog

def line(x, y, x2, y2):
    tools.choose(tools.PEN)
    tools.mouse([(x, y), (x2, y2)])

ZOOM = dialog.input_value("Zoom (Between 0.1 and 0.4 it creates a 3d room, more than 0.5 to 0.9 it creates a cross with a rectangle)", 0.25)

image.do_begin()
layer.new()
w = image.get_width()
h = image.get_height() 

#top left
line(0, 0, w * ZOOM, h * ZOOM)
#bottom left
line(0, h, w * ZOOM, h - (h * ZOOM))
#top right
line(w, 0, w - (w * ZOOM), h * ZOOM)
#bottom right
line(w, h, w - (w * ZOOM), h - (h * ZOOM))
#top
line(w * ZOOM, h * ZOOM, w - (w * ZOOM),  h * ZOOM)
#bottom
line(w * ZOOM, h - (h * ZOOM), w - (w * ZOOM), h - (h * ZOOM))
#left
line(w * ZOOM, h * ZOOM, w * ZOOM, h - (h * ZOOM))
#right
line(w - (w * ZOOM), h - (h * ZOOM), w - (w * ZOOM), h * ZOOM)

image.do_end()
