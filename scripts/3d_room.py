from lazpaint import tools, image, layer, dialog

def line(x, y, x2, y2):
    tools.choose(tools.PEN)
    tools.mouse([(x, y), (x2, y2)])
    tools.keys(tools.KEY_RETURN)


ZOOM = dialog.input_value("Zoom (Between 0.1 and 0.4 it creates a 3d room, more than 0.5 to 0.9 it creates a cross with a rectangle)", 0.25)

layer.new()
#top left
line(0, 0, image.get_width() * ZOOM, image.get_height() * ZOOM)
#bottom left
line(0, image.get_height(), image.get_width() * ZOOM, image.get_height() - (image.get_height() * ZOOM))
#top right
line(image.get_width(), 0, image.get_width() - (image.get_width() * ZOOM), image.get_height() * ZOOM)
#bottom right
line(image.get_width(), image.get_height(), image.get_width() - (image.get_width() * ZOOM), image.get_height() - (image.get_height() * ZOOM))
#top
line(image.get_width() * ZOOM, image.get_height() * ZOOM, image.get_width() - (image.get_width() * ZOOM),  image.get_height() * ZOOM)
#bottom
line(image.get_width() * ZOOM, image.get_height() - (image.get_height() * ZOOM), image.get_width() - (image.get_width() * ZOOM), image.get_height() - (image.get_height() * ZOOM))
#left
line(image.get_width() * ZOOM, image.get_height() * ZOOM, image.get_width() * ZOOM, image.get_height() - (image.get_height() * ZOOM))
#right
line(image.get_width() - (image.get_width() * ZOOM), image.get_height() - (image.get_height() * ZOOM), image.get_width() - (image.get_width() * ZOOM), image.get_height() * ZOOM)