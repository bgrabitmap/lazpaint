# Render > Fractal tree
# (fr) Rendu > Arbre fractal
from lazpaint import tools, image, layer, dialog
import math, random

translation = dialog.translate_dict(["Invalid angle", "Vertical size"])

line_buf = []

def line(x, y, x2, y2):
  global line_buf
  if len(line_buf) > 0 and line_buf[-1] == (x, y):
    line_buf.append( (x2, y2) )
  else:
    flush_line()
    line_buf = [(x, y), (x2, y2)]

def flush_line():
  global line_buf
  if len(line_buf) > 0:
    tools.choose(tools.PEN)
    tools.mouse(line_buf)
  line_buf = []

DEG_TO_RAD = math.pi / 180
ANGLE = abs(dialog.input_value(dialog.translate_text("Angle") + " (< 90)", 45))
if ANGLE >= 90:
  dialog.show_message(translation["Invalid angle"])
  exit()
DEFAULT_SIZE_Y = 7*8/2*2 * (1.14+math.cos(ANGLE * DEG_TO_RAD))/2.14
MULTIPLIER = image.get_height() / DEFAULT_SIZE_Y

ZOOM = dialog.input_value(translation["Vertical size"] + " (%)", 95)
MULTIPLIER = MULTIPLIER * ZOOM/100


def drawTree(x1, y1, angle, depth):
    if (depth > 0):
        x2 = x1 + (math.cos(angle * DEG_TO_RAD) * depth * MULTIPLIER)
        y2 = y1 + (math.sin(angle * DEG_TO_RAD) * depth * MULTIPLIER)
        line(x1, y1, x2, y2)
        drawTree(x2, y2, angle - ANGLE, depth - 2)
        drawTree(x2, y2, angle + ANGLE, depth - 2)

image.do_begin()
layer.new()
drawTree(image.get_width() / 2, image.get_height(), -90, 14)
flush_line()
image.do_end()
