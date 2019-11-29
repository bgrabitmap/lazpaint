from lazpaint import command

HAND = 'Hand'
HOT_SPOT = 'HotSpot'
MOVE_LAYER = 'MoveLayer'
ROTATE_LAYER = 'RotateLayer'
ZOOM_LAYER = 'ZoomLayer'
PEN = 'Pen'
BRUSH = 'Brush'
CLONE = 'Clone'
COLOR_PICKER = 'ColorPicker'
ERASER = 'Eraser'
EDIT_SHAPE = 'EditShape'
RECTANGLE = 'Rect'
ELLIPSE = 'Ellipse'
POLYGON = 'Polygon'
SPLINE = 'Spline'
FLOOD_FILL = 'FloodFill'
GRADIENT = 'Gradient'
PHONG_SHAPE = 'Phong'
SELECT_PEN = 'SelectPen'
SELECT_RECT = 'SelectRect'
SELECT_ELLIPSE = 'SelectEllipse'
SELECT_POLY = 'SelectPoly'
SELECT_SPLINE = 'SelectSpline'
MOVE_SELECTION = 'MoveSelection'
ROTATE_SELECTION = 'RotateSelection'
MAGIC_WAND = 'MagicWand'
DEFORMATION_GRID = 'Deformation'
TEXTURE_MAPPING = 'TextureMapping'
LAYER_MAPPING = 'LayerMapping'
TEXT = 'Text'

#click state
STATE_LEFT = 'Left'
STATE_RIGHT = 'Right'
STATE_SHIFT = 'Shift'
STATE_ALT = 'Alt'
STATE_CTRL = 'Ctrl'

ERASER_MODE_ALPHA = 'EraseAlpha'
ERASER_MODE_SOFTEN = 'Soften'

PEN_STYLE_SOLD = 'Solid'
PEN_STYLE_DASH = 'Dash'
PEN_STYLE_DOT = 'Dot'
PEN_STYLE_DASH_DOT = 'DashDot'
PEN_STYLE_DASH_DOT_DOT = 'DashDotDot'

JOIN_STYLE_BEVEL = 'Bevel'
JOIN_STYLE_MITER = 'Miter'
JOIN_STYLE_ROUND = 'Round'

SHAPE_OPTION_ALIASING = 'Aliasing'
SHAPE_OPTION_DRAW_SHAPE = 'DrawShape'
SHAPE_OPTION_FILL_SHAPE = 'FillShape'
SHAPE_OPTION_CLOSE_SHAPE = 'CloseShape'

KEY_UNKNOWN = 'Unknown'
KEY_BACKSPACE = 'Backspace'
KEY_TAB = 'Tab'
KEY_RETURN = 'Return'
KEY_ESCAPE = 'Escape'
KEY_PAGE_UP = 'PageUp'
KEY_PAGE_DOWN = 'PageDown'
KEY_HOME = 'Home'
KEY_END = 'End'
KEY_LEFT = 'Left'
KEY_UP = 'Up'
KEY_RIGHT = 'Right'
KEY_DOWN = 'Down'
KEY_INSERT = 'Insert'
KEY_DELETE = 'Delete'
KEY_NUM0 = 'Num0'
KEY_NUM1 = 'Num1'
KEY_NUM2 = 'Num2'
KEY_NUM3 = 'Num3'
KEY_NUM4 = 'Num4'
KEY_NUM5 = 'Num5'
KEY_NUM6 = 'Num6'
KEY_NUM7 = 'Num7'
KEY_NUM8 = 'Num8'
KEY_NUM9 = 'Num9'
KEY_F1 = 'F1'
KEY_F2 = 'F2'
KEY_F3 = 'F3'
KEY_F4 = 'F4'
KEY_F5 = 'F5'
KEY_F6 = 'F6'
KEY_F7 = 'F7'
KEY_F8 = 'F8'
KEY_F9 = 'F9'
KEY_F10 = 'F10'
KEY_F11 = 'F11'
KEY_F12 = 'F12'
KEY_A = 'A'
KEY_B = 'B'
KEY_C = 'C'
KEY_D = 'D'
KEY_E = 'E'
KEY_F = 'F'
KEY_G = 'G'
KEY_H = 'H'
KEY_I = 'I'
KEY_J = 'J'
KEY_K = 'K'
KEY_L = 'L'
KEY_M = 'M'
KEY_M = 'N'
KEY_O = 'O'
KEY_P = 'P'
KEY_Q = 'Q'
KEY_R = 'R'
KEY_S = 'S'
KEY_T = 'T'
KEY_U = 'U'
KEY_V = 'V'
KEY_W = 'W'
KEY_X = 'X'
KEY_Y = 'Y'
KEY_W = 'Z'
KEY_0 = '0'
KEY_1 = '1'
KEY_2 = '2'
KEY_3 = '3'
KEY_4 = '4'
KEY_5 = '5'
KEY_6 = '6'
KEY_7 = '7'
KEY_8 = '8'
KEY_9 = '9'
KEY_SHIFT = 'Shift'
KEY_CTRL = 'Ctrl'
KEY_ALT = 'Alt'

def choose(name):
  command.send("ChooseTool", Name=name)

def mouse(coords, state=[STATE_LEFT], default_pressure=1.0):
  if not isinstance(coords, list):
      xy = [(float(coords[0]), float(coords[1]))]
      if len(coords)>2:
        pressure = [float(coords[2])]
      else:
        pressure = [default_pressure]
  else:
      xy = [(float(c[0]), float(c[1])) for c in coords]
      pressure = [float(c[2]) if len(c)>2 else default_pressure for c in coords]      
  command.send("ToolMouse", Coords=xy, State=state, Pressure=pressure)

def keys(keys, state=[]):
  if isinstance(keys, str):
    keys = [keys]
  command.send("ToolKeys", Keys=keys, State=state)

def write(text):
  command.send("ToolWrite", Text=text)

def set_pen_color(color):
  command.send("ToolSetPenColor", Color=color)

def set_back_color(color):
  command.send("ToolSetBackColor", Color=color)

def get_pen_color():
  return str_to_RGBA(command.send("ToolGetPenColor?"))

def get_back_color():
  return str_to_RGBA(command.send("ToolGetBackColor?"))

def set_eraser_mode(mode):
  command.send('ToolSetEraserMode', Mode=mode)

def get_eraser_mode():
  return command.send('ToolGetEraserMode?')

def set_eraser_alpha(alpha):
  command.send('ToolSetEraserAlpha', Alpha=alpha)

def get_eraser_alpha():
  return command.send('ToolGetEraserAlpha?')

def set_pen_width(width):
  command.send('ToolSetPenWidth', Width=width)

def get_pen_width():
  return command.send('ToolGetPenWidth?')

def set_pen_style(style):
  command.send('ToolSetPenStyle', Style=style)

def get_pen_style():
  return command.send('ToolGetPenStyle?')

def set_join_style(style):
  command.send('ToolSetJoinStyle', Style=style)

def get_join_style():
  return command.send('ToolGetJoinStyle?')

def set_shape_options(options):
  command.send('ToolSetShapeOptions', Options=options)

def get_shape_options():
  return command.send('ToolGetShapeOptions?')

