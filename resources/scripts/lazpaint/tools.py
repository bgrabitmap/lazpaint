from lazpaint import command, colors

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
ERASER_MODE_SHARPEN = 'Sharpen'
ERASER_MODE_SOFTEN = 'Soften'
ERASER_MODE_LIGHTEN = 'Lighten'
ERASER_MODE_DARKEN = 'Darken'

PEN_STYLE_SOLD = 'Solid'
PEN_STYLE_DASH = 'Dash'
PEN_STYLE_DOT = 'Dot'
PEN_STYLE_DASH_DOT = 'DashDot'
PEN_STYLE_DASH_DOT_DOT = 'DashDotDot'

JOIN_STYLE_BEVEL = 'Bevel'
JOIN_STYLE_MITER = 'Miter'
JOIN_STYLE_ROUND = 'Round'

LINE_CAP_ROUND = 'Round'
LINE_CAP_SQUARE = 'Square'
LINE_CAP_FLAT = 'Flat'

FONT_STYLE_BOLD = 'Bold'
FONT_STYLE_ITALIC = 'Italic'
FONT_STYLE_UNDERLINE = 'Underline'
FONT_STYLE_STRIKE_OUT = 'StrikeOut'

ALIGN_LEFT = 'Left'
ALIGN_CENTER = 'Center'
ALIGN_RIGHT = 'Right'

BIDI_AUTO = 'BidiAuto'
BIDI_LEFT_TO_RIGHT = 'LeftToRight'
BIDI_RIGHT_TO_LEFT = 'RightToLeft'

SHAPE_OPTION_DRAW_SHAPE = 'DrawShape'
SHAPE_OPTION_FILL_SHAPE = 'FillShape'
SHAPE_OPTION_CLOSE_SHAPE = 'CloseShape'

ARROW_NONE = 'None'
ARROW_TAIL = 'Tail'
ARROW_TIP = 'Tip'
ARROW_NORMAL = 'Normal'
ARROW_CUT = 'Cut'
ARROW_FLIPPED = 'Flipped'
ARROW_FLIPPED_CUT = 'FlippedCut'
ARROW_TRIANGLE = 'Triangle'
ARROW_TRIANGLE_BACK1 = 'TriangleBack1'
ARROW_TRIANGLE_BACK2 = 'TriangleBack2'
ARROW_HOLLOW_TRIANGLE = 'HollowTriangle'
ARROW_HOLLOW_TRIANGLE_BACK1 = 'HollowTriangleBack1'
ARROW_HOLLOW_TRIANGLE_BACK2 = 'HollowTriangleBack2'

SPLINE_STYLE_INSIDE = 'Inside'
SPLINE_STYLE_INSIDE_WITH_ENDS = 'InsideWithEnds'
SPLINE_STYLE_CROSSING = 'Crossing'
SPLINE_STYLE_CROSSING_WITH_ENDS = 'CrossingWithEnds'
SPLINE_STYLE_OUTSIDE = 'Outside'
SPLINE_STYLE_ROUND_OUTSIDE = 'RoundOutside'
SPLINE_STYLE_VERTEX_TO_SIDE = 'VertexToSide'
SPLINE_STYLE_EASY_BEZIER = 'EasyBezier'

GRADIENT_TYPE_LINEAR = 'Linear'
GRADIENT_TYPE_REFLECTED = 'Reflected'
GRADIENT_TYPE_DIAMOND = 'Diamond'
GRADIENT_TYPE_RADIAL =  'Radial'
GRADIENT_TYPE_ANGULAR = 'Angular'

GRADIENT_INTERPOLATION_STD_RGB = 'StdRGB'
GRADIENT_INTERPOLATION_LINEAR_RGB = 'LinearRGB'
GRADIENT_INTERPOLATION_LINEAR_HSL_POSITIVE = 'LinearHSLPositive'
GRADIENT_INTERPOLATION_LINEAR_HSL_NEGATIVE = 'LinearHSLNegative'
GRADIENT_INTERPOLATION_CORR_HSL_POSITIVE = 'GSBPositive'
GRADIENT_INTERPOLATION_CORR_HSL_NEGATIVE = 'GSBNegative'

GRADIENT_REPETITION_PAD = 'Pad'
GRADIENT_REPETITION_REPEAT = 'Repeat'
GRADIENT_REPETITION_REFLECT = 'Reflect'
GRADIENT_REPETITION_SINE = 'Sine'

TEXTURE_REPETITION_NONE = 'None';
TEXTURE_REPETITION_X = 'RepeatX';
TEXTURE_REPETITION_Y = 'RepeatY';
TEXTURE_REPETITION_BOTH = 'RepeatBoth';

SHAPE_KIND_RECTANGLE = 'Rectangle'
SHAPE_KIND_ROUND_RECTANGLE = 'RoundRectangle'
SHAPE_KIND_HALF_SPHERE = 'HalfSphere'
SHAPE_KIND_CONE_TOP = 'ConeTop'
SHAPE_KIND_CONE_SIDE = 'ConeSide'
SHAPE_KIND_HORIZ_CYLINDER = 'HorizCylinder'
SHAPE_KIND_VERT_CYLINDER = 'VertCylinder'

DEFORMATION_MODE_DEFORM = 'Deform'
DEFORMATION_MODE_MOVE_POINT_WITHOUT_DEFORMATION = 'MovePointWithoutDeformation'

FLOOD_FILL_PROGRESSIVE = 'Progressive'
FLOOD_FILL_FILL_ALL = 'FillAll' #not implemented

PERSPECTIVE_REPEAT = 'Repeat'
PERSPECTIVE_TWO_PLANES = 'TwoPlanes'

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

def set_fore_color(color):
  command.send("ToolSetForeColor", Color=color)

def set_back_color(color):
  command.send("ToolSetBackColor", Color=color)

def set_outline_color(color):
  command.send("ToolSetOutlineColor", Color=color)

def get_fore_color():
  return colors.str_to_RGBA(command.send("ToolGetForeColor?"))

def get_back_color():
  return colors.str_to_RGBA(command.send("ToolGetBackColor?"))

def get_outline_color():
  return colors.str_to_RGBA(command.send("ToolGetOutlineColor?"))

def set_eraser_mode(mode):
  command.send('ToolSetEraserMode', Mode=mode)

def get_eraser_mode():
  return command.send('ToolGetEraserMode?')

def set_eraser_alpha(alpha: int): #0..255
  command.send('ToolSetEraserAlpha', Alpha=alpha)

def get_eraser_alpha() -> int:
  return command.send('ToolGetEraserAlpha?')

def set_pen_width(width: float):
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

def set_line_cap(cap):
  command.send('ToolSetLineCap', Cap=cap)

def get_line_cap():
  return command.send('ToolGetLineCap?')

def set_shape_options(options: list):
  command.send('ToolSetShapeOptions', Options=options)

def get_shape_options() -> list:
  return command.send('ToolGetShapeOptions?')

def set_aliasing(enabled: bool):
  command.send('ToolSetAliasing', Enabled=enabled)

def get_aliasing() -> bool:
  return command.send('ToolGetAliasing?')

def set_shape_ratio(ratio: float):
  command.send('ToolSetShapeRatio', Ratio=ratio)

def get_shape_ratio() -> float:
  return command.send('ToolGetShapeRatio?')

def set_brush_index(index: int):
  command.send('ToolSetBrushIndex', Index=index)

def get_brush_index() -> int:
  return command.send('ToolGetBrushIndex?')

def get_brush_count() -> int: 
  return command.send('ToolGetBrushCount?')

def set_brush_spacing(spacing: int):
  command.send('ToolSetBrushSpacing', Spacing=spacing)

def get_brush_spacing() -> int:
  return command.send('ToolGetBrushSpacing?')

def set_font_name(name: str):
  command.send('ToolSetFontName', Name=name)

def get_font_name() -> str:
  return command.send('ToolGetFontName?')

def set_font_size(size: float):
  command.send('ToolSetFontSize', Size=size)

def get_font_size() -> float:
  return command.send('ToolGetFontSize?')

def set_font_style(style: list):
  command.send('ToolSetFontStyle', Style=style)

def get_font_style() -> list:
  return command.send('ToolGetFontStyle?')

def set_text_align(align):
  command.send('ToolSetTextAlign', Align=align)

def get_text_align():
  return command.send('ToolGetTextAlign?')

def set_text_bidi_mode(bidi_mode):
  command.send('ToolSetTextBidiMode', BidiMode=bidi_mode)

def get_text_bidi_mode():
  return command.send('ToolGetTextBidiMode?')

def set_text_outline(width: float):
  command.send('ToolSetTextOutline', Width=width)

def get_text_outline() -> float:
  return command.send('ToolGetTextOutline?')

def set_text_phong(enabled: bool):
  command.send('ToolSetTextPhong', Enabled=enabled)

def get_text_phong() -> bool:
  return command.send('ToolGetTextPhong?')

def set_light_position(x: float, y: float):
  command.send('ToolSetLightPosition', Position=(x, y))

def get_light_position() -> tuple:
  return command.send('ToolGetLightPosition?')

def set_arrow_start(arrow):
  command.send('ToolSetArrowStart', Arrow=arrow)

def get_arrow_start():
  return command.send('ToolGetArrowStart?')

def set_arrow_end(arrow):
  command.send('ToolSetArrowEnd', Arrow=arrow)

def get_arrow_end():
  return command.send('ToolGetArrowEnd?')

def set_arrow_size(x, y):
  command.send('ToolSetArrowSize', Size=(x, y))

def get_arrow_size() -> tuple:
  return command.send('ToolGetArrowSize?')

def set_spline_style(style):
  command.send('ToolSetSplineStyle', Style=style)

def get_spline_style():
  return command.send('ToolGetSplineStyle?')

def set_fore_gradient_type(gradient_type):
  command.send('ToolSetForeGradientType', GradientType=gradient_type)

def get_fore_gradient_type():
  return command.send('ToolGetForeGradientType?')

def set_fore_gradient_colors(colors: list):
  command.send('ToolSetForeGradientColors', Colors=colors)

def get_fore_gradient_colors() -> list:
  return colors.str_to_RGBA(command.send('ToolGetForeGradientColors?'))

def set_fore_gradient_interpolation(interpolation):
  command.send('ToolSetForeGradientInterpolation', Interpolation=interpolation)

def get_fore_gradient_interpolation():
  return command.send('ToolGetForeGradientInterpolation?')

def set_fore_gradient_repetition(repetition):
  command.send('ToolSetForeGradientRepetition', Repetition=repetition)

def get_fore_gradient_repetition():
  return command.send('ToolGetForeGradientRepetition?')

def set_back_gradient_type(gradient_type):
  command.send('ToolSetBackGradientType', GradientType=gradient_type)

def get_back_gradient_type():
  return command.send('ToolGetGradientType?')

def set_back_gradient_interpolation(interpolation):
  command.send('ToolSetBackGradientInterpolation', Interpolation=interpolation)

def get_back_gradient_interpolation():
  return command.send('ToolGetBackGradientInterpolation?')

def set_back_gradient_colors(colors: list):
  command.send('ToolSetBackGradientColors', Colors=colors)

def get_back_gradient_colors() -> list:
  return colors.str_to_RGBA(command.send('ToolGetBackGradientColors?'))

def set_back_gradient_repetition(repetition):
  command.send('ToolSetBackGradientRepetition', Repetition=repetition)

def get_back_gradient_repetition():
  return command.send('ToolGetBackGradientRepetition?')

def set_outline_gradient_type(gradient_type):
  command.send('ToolSetOutlineGradientType', GradientType=gradient_type)

def get_outline_gradient_type():
  return command.send('ToolGetOutlineGradientType?')

def set_outline_gradient_colors(colors: list):
  command.send('ToolSetOutlineGradientColors', Colors=colors)

def get_outline_gradient_colors() -> list:
  return colors.str_to_RGBA(command.send('ToolGetOutlineGradientColors?'))

def set_outline_gradient_interpolation(interpolation):
  command.send('ToolSetOutlineGradientInterpolation', Interpolation=interpolation)

def get_outline_gradient_interpolation():
  return command.send('ToolGetOutlineGradientInterpolation?')

def set_outline_gradient_repetition(repetition):
  command.send('ToolSetOutlineGradientRepetition', Repetition=repetition)

def get_outline_gradient_repetition():
  return command.send('ToolGetOutlineGradientRepetition?')

def set_fore_texture(file_name):
  command.send('ToolSetForeTexture', FileName=file_name)

def set_fore_texture_repetition(repetition):
  command.send('ToolSetForeTextureRepetition', Repetition=repetition)

def get_fore_texture_repetition():
  return command.send('ToolGetForeTextureRepetition?')

def set_fore_texture_opacity(opacity: int):
  command.send('ToolSetForeTextureOpacity', Opacity=opacity)

def get_fore_texture_opacity(): #-> int 0..255
  return command.send('ToolGetForeTextureOpacity?')

def set_back_texture(file_name):
  command.send('ToolSetBackTexture', FileName=file_name)

def set_back_texture_repetition(repetition):
  command.send('ToolSetBackTextureRepetition', Repetition=repetition)

def get_back_texture_repetition():
  return command.send('ToolGetBackTextureRepetition?')

def set_back_texture_opacity(opacity: int):
  command.send('ToolSetBackTextureOpacity', Opacity=opacity)

def get_back_texture_opacity(): #-> int 0..255
  return command.send('ToolGetBackTextureOpacity?')

def set_outline_texture(file_name):
  command.send('ToolSetOutlineTexture', FileName=file_name)

def set_outline_texture_repetition(repetition):
  command.send('ToolSetOutlineTextureRepetition', Repetition=repetition)

def get_outline_texture_repetition():
  return command.send('ToolGetOutlineTextureRepetition?')

def set_outline_texture_opacity(opacity: int):
  command.send('ToolSetOutlineTextureOpacity', Opacity=opacity)

def get_outline_texture_opacity(): #-> int 0..255
  return command.send('ToolGetOutlineTextureOpacity?')

def set_phong_shape_kind(kind):
  command.send('ToolSetPhongShapeKind', Kind=kind)

def get_phong_shape_kind():
  return command.send('ToolGetPhongShapeKind?')

def set_grid_deformation_mode(mode):
  command.send('ToolSetDeformationGridMode', Mode=mode)

def get_grid_deformation_mode():
  return command.send('ToolGetDeformationGridMode?')

def set_flood_fill_options(options: list):
  command.send('ToolSetFloodFillOptions', Options=options)

def get_flood_fill_options() -> list:
  return command.send('ToolGetFloodFillOptions?')

def set_perspective_options(options: list):
  command.send('ToolSetPerspectiveOptions', Options=options)

def get_perspective_options() -> list:
  return command.send('ToolGetPerspectiveOptions?')

