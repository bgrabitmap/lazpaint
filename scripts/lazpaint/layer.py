from lazpaint import command, dialog, colors

if __name__ == "__main__":
  dialog.show_message("Library to access layer content.")

DM_DRAW = "dmDrawWithTransparency"
DM_LINEAR = "dmLinearBlend"
DM_SET = "dmSet"
DM_SET_EXCEPT_TRANSPARENT = "dmSetExceptTransparent"
DM_XOR = "dmXor"

BLEND_DRAW = 'Transparent'
BLEND_LINEAR = 'LinearBlend'
BLEND_LIGHTEN = 'Lighten'
BLEND_SCREEN = 'Screen'
BLEND_ADD = 'Additive'
BLEND_LINEAR_ADD = 'LinearAdd'
BLEND_COLOR_DODGE = 'ColorDodge'
BLEND_DIVIDE = 'Divide'
BLEND_NICE_GLOW = 'NiceGlow'
BLEND_SOFT_LIGHT = 'SoftLight'
BLEND_HARD_LIGHT = 'HardLight'
BLEND_GLOW = 'Glow'
BLEND_REFLECT = 'Reflect'
BLEND_OVERLAY = 'Overlay'
BLEND_DARK_OVERLAY = 'DarkOverlay'
BLEND_DARKEN = 'Darken'
BLEND_MULTIPLY = 'Multiply'
BLEND_COLOR_BURN = 'ColorBurn'
BLEND_DIFFERENCE = 'Difference'
BLEND_LINEAR_DIFFERENCE = 'LinearDifference'
BLEND_EXCLUSION = 'Exclusion'
BLEND_LINEAR_EXCLUSION = 'LinearExclusion'
BLEND_SUBTRACT = 'Subtract'
BLEND_LINEAR_SUBTRACT = 'LinearSubtract'
BLEND_SUBTRACT_INVERSE = 'SubtractInverse'
BLEND_LINEAR_SUBTRACT_INVERSE = 'LinearSubtractInverse'
BLEND_NEGATION = 'Negation'
BLEND_LINEAR_NEGATION = 'LinearNegation'
BLEND_XOR = 'Xor'
BLEND_SVG_SOFT_LIGHT = 'SvgSoftLight'

def get_id():
  return command.send("LayerGetId?")

def get_name() -> str:
  return command.send("LayerGetName?")

def get_opacity() -> int:
  return command.send("LayerGetOpacity?")

def get_blend_op():
  return command.send("LayerGetBlendOp?")

def get_visible() -> bool:
  return command.send("LayerGetVisible?")

def select_id(id):
  command.send("LayerSelectId", Id=id)

def set_name(name: str):
  return command.send("LayerSetName", Name=name)

def set_opacity(opacity: int):
  return command.send("LayerSetOpacity", Opacity=opacity)

def set_blend_op(blend_op):
  return command.send("LayerSetBlendOp", BlendOp=blend_op)

def set_visible(visible: bool):
  return command.send("LayerSetVisible", Visible=visible)

def new(): #-> id
  return command.send("LayerAddNew?")

def paste_as_new(): #-> id
  return command.send("EditPasteAsNewLayer?")

def add_from_file(file_name): #-> id
  return command.send("LayerFromFile?", FileName=file_name)

def duplicate(): #-> id
  return command.send("LayerDuplicate?")

def merge_over():
  command.send("LayerMergeOver")

def is_empty() -> bool:
  return command.send("IsLayerEmpty?")

def get_registry(identifier):
  str_result = command.send("LayerGetRegistry?", Identifier=identifier)
  if str_result == "":
    return None
  else:
    return command.parse_str(str_result)

def set_registry(identifier, value):
  if value == None:
    value = ""
  elif isinstance(value, str):
    value = repr(value)
  command.send("LayerSetRegistry", Identifier=identifier, Value=value)

def remove():
  command.send("LayerRemoveCurrent")

def get_count() -> int:
  return command.send("GetLayerCount?")

def rasterize():
  command.send("LayerRasterize")

def put_image(x: int, y: int, image, mode=DM_DRAW, opacity=255):
  height = len(image)
  if height == 0: return
  width = max([len(scanline) for scanline in image])
  flattened = ""
  for scanline in image:
    flattened += "".join([str(color) for color in scanline]) + "00000000" * (width - len(scanline))
  command.send("PutImage", X=x, Y=y, Width=width, Height=height, Data=flattened, Mode=mode, Opacity=opacity)

def get_pixel(x: int, y: int):
  return colors.str_to_RGBA(command.send("GetPixel?", X=x, Y=y))

def fill(color, mode=DM_DRAW):
  command.send("LayerFill", Color=color, Mode=mode)

def horizontal_flip():
  command.send("LayerHorizontalFlip")

def vertical_flip():
  command.send("LayerVerticalFlip")

def clear_alpha(back_color=None):
  command.send("ImageClearAlpha", BackColor=back_color)

def fill_background(back_color=None):
  command.send("ImageFillBackground", BackColor=back_color)
