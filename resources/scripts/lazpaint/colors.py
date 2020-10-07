import collections, math
from lazpaint import dialog, command, filters

GAMMA = 2.2
ALPHA_OPAQUE = 255
ALPHA_TRANSPARENT = 0

if __name__ == "__main__":
  dialog.show_message("Library defining colors.")

def to_linear(std_value: int): #0..255
  return math.pow(std_value/255, GAMMA)

def to_std(linear_value: float):
  return round(math.pow(linear_value, 1/GAMMA)*255)

CustomRGBA = collections.namedtuple("RGBA", "red, green, blue, alpha")
class RGBA(CustomRGBA):
  def __repr__(self):
    if self.alpha != 255: 
      return '#{:02X}{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue,self.alpha) 
    else:
      return '#{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue) 
  def __str__(self):
    return '{:02X}{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue,self.alpha)
  def __eq__(self, other):
    if isinstance(other, RGBA):
      return (other.red == self.red) and (other.green == self.green) and (other.blue == self.blue) and (other.alpha == self.alpha)
    else:
      return NotImplemented
  def negative(self):
    return RGBA(to_std(1-to_linear(self.red)), to_std(1-to_linear(self.green)), to_std(1-to_linear(self.blue)), self.alpha)
  def linear_negative(self):
    return RGBA(255-self.red, 255-self.green, 255-self.blue, self.alpha)
  def swap_red_blue(self):
    return RGBA(self.blue, self.green, self.red, self.alpha)
  def grayscale(self):
    gray = to_std(to_linear(self.red)*0.299 + to_linear(self.green)*0.587 + to_linear(self.blue)*0.114)
    return RGBA(gray, gray, gray, self.alpha)

def RGB(red: int, green: int, blue: int): #0..255
  return RGBA(red, green, blue, 255)

def str_to_RGBA(s):
  if s is None:
    return None
  elif isinstance(s, list):
    return [str_to_RGBA(c) for c in s]
  else:
    if s[0:1] == "#":
      s = s[1:]
    if len(s) == 6:
      return RGBA(int(s[0:2],16), int(s[2:4],16), int(s[4:6],16), 255)
    elif len(s) == 8:
      return RGBA(int(s[0:2],16), int(s[2:4],16), int(s[4:6],16), int(s[6:8],16))
    else:
      raise ValueError("Invalid color string")

TRANSPARENT = RGBA(0,0,0,0)

#VGA color names
BLACK = RGB(0,0,0)
BLUE = RGB(0,0,255)
LIME = RGB(0,255,0)
AQUA = RGB(0,255,255)
RED = RGB(255,0,0)
FUCHSIA = RGB(255,0,255)
ORANGE = RGB(255,165,0)
YELLOW = RGB(255,255,0)
WHITE = RGB(255,255,255)
GRAY = RGB(128,128,128)
NAVY = RGB(0,0,128)
GREEN = RGB(0,128,0)
TEAL = RGB(0,128,128)
MAROON = RGB(128,0,0)
PURPLE = RGB(128,0,128)
OLIVE = RGB(128,128,0)
SILVER = RGB(192,192,192)

def get_curve(points: list, posterize=False):
  return {'Points': points, 'Posterize': posterize}

def curves(red=[], red_posterize=False, green=[], green_posterize=False, blue=[], blue_posterize=False, hue=[], hue_posterize=False, saturation=[], saturation_posterize=False, lightness=[], lightness_posterize=False, validate=True):
  command.send('ColorCurves', Red=get_curve(red, red_posterize), Green=get_curve(green, green_posterize), Blue=get_curve(blue, blue_posterize), Hue=get_curve(hue, hue_posterize), Saturation=get_curve(saturation, saturation_posterize), Lightness=get_curve(lightness, lightness_posterize), Validate=validate)

def posterize(levels=None, by_lightness=True, validate=True):
  command.send('ColorPosterize', Levels=levels, ByLightness=by_lightness, Validate=validate)

def colorize(hue_angle=None, saturation=None, correction=None, validate=True): #saturation: 0..1
  command.send('ColorColorize', Hue=hue_angle, Saturation=saturation, Correction=correction, Validate=validate)

def complementary():
  filters.run(filters.COLOR_COMPLEMENTARY)

def shift(hue_angle=None, saturation=None, correction=None, validate=True): #saturation shift: -2..2
  command.send('ColorShiftColors', Hue=hue_angle, Saturation=saturation, Correction=correction, Validate=validate)

def intensity(factor=None, shift=None, validate=True): #factor and shift: -2..2
  command.send('ColorIntensity', Factor=factor, Shift=shift, Validate=validate)

def lightness(factor=None, shift=None, validate=True): #factor and shift: -2..2
  command.send('ColorLightness', Factor=factor, Shift=shift, Validate=validate)

def negative():
  filters.run(filters.COLOR_NEGATIVE)

def linear_negative():
  filters.run(filters.COLOR_LINEAR_NEGATIVE)

def normalize():
  filters.run(filters.COLOR_NORMALIZE)

def grayscale():
  filters.run(filters.COLOR_GRAYSCALE)

def show_dialog(color=None): #-> RGBA
  result_str = dialog.show_color_dialog(color)
  if result_str is None or len(result_str) == 0:
    return None
  else: 
    return str_to_RGBA(result_str)



