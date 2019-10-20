import collections

CustomRGBA = collections.namedtuple("RGBA", "red, green, blue, alpha")
class RGBA(CustomRGBA):
  def __repr__(self):
    if self.alpha != 255: 
      return '#{:02X}{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue,self.alpha) 
    else:
      return '#{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue) 
  def __str__(self):
    return '{:02X}{:02X}{:02X}{:02X}'.format(self.red,self.green,self.blue,self.alpha)

def RGB(red,green,blue):
  return RGBA(red,green,blue,255)

def str_to_RGBA(s):
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

