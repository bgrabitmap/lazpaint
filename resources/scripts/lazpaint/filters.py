from lazpaint import command, dialog

if __name__ == "__main__":
  dialog.show_message("Library to execute filters on the current layer.")

#filters
BLUR_PRECISE = 'BlurPrecise'
BLUR_RADIAL = 'BlurRadial'
BLUR_FAST = 'BlurFast'
BLUR_BOX = 'BlurBox'
BLUR_CORONA = 'BlurCorona'
BLUR_DISK = 'BlurDisk'
BLUR_MOTION = 'BlurMotion'
BLUR_CUSTOM = 'BlurCustom'
PIXELATE = 'Pixelate'

SHARPEN = 'Sharpen'
SMOOTH = 'Smooth'
MEDIAN = 'Median'
NOISE = 'Noise'
CLEAR_TYPE = 'ClearType'
CLEAR_TYPE_INVERSE = 'ClearTypeInverse'
FILTER_FUNCTION = 'Function'
CONTOUR = 'Contour'
EMBOSS = 'Emboss'
PHONG = 'Phong'

SPHERE = 'Sphere'
TWIRL = 'Twirl'
WAVE_DISPLACEMENT = 'WaveDisplacement'
CYLINDER = 'Cylinder'
PLANE = 'Plane'

PIXELATE_QUALITY_FAST = 'Fast'
PIXELATE_QUALITY_LINEAR = 'Linear'
PIXELATE_QUALITY_MITCHELL = 'Mitchell'
PIXELATE_QUALITY_SPLINE = 'Spline'
PIXELATE_QUALITY_BEST = PIXELATE_QUALITY_MITCHELL

PHONG_COLOR_LAYER = 'Layer'
PHONG_COLOR_PEN = 'Pen'
PHONG_COLOR_BACK = 'Back'

PHONG_ALTITUDE_LIGHTNESS = 'Lightness'
PHONG_ALTITUDE_LINEAR_LIGHTNESS = 'LinearLightness'
PHONG_ALTITUDE_SATURATION = 'Saturation'
PHONG_ALTITUDE_ALPHA_CHANNEL = 'Alpha'
PHONG_ALTITUDE_RED_CHANNEL = 'Red'
PHONG_ALTITUDE_GREEN_CHANNEL = 'Green'
PHONG_ALTITUDE_BLUE_CHANNEL = 'Blue'

#colors
COLOR_COMPLEMENTARY = 'ComplementaryColor'
COLOR_NEGATIVE = 'Negative'
COLOR_LINEAR_NEGATIVE = 'LinearNegative'
COLOR_NORMALIZE = 'Normalize'
COLOR_GRAYSCALE = 'Grayscale'

#render
RENDER_PERLIN_NOISE = 'PerlinNoise'
RENDER_CYCLIC_PERLIN_NOISE = 'CyclicPerlinNoise'
RENDER_CLOUDS = 'Clouds'
RENDER_CUSTOM_WATER = 'CustomWater'
RENDER_WATER = 'Water'
RENDER_RAIN = 'Rain'
RENDER_WOOD = 'Wood'
RENDER_WOOD_VERTICAL = 'WoodVertical'
RENDER_PLASTIK = 'Plastik'
RENDER_METAL_FLOOR = 'MetalFloor'
RENDER_CAMOUFLAGE = 'Camouflage'
RENDER_SNOW_PRINT = 'SnowPrint'
RENDER_STONE = 'Stone'
RENDER_ROUND_STONE = 'RoundStone'
RENDER_MARBLE = 'Marble'

def run(name, validate=True):
  if name[0:5] == "Color":
    command.send(name, Validate=validate)
  else:
    command.send("Filter", Name=name, Validate=validate)

def blur(name=BLUR_FAST, radius=None, validate=True): #radius: float or (x,y) 
  if isinstance(radius, tuple):
    radius_x = radius[0]
    radius_y = radius[1]
    radius = None
  else:
    radius_x = None
    radius_y = None
  command.send("Filter", Name=name, Radius=radius, RadiusX=radius_x, RadiusY=radius_y, Validate=validate)

def blur_motion(distance=None, angle=None, oriented=None, validate=True): #oriented: bool
  command.send("Filter", Name=BLUR_MOTION, Distance=distance, Angle=angle, Oriented=oriented, Validate=validate)

def sharpen(amount=None, validate=True): #amout: 0..10
  command.send("Filter", Name=SHARPEN, Amount=amount, Validate=validate)

def noise(grayscale=None, opacity=None, validate=True): #grayscale: bool, opacity: 0..255
  command.send("Filter", Name=NOISE, Grayscale=grayscale, Opacity=opacity, Validate=validate)

def pixelate(pixel_size=None, quality=None, validate=True):
  command.send("Filter", Name=PIXELATE, PixelSize=pixel_size, Quality=quality, Validate=validate)

def filter_function(red=None, green=None, blue=None, alpha=None, hue=None, saturation=None, lightness=None, L=None, a=None, b=None, corrected_hue=None, gamma_correction=None, validate=True): #expressions: str
  command.send("Filter", Name=FILTER_FUNCTION, Red=red, Green=green, Blue=blue, Alpha=alpha, Hue=hue, Saturation=saturation, Lightness=lightness, CorrectedHue=corrected_hue, GammaCorrection=gamma_correction, Validate=validate)

def emboss(angle=None, transparent=None, preserve_colors=None, validate=True):
  command.send("Filter", Name=EMBOSS, Angle=angle, Transparent=transparent, PreserveColors=preserve_colors, Validate=validate)

def rain(amount=None, wind=None, validate=True): #amount and wind: 0..2
  command.send("Filter", Name=RENDER_RAIN, Amount=amount, Wind=wind, Validate=validate)

def phong(color_source=None, altitude_percent=None, altitude_source=None, light_pos_percent=None, light_x_percent=None, light_y_percent=None, validate=True):  
  command.send("Filter", Name=PHONG, ColorSource=color_source, AltitudePercent=altitude_percent, AltitudeSource=altitude_source, LightPosPercent=light_pos_percent, LightXPercent=light_x_percent, LightYPercent=light_y_percent, Validate=validate)

def twirl(radius=None, angle=None, center_pos_percent=None, center_x_percent=None, center_y_percent=None, validate=True):
  command.send("Filter", Name=TWIRL, Radius=radius, Angle=angle, CenterPosPercent=center_pos_percent, CenterXPercent=center_x_percent, CenterYPercent=center_y_percent, Validate=validate)

def wave_displacement(wave_length=None, displacement=None, phase=None, center_pos_percent=None, center_x_percent=None, center_y_percent=None, validate=True): #phase: 0..360
  command.send("Filter", Name=WAVE_DISPLACEMENT, WaveLength=wave_length, Displacement=displacement, Phase=phase, CenterPosPercent=center_pos_percent, CenterXPercent=center_x_percent, CenterYPercent=center_y_percent, Validate=validate)
