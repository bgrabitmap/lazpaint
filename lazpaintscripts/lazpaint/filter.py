from lazpaint import command, dialog

if __name__ == "__main__":
  dialog.show_message("Library to execute filters on the current layer.")

BLUR_PRECISE = 'BlurPrecise'
BLUR_RADIAL = 'BlurRadial'
BLUR_FAST = 'BlurFast'
BLUR_BOX = 'BlurBox'
BLUR_CORONA = 'BlurCorona'
BLUR_DISK = 'BlurDisk'
BLUR_MOTION = 'BlurMotion'
BLUR_CUSTOM = 'BlurCustom'
SHARPEN = 'Sharpen'
SMOOTH = 'Smooth'
MEDIAN = 'Median'
NOISE = 'Noise'
PIXELATE = 'Pixelate'
CLEAR_TYPE = 'ClearType'
CLEAR_TYPE_INVERSE = 'ClearTypeInverse'
FILTER_FUNCTION = 'Function'
EMBOSS = 'Emboss'
PHONG = 'Phong'
CONTOUR = 'Contour'
GRAYSCALE = 'Grayscale'
NEGATIVE = 'Negative'
LINEAR_NEGATIVE = 'LinearNegative'
COMPLEMENTARY_COLOR = 'ComplementaryColor'
NORMALIZE = 'Normalize'
SPHERE = 'Sphere'
TWIRL = 'Twirl'
WAVE_DISPLACEMENT = 'WaveDisplacement'
CYLINDER = 'Cylinder'
PLANE = 'Plane'
PERLIN_NOISE = 'PerlinNoise'
CYCLIC_PERLIN_NOISE = 'CyclicPerlinNoise'
CLOUDS = 'Clouds'
CUSTOM_WATER = 'CustomWater'
WATER = 'Water'
RAIN = 'Rain'
WOOD = 'Wood'
WOOD_VERTICAL = 'WoodVertical'
PLASTIK = 'Plastik'
METAL_FLOOR = 'MetalFloor'
CAMOUFLAGE = 'Camouflage'
SNOW_PRINT = 'SnowPrint'
STONE = 'Stone'
ROUND_STONE = 'RoundStone'
MARBLE = 'Marble'

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

def run(name, validate=True):
  command.send("Filter", Name=name, Validate=validate)

def blur(name, radius=None, radius_x=None, radius_y=None, validate=True):
  command.send("Filter", Name=name, Radius=radius, RadiusX=radius_x, RadiusY=radius_y, Validate=validate)

def blur_motion(distance=None, angle=None, oriented=None, validate=True):
  command.send("Filter", Name=BLUR_MOTION, Distance=distance, Angle=angle, Oriented=oriented, Validate=validate)

def sharpen(amount=None, validate=True):
  command.send("Filter", Name=SHARPEN, Amount=amount, Validate=validate)

def noise(grayscale=None, opacity=None, validate=True):
  command.send("Filter", Name=NOISE, Grayscale=grayscale, Opacity=opacity, Validate=validate)

def pixelate(pixel_size=None, quality=None, validate=True):
  command.send("Filter", Name=PIXELATE, PixelSize=pixel_size, Quality=quality, Validate=validate)

def filter_function(red=None, green=None, blue=None, alpha=None, hue=None, saturation=None, lightness=None, validate=True):
  command.send("Filter", Name=FILTER_FUNCTION, Red=red, Green=green, Blue=blue, Alpha=alpha, Hue=hue, Saturation=saturation, Lightness=lightness, Validate=validate)

def emboss(angle=None, transparent=None, preserve_colors=None, validate=True):
  command.send("Filter", Name=EMBOSS, Angle=angle, Transparent=transparent, PreserveColors=preserve_colors, Validate=validate)

def rain(amount=None, wind=None, validate=True):
  command.send("Filter", Name=RAIN, Amount=amount, Wind=wind, Validate=validate)

def posterize(levels=None, by_lightness=None, validate=True):
  command.send("Filter", Name=POSTERIZE, Levels=levels, ByLightness=by_lightness, Validate=validate)

def phong(color_source=None, altitude_percent=None, altitude_source=None, light_x_percent=None, light_y_percent=None, validate=True):
  command.send("Filter", Name=PHONG, ColorSource=color_source, AltitudePercent=altitude_percent, AltitudeSource=altitude_source, LightXPercent=light_x_percent, LightYPercent=light_y_percent, Validate=validate)

def twirl(radius=None, angle=None, center_x_percent=None, center_y_percent=None, validate=True):
  command.send("Filter", Name=TWIRL, Radius=radius, Angle=angle, CenterXPercent=center_x_percent, CenterYPercent=center_y_percent, Validate=validate)
