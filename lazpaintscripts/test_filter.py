from lazpaint import filter, image

#filter.filter_function(green="green*2")
#filter.phong(color_source=filter.PHONG_COLOR_LAYER, light_x_percent=20, light_y_percent=20, altitude_percent=30, altitude_source=filter.PHONG_ALTITUDE_ALPHA_CHANNEL)
#filter.emboss(angle=45, transparent=False, preserve_colors=True)
#filter.noise(grayscale=True, opacity=128)
#filter.pixelate(20, filter.PIXELATE_QUALITY_BEST)
#filter.twirl(radius=min(image.get_width(),image.get_height())/2, angle=360)
#filter.blur_motion(50, 45, True)
#filter.sharpen(1)
filter.wave_displacement(100,30,0,25,25)
