from lazpaint import filters, image

#filters.filter_function(green="green*2")
#filters.phong(color_source=filter.PHONG_COLOR_LAYER, light_x_percent=20, light_y_percent=20, altitude_percent=30, altitude_source=filter.PHONG_ALTITUDE_ALPHA_CHANNEL)
#filters.emboss(angle=45, transparent=False, preserve_colors=True)
#filters.noise(grayscale=True, opacity=128)
#filters.pixelate(20, filter.PIXELATE_QUALITY_BEST)
#filters.twirl(radius=min(image.get_width(),image.get_height())/2, angle=360)
#filters.blur_motion(50, 45, True)
#filters.sharpen(1)
filters.wave_displacement(100,30,0,25,25)
