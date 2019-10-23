from lazpaint import filter

filter.filter_function(green="green*2")
filter.noise(grayscale=True, opacity=128)
filter.pixelate(20, filter.PIXELATE_QUALITY_BEST)

