from lazpaint import tools, image, colors, dialog

tools.choose(tools.GRADIENT)
tools.mouse([
    (0,0), (image.get_width()/2, image.get_height()/2)
  ])
tools.set_back_gradient_colors([colors.GREEN, colors.RED])
tools.set_back_gradient_interpolation(tools.GRADIENT_INTERPOLATION_LINEAR_HSL_POSITIVE)
tools.set_back_gradient_repetition(tools.GRADIENT_REPETITION_REPEAT)
tools.keys([tools.KEY_RETURN])

dialog.show_message(tools.get_back_gradient_colors())


