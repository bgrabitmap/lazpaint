# Version
# (fr) Version

from lazpaint import command, dialog
import platform

lazpaint_version = command.get_version()
python_version = platform.python_version()

dialog.show_message("Python " + python_version + ", " + "LazPaint " + str(lazpaint_version[0]) + "." + str(lazpaint_version[1]) + "." + str(lazpaint_version[2]) )

