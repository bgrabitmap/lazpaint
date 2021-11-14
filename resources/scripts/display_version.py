# Version
# (fr) Version

from lazpaint import command, dialog
import sys

lazpaint_version = command.get_version()
python_version = sys.version_info

dialog.show_message("Python " + str(python_version[0]) + "." + str(python_version[1]) + "." + str(python_version[2]) + ", " + "LazPaint " + str(lazpaint_version[0]) + "." + str(lazpaint_version[1]) + "." + str(lazpaint_version[2]) )

