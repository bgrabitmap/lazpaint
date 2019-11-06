from lazpaint import command, dialog, colors

if __name__ == "__main__":
  dialog.show_message("Library to act on the whole image.")

RESAMPLE_QUALITY_BOX = 'Box'
RESAMPLE_QUALITY_LINEAR = 'Linear'
RESAMPLE_QUALITY_HALF_COSINE = 'HalfCosine'
RESAMPLE_QUALITY_COSINE = 'Cosine'
RESAMPLE_QUALITY_BICUBIC = 'Bicubic'
RESAMPLE_QUALITY_MITCHELL = 'Mitchell'
RESAMPLE_QUALITY_SPLINE = 'Spline'
RESAMPLE_QUALITY_LANCZOS2 = 'Lanczos2'
RESAMPLE_QUALITY_LANCZOS3 = 'Lanczos3'
RESAMPLE_QUALITY_LANCZOS4 = 'Lanczos4'
RESAMPLE_QUALITY_BEST = 'BestQuality'

ANCHOR_TOP_LEFT = 'TopLeft'
ANCHOR_TOP = 'Top'
ANCHOR_TOP_RIGHT = 'TopRight'
ANCHOR_LEFT = 'Left'
ANCHOR_MIDDLE = 'Middle'
ANCHOR_RIGHT = 'Right'
ANCHOR_BOTTOM_LEFT = 'BottomLeft'
ANCHOR_BOTTOM = 'Bottom'
ANCHOR_BOTTOM_RIGHT = 'BottomRight' 

def new(width, height, color=colors.TRANSPARENT, ignore_modified=False):
  command.send("FileNew", Width=width, Height=height, BackColor=color, IgnoreModified=ignore_modified)

def paste_as_new():
  command.send("EditPasteAsNew")

def get_width():
  return command.send("GetImageWidth?")

def get_height():
  return command.send("GetImageHeight?")

def get_registry(identifier):
  str_result = command.send("ImageGetRegistry?", Identifier=identifier)
  if str_result == "":
    return None
  else:
    return command.parse_str(str_result)

def set_registry(identifier, value):
  if value == None:
    value = ""
  else:
    value = str(value)
  command.send("ImageSetRegistry", Identifier=identifier, Value=value)

def get_layer_index(layer_id=None):
  return command.send("GetLayerIndex?", LayerId=layer_id)

def select_layer_index(index):
  return command.send("SelectLayerIndex", Index=index)

def move_layer_index(from_index, to_index):
  return command.send("ImageMoveLayerIndex", FromIndex=from_index, ToIndex=to_index)

def get_layer_count():
  return command.send("GetLayerCount?")

def get_frame_index():
  return command.send("GetFrameIndex?")

def get_frame_count():
  return command.send("GetFrameCount?")

def load_frame(frame_index=None, ignore_modified=False):
  return command.send("FileChooseEntry?", EntryIndex=frame_index, IgnoreModified=ignore_modified)

def new_frame(width=None, height=None, back_color=colors.TRANSPARENT, ignore_modified=False):
  command.send("FileNewEntry", Width=width, Height=height, BackColor=back_color, IgnoreModified=ignore_modified)

def open(file_name=None, ignore_modified=False):
  command.send("FileOpen", FileName=file_name, IgnoreModified=ignore_modified)

def save(skip_options=False):
  return command.send("FileSave?", SkipOptions=skip_options)

def save_as(file_name=None, validate=False, overwrite=False, skip_options=False):
  return command.send("FileSaveAs?", FileName=file_name, Validate=validate, Overwrite=overwrite, SkipOptions=skip_options) 

def reload(ignore_modified=False):
  command.send("FileReload", IgnoreModified=ignore_modified)

def get_name():
  return command.send("GetFileName?")

def resample(width, height, quality=RESAMPLE_QUALITY_BEST):
  command.send("ImageResample", Width=width, Height=height, Quality=quality, Validate=True)

def smart_zoom3():
  command.send("ImageSmartZoom3")

def horizontal_flip():
  command.send("ImageHorizontalFlip")

def vertical_flip():
  command.send("ImageVerticalFlip")

def rotate_cw():
  command.send("ImageRotateCW")

def rotate_ccw():
  command.send("ImageRotateCCW")

def negative():
  command.send("ImageNegative")

def linear_negative():
  command.send("ImageLinearNegative")

def swap_red_blue():
  command.send("ImageSwapRedBlue")

def crop_to_selection():
  command.send("ImageCrop")

def crop_to_selection_and_layer():
  command.send("ImageCropLayer")

def flatten():
  command.send("ImageFlatten")

def canvas_size(width, height, anchor=ANCHOR_MIDDLE):
  command.send("ImageCanvasSize", Width=width, Height=height, Anchor=anchor)

def repeat(width, height, anchor=ANCHOR_MIDDLE, flip=False):
  command.send("ImageRepeat", Width=width, Height=height, Anchor=anchor, Flip=flip)

def undo():
  command.send("EditUndo")

def redo():
  command.send("EditRedo")

# starts a series of actions (undoable with only one call to "undo")
def do_begin():
  command.send("EditDoBegin")

# returns True if some action was done within the series of actions
def do_end():
  return command.send("EditDoEnd?")

