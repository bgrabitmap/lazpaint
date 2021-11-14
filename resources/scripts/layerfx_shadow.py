# Layer effect > Drop shadow
# (fr) Effet de calque > Ombre portée
# (es) Efectos de capa > Sombra paralela
# (de) Ebeneneffekte > Schlagschatten
from lazpaint import dialog

try:
    from tkinter import *
except ImportError:
    dialog.show_message("Please install tkinter. On Debian distributions, use the command: apt install python3-tk")
    exit()
        
from lazpaint import colors, image, layer, filters, tools, selection

translation = dialog.translate_dict(["Layer is empty", "Shadow of ", "Radius", "Offset", "Opacity", "Ok", "Cancel"])

if layer.is_empty():
    dialog.show_message(translation["Layer is empty"])
    exit()

############ image processing

FRIENDLY_NAME = dialog.get_script_name()
MAX_RADIUS = 100
MAX_OFFSET = 100
MAX_OPACITY = 255

source_layer_id = layer.get_registry("shadow-source-layer-id")
if source_layer_id is not None:
  layer.select_id(source_layer_id)
else:
  source_layer_id = layer.get_id()
source_layer_name = layer.get_name()

chosen_radius = layer.get_registry("shadow-radius")
if chosen_radius == None: 
    chosen_radius = image.get_registry("shadow-radius")
if chosen_radius == None:
    chosen_radius = 10

chosen_offset = layer.get_registry("shadow-offset")
if chosen_offset == None or len(chosen_offset) != 2:
    chosen_offset = image.get_registry("shadow-offset")
if chosen_offset == None or len(chosen_offset) != 2:
    chosen_offset = (10, 10)

shadow_layer_id = layer.get_registry("shadow-layer-id")
if image.get_layer_index(shadow_layer_id) == None:
    shadow_layer_id = None

if shadow_layer_id is not None:
    layer.select_id(shadow_layer_id)
    chosen_opacity = layer.get_opacity()
    overlay_color = colors.str_to_RGBA(layer.get_registry("overlay-color"))
    layer.select_id(source_layer_id)
else:
    chosen_opacity = layer.get_opacity()*2/3 
    overlay_color = None

if overlay_color is None:
  overlay_color = colors.BLACK

def create_shadow_layer():
    global shadow_layer_id
    image.do_begin()
    if shadow_layer_id != None:
        layer.select_id(shadow_layer_id)
        layer.remove()
    layer.select_id(source_layer_id)
    layer.duplicate()
    layer.set_name(FRIENDLY_NAME + " - " + source_layer_name)
    layer.set_registry("shadow-source-layer-id", source_layer_id)
    shadow_layer_id = layer.get_id()
    shadow_index = image.get_layer_index()
    image.move_layer_index(shadow_index, shadow_index-1)
    filters.filter_function(red=overlay_color.red/255, green=overlay_color.green/255, blue=overlay_color.blue/255, gamma_correction=False)
    layer.set_registry("overlay-color", overlay_color)
    layer.set_opacity(chosen_opacity)    
    image.do_end()

blur_done = False
offset_done = False

def apply_blur():
    global blur_done, offset_done
    if offset_done:
        image.undo()
        offset_done = False
    if blur_done:
        image.undo()
        blur_done = False
    image.do_begin() 
    filters.blur(radius=chosen_radius)
    blur_done = image.do_end()
    apply_offset()

def apply_offset():
    global offset_done
    if offset_done:
        image.undo()
        offset_done = False
    image.do_begin()
    tools.choose(tools.MOVE_LAYER)
    tools.mouse([(0,0), chosen_offset])
    layer.set_opacity(chosen_opacity)
    offset_done = image.do_end()

######## interface

def button_ok_click():
    global source_layer_id, chosen_radius, chosen_offset
    layer.select_id(source_layer_id)
    layer.set_registry("shadow-radius", chosen_radius)
    layer.set_registry("shadow-offset", chosen_offset)
    layer.set_registry("shadow-layer-id", shadow_layer_id)
    image.set_registry("shadow-radius", chosen_radius)
    image.set_registry("shadow-offset", chosen_offset)
    image.do_end()
    exit()

def button_cancel_click():    
    if image.do_end():
        image.undo()
    layer.select_id(source_layer_id)
    exit()

scale_radius_update_job = None

def scale_radius_update_do():
    global scale_radius_update_job, chosen_radius, scale_radius
    new_radius = scale_radius.get() 
    if new_radius != chosen_radius:
        chosen_radius = new_radius
        apply_blur()
    scale_radius_update_job = None    

def scale_radius_update(event):
    global window, scale_radius_update_job
    if scale_radius_update_job:
        window.after_cancel(scale_radius_update_job)
    scale_radius_update_job = window.after(500, scale_radius_update_do)

scale_offset_update_job = None

def scale_offset_update_do():
    global chosen_offset 
    new_offset = (scale_offset_x.get(), scale_offset_y.get())
    if new_offset != chosen_offset:
        chosen_offset = new_offset
        apply_offset()
    scale_offset_update_job = None

def scale_offset_update(event):   
    global window, scale_offset_update_job
    if scale_offset_update_job:
        window.after_cancel(scale_offset_update_job)
    scale_offset_update_job = window.after(100, scale_offset_update_do)

scale_opacity_update_job = None

def scale_opacity_update_do():
    global chosen_opacity 
    new_opacity = scale_opacity.get()
    if new_opacity != chosen_opacity:
        chosen_opacity = new_opacity
        apply_offset()
    scale_opacity_update_job = None

def scale_opacity_update(event):   
    global window, scale_opacity_update_job
    if scale_opacity_update_job:
        window.after_cancel(scale_opacity_update_job)
    scale_opacity_update_job = window.after(100, scale_opacity_update_do)

window = Tk()
window.title(FRIENDLY_NAME)
window.resizable(False, False)

frame = Frame(window)
frame.pack()

label_radius = Label(frame, text=translation["Radius"])
label_radius.grid(column=0, row=0)
scale_radius = Scale(frame, from_=0, to=MAX_RADIUS, orient=HORIZONTAL, command=scale_radius_update)
scale_radius.grid(column=1, row=0, sticky=W+E, padx=10)
scale_radius.set(chosen_radius)

label_offset = Label(frame, text=translation["Offset"])
label_offset.grid(column=0, row=1)
scale_offset_x = Scale(frame, from_=-MAX_OFFSET, to=MAX_OFFSET, orient=HORIZONTAL, command=scale_offset_update)
scale_offset_x.grid(column=1, row=1, sticky=W+E, padx=10)
scale_offset_x.set(chosen_offset[0])
scale_offset_y = Scale(frame, from_=-MAX_OFFSET, to=MAX_OFFSET, orient=HORIZONTAL, command=scale_offset_update)
scale_offset_y.grid(column=1, row=2, sticky=W+E, padx=10)
scale_offset_y.set(chosen_offset[1])

label_opacity = Label(frame, text=translation["Opacity"])
label_opacity.grid(column=0, row=3)
scale_opacity = Scale(frame, from_=0, to=MAX_OPACITY, orient=HORIZONTAL, command=scale_opacity_update)
scale_opacity.grid(column=1, row=3, sticky=W+E, padx=10)
scale_opacity.set(chosen_opacity)

frame.columnconfigure(0, pad=20)
frame.columnconfigure(1, minsize=250)
frame.rowconfigure(0, pad=20)
frame.rowconfigure(1, pad=20)
frame.rowconfigure(2, pad=20)
frame.rowconfigure(3, pad=20)

button_ok = Button(window, text=translation["Ok"], command=button_ok_click)
button_ok.pack(side=RIGHT, padx=10, pady=10)
button_cancel = Button(window, text=translation["Cancel"], command=button_cancel_click)
button_cancel.pack(side=RIGHT, pady=10)

image.do_begin()
selection.deselect()
create_shadow_layer()
apply_blur()

window.update()
window_width = window.winfo_width()
screen_width = window.winfo_screenwidth()
window.geometry('+%d+0' % (int((screen_width - window_width) / 2)))

window.lift()
window.attributes('-topmost',True)
window.after_idle(window.attributes,'-topmost',False)

window.mainloop()
button_cancel_click()
