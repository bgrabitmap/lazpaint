# Layer effect > Stroke
# (fr) Effet de calque > Trait
from lazpaint import dialog

try:
    from tkinter import *
except ImportError:
    dialog.show_message("Please install tkinter. On Debian distributions, use the command: apt install python3-tk")
    exit()
        
from lazpaint import colors, image, layer, filters, tools, selection

translation = dialog.translate_dict(["Layer is empty", "Radius", "Color", "Opacity", "Ok", "Cancel"])

if layer.is_empty():
    dialog.show_message(translation["Layer is empty"])
    exit()

############ image processing

FRIENDLY_NAME = dialog.get_script_name()
MAX_RADIUS = 100
MAX_OPACITY = 255

source_layer_id = layer.get_id()
source_layer_name = layer.get_name()

chosen_radius = layer.get_registry("stroke-radius")
if chosen_radius == None: 
    chosen_radius = image.get_registry("stroke-radius")
if chosen_radius == None:
    chosen_radius = 10

stroke_layer_id = layer.get_registry("stroke-layer-id")
if image.get_layer_index(stroke_layer_id) == None:
    stroke_layer_id = None

if stroke_layer_id is not None:
    layer.select_id(stroke_layer_id)
    chosen_opacity = layer.get_opacity()
    overlay_color = colors.str_to_RGBA(layer.get_registry("overlay-color"))
    layer.select_id(source_layer_id)
else:
    chosen_opacity = layer.get_opacity() 
    overlay_color = None

if overlay_color is None:
  overlay_color = colors.BLACK

def create_stroke_layer():
    global stroke_layer_id
    image.do_begin()
    if stroke_layer_id != None:
        layer.select_id(stroke_layer_id)
        stroke_index = image.get_layer_index()
        selection.select_all()
        selection.delete()
        layer.select_id(source_layer_id)
        layer.duplicate()
        image.move_layer_index(image.get_layer_index(), stroke_index+1)
        layer.merge_over()        
    else:
        layer.select_id(source_layer_id)
        layer.duplicate()
        layer.set_name(FRIENDLY_NAME + " - " + source_layer_name)
        layer.set_registry("stroke-source-layer-id", source_layer_id)
        stroke_layer_id = layer.get_id()
        stroke_index = image.get_layer_index()
        image.move_layer_index(stroke_index, stroke_index-1)
    image.do_end()

stroke_done = False
opacity_done = False
stroke_initial_color = None

def apply_stroke():
    global stroke_done, opacity_done, stroke_initial_color
    if opacity_done:
        image.undo()
        opacity_done = False
    if stroke_done:
        image.undo()
        stroke_done = False
    image.do_begin() 
    if chosen_radius > 0.5:
        layer.duplicate()
        disk_id = layer.get_id()
        filters.blur(name=filters.BLUR_DISK, radius=chosen_radius-0.5)
        filters.filter_function(red=overlay_color.red/255, green=overlay_color.green/255, blue=overlay_color.blue/255, alpha="min(alpha*"+str(chosen_radius*5)+",(1-alpha)*"+str(chosen_radius*5)+")", gamma_correction=False)
        layer.select_id(stroke_layer_id) 
    else:
        disk_id = None   
    filters.blur(name=filters.BLUR_CORONA, radius=chosen_radius+0.5)
    filters.filter_function(red=overlay_color.red/255, green=overlay_color.green/255, blue=overlay_color.blue/255, alpha="min(alpha*"+str(chosen_radius)+",(1-alpha)*"+str(chosen_radius)+")", gamma_correction=False)
    if disk_id is not None:
        layer.select_id(disk_id)
        layer.merge_over()
    layer.set_registry("overlay-color", overlay_color)
    stroke_initial_color = overlay_color
    stroke_done = image.do_end()
    apply_opacity()

def apply_opacity():
    global opacity_done, chosen_opacity, overlay_color
    if opacity_done:
        image.undo()
        opacity_done = False
    image.do_begin()
    layer.set_opacity(chosen_opacity)
    if overlay_color != stroke_initial_color:
        filters.filter_function(red=overlay_color.red/255, green=overlay_color.green/255, blue=overlay_color.blue/255, gamma_correction=False)
        layer.set_registry("overlay-color", overlay_color)
    opacity_done = image.do_end()

######## interface

def button_ok_click():
    global source_layer_id, chosen_radius, chosen_offset
    layer.select_id(source_layer_id)
    layer.set_registry("stroke-radius", chosen_radius)
    layer.set_registry("stroke-layer-id", stroke_layer_id)
    image.set_registry("stroke-radius", chosen_radius)
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
        apply_stroke()
    scale_radius_update_job = None    

def scale_radius_update(event):
    global window, scale_radius_update_job
    if scale_radius_update_job:
        window.after_cancel(scale_radius_update_job)
    scale_radius_update_job = window.after(500, scale_radius_update_do)

scale_opacity_update_job = None

def scale_opacity_update_do():
    global chosen_opacity 
    new_opacity = scale_opacity.get()
    if new_opacity != chosen_opacity:
        chosen_opacity = new_opacity
        apply_opacity()
    scale_opacity_update_job = None

def scale_opacity_update(event):   
    global window, scale_opacity_update_job
    if scale_opacity_update_job:
        window.after_cancel(scale_opacity_update_job)
    scale_opacity_update_job = window.after(100, scale_opacity_update_do)

def button_color_click():
    global overlay_color, window
    new_color = colors.show_dialog(overlay_color)
    window.attributes('-topmost', True)
    window.attributes('-topmost', False)
    if new_color is not None and new_color != overlay_color:
        overlay_color = new_color
        apply_opacity()

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

label_opacity = Label(frame, text=translation["Opacity"])
label_opacity.grid(column=0, row=1)
scale_opacity = Scale(frame, from_=0, to=MAX_OPACITY, orient=HORIZONTAL, command=scale_opacity_update)
scale_opacity.grid(column=1, row=1, sticky=W+E, padx=10)
scale_opacity.set(chosen_opacity)

label_color = Label(frame, text=translation["Color"])
label_color.grid(column=0, row=2)
button_color = Button(frame, text=translation["Color"] + "...", command=button_color_click)
button_color.grid(column=1, row=2)

frame.columnconfigure(0, pad=20)
frame.columnconfigure(1, minsize=250)
frame.rowconfigure(0, pad=20)
frame.rowconfigure(1, pad=20)
frame.rowconfigure(2, pad=20)

button_ok = Button(window, text=translation["Ok"], command=button_ok_click)
button_ok.pack(side=RIGHT, padx=10, pady=10)
button_cancel = Button(window, text=translation["Cancel"], command=button_cancel_click)
button_cancel.pack(side=RIGHT, pady=10)

image.do_begin()
selection.deselect()
create_stroke_layer()
apply_stroke()

window.update()
window_width = window.winfo_width()
screen_width = window.winfo_screenwidth()
window.geometry('+%d+0' % (int((screen_width - window_width) / 2)))

window.lift()
window.attributes('-topmost',True)
window.after_idle(window.attributes,'-topmost',False)

window.mainloop()
button_cancel_click()
