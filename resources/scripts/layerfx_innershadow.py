# Layer effect > Inner shadow
# (fr) Effet de calque > Ombre intérieure
from lazpaint import dialog

try:
    from tkinter import *
except ImportError:
    dialog.show_message("Please install tkinter. On Debian distributions, use the command: apt install python3-tk")
    exit()
        
from lazpaint import colors, image, layer, filters, tools, selection
import math

translation = dialog.translate_dict(["Layer is empty", "Radius", "Angle", "Opacity", "Ok", "Cancel"])

if layer.is_empty():
    dialog.show_message(translation["Layer is empty"])
    exit()

############ image processing

FRIENDLY_NAME = dialog.get_script_name()
REGISTRY_NAME = "innershadow"
OPPOSITE_REGISTRY_NAME = "innerlight"
DEFAULT_ANGLE = 135
DEFAULT_COLOR = colors.BLACK

MAX_RADIUS = 200
MAX_OPACITY = 255
MAX_ANGLE = 360

source_layer_id = layer.get_registry(REGISTRY_NAME+"-source-layer-id")
if source_layer_id is not None:
  layer.select_id(source_layer_id)
else:
  source_layer_id = layer.get_id()
source_layer_name = layer.get_name()

chosen_radius = layer.get_registry(REGISTRY_NAME+"-radius")
if chosen_radius == None: 
    chosen_radius = layer.get_registry(OPPOSITE_REGISTRY_NAME+"-radius")
if chosen_radius == None: 
    chosen_radius = image.get_registry(REGISTRY_NAME+"-radius")
if chosen_radius == None: 
    chosen_radius = image.get_registry(OPPOSITE_REGISTRY_NAME+"-radius")
if chosen_radius == None:
    chosen_radius = 20

chosen_angle = layer.get_registry(REGISTRY_NAME+"-angle")
if chosen_angle == None: 
    chosen_angle = layer.get_registry(OPPOSITE_REGISTRY_NAME+"-angle")
    if chosen_angle is not None:
        chosen_angle = (chosen_angle+180) % 360
if chosen_angle == None: 
    chosen_angle = image.get_registry(REGISTRY_NAME+"-angle")
if chosen_angle == None: 
    chosen_angle = image.get_registry(OPPOSITE_REGISTRY_NAME+"-angle")
    if chosen_angle is not None:
        chosen_angle = (chosen_angle+180) % 360
if chosen_angle == None:
    chosen_angle = DEFAULT_ANGLE

shadow_layer_id = layer.get_registry(REGISTRY_NAME+"-layer-id")
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
  overlay_color = DEFAULT_COLOR

def create_shadow_layer():
    global shadow_layer_id
    image.do_begin()
    if shadow_layer_id != None:
        layer.select_id(shadow_layer_id)
        layer.remove()
    layer.select_id(source_layer_id)
    layer.duplicate()
    layer.rasterize()
    layer.set_name(FRIENDLY_NAME+" - "+source_layer_name)
    layer.set_registry(REGISTRY_NAME+"-source-layer-id", source_layer_id)
    shadow_layer_id = layer.get_id()
    layer.set_registry("overlay-color", overlay_color)
    layer.set_opacity(chosen_opacity)    
    image.do_end()

blur_done = False
opacity_done = False

def apply_blur():
    global blur_done, opacity_done
    if opacity_done:
        image.undo()
        opacity_done = False
    if blur_done:
        image.undo()
        blur_done = False
    image.do_begin()
    if chosen_radius == 0:
        filters.filter_function(alpha=0, gamma_correction=False)
    else:
        layer.duplicate()
        layer.set_opacity(255)
        filters.filter_function(red="alpha", green="alpha", blue="alpha", alpha=1, gamma_correction=False)
        mask_layer_id = layer.get_id()    

        layer.select_id(shadow_layer_id)
        filters.filter_function(red=overlay_color.red/255, green=overlay_color.green/255, blue=overlay_color.blue/255, gamma_correction=False)
        tools.choose(tools.MOVE_LAYER)
        offset = (math.sin(chosen_angle*math.pi/180)*chosen_radius, -math.cos(chosen_angle*math.pi/180)*chosen_radius)
        tools.mouse([(0,0), (offset[0],offset[1])])

        layer.select_id(mask_layer_id)
        layer.duplicate()
        mask_layer_id2 = layer.get_id()
        layer.select_id(mask_layer_id)
        tools.choose(tools.MOVE_LAYER)
        tools.mouse([(offset[0]/2,offset[1]/2), (0,0)])
        colors.linear_negative()
        layer.set_blend_op(layer.BLEND_MASK)
        layer.merge_over()
        mask_layer_id = mask_layer_id2

        filters.blur(radius=chosen_radius)

        layer.select_id(mask_layer_id)
        layer.set_blend_op(layer.BLEND_MASK)
        layer.merge_over()  
   
    blur_done = image.do_end()
    apply_opacity()

def apply_opacity():
    global opacity_done
    if opacity_done:
        image.undo()
        opacity_done = False
    image.do_begin()
    layer.set_opacity(chosen_opacity)
    opacity_done = image.do_end()

######## interface

def button_ok_click():
    global source_layer_id, chosen_radius, chosen_angle
    layer.select_id(source_layer_id)
    layer.set_registry(REGISTRY_NAME+"-radius", chosen_radius)
    layer.set_registry(REGISTRY_NAME+"-angle", chosen_angle)
    layer.set_registry(REGISTRY_NAME+"-layer-id", shadow_layer_id)
    image.set_registry(REGISTRY_NAME+"-radius", chosen_radius)
    image.set_registry(REGISTRY_NAME+"-angle", chosen_angle)
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
    scale_radius_update_job = window.after(800, scale_radius_update_do)

scale_angle_update_job = None

def scale_angle_update_do():
    global scale_angle_update_job, chosen_angle, scale_angle
    new_angle = scale_angle.get() 
    if new_angle != chosen_angle:
        chosen_angle = new_angle
        apply_blur()
    scale_angle_update_job = None    

def scale_angle_update(event):
    global window, scale_angle_update_job
    if scale_angle_update_job:
        window.after_cancel(scale_angle_update_job)
    scale_angle_update_job = window.after(800, scale_angle_update_do)

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

label_angle = Label(frame, text=translation["Angle"])
label_angle.grid(column=0, row=1)
scale_angle = Scale(frame, from_=0, to=MAX_ANGLE, orient=HORIZONTAL, command=scale_angle_update)
scale_angle.grid(column=1, row=1, sticky=W+E, padx=10)
scale_angle.set(chosen_angle)

label_opacity = Label(frame, text=translation["Opacity"])
label_opacity.grid(column=0, row=2)
scale_opacity = Scale(frame, from_=0, to=MAX_OPACITY, orient=HORIZONTAL, command=scale_opacity_update)
scale_opacity.grid(column=1, row=2, sticky=W+E, padx=10)
scale_opacity.set(chosen_opacity)

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
