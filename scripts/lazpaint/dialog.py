import ast
from lazpaint import command

def show_message(message):
  # wait for validation before continuing script
  command.send("ShowMessage?", Message=message)

def show_directory_dialog(prompt, initial_dir=None) -> str:
  return command.send("ShowDirectoryDialog?", Prompt=prompt, InitialDir=initial_dir)

def input_text(prompt, default=None) -> str:
  return command.send("InputBox?", Prompt=prompt, Default=default)

def input_value(prompt, default):
  return ast.literal_eval(input_text(prompt, str(default)))

def show_color_dialog(color=None) -> str:
  return command.send('ShowColorDialog?', Color=color);

