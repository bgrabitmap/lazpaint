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
  return command.send('ShowColorDialog?', Color=color)

def translate_text(text) -> str:
  return command.send('TranslateText?', Text=text)

def translate_dict(texts) -> dict:
  return dict(zip(texts, translate_text(texts)))

def get_language() -> str:
  return command.send('TranslateGetLanguage?')

def select_translation(**translations):
  lang = get_language()
  if translations.get(lang) is None:
    lang = "en"
  return translations[lang]

def get_script_name() -> str:
  return command.send('ScriptGetName?')
