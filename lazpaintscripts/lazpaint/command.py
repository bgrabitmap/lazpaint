import ast

# tells LazPaint it is a script, we are going to send commands
print("LazPaint script\t")
# wait for LazPaint response
if input('') != chr(27) + 'LazPaint': 
  exit()

# sends a command to LazPaint
def send(command, **keywords):
  if keywords is None:
    print(chr(27) + command)
  else:
    print(chr(27) + command + chr(29) + str(keywords))
  if command[-1] == '?':
    return ast.literal_eval(input(''))
  else:
    return

def show_message(message):
  send("ShowMessage?", Message=message)

if __name__ == "__main__":
  show_message("Library to communicate with LazPaint.")
