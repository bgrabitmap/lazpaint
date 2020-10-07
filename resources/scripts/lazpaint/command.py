import ast

# tells LazPaint it is a script, we are going to send commands
print("LazPaint script\t")
# wait for LazPaint response
if input('') != chr(27) + 'LazPaint': 
  print("Needs to be run from LazPaint.")
  exit()
  
import sys
if sys.platform == "win32":
  import io
  sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding = 'utf-8')
  sys.stderr = io.TextIOWrapper(sys.stderr.detach(), encoding = 'utf-8')
  sys.stdin = io.TextIOWrapper(sys.stdin.detach(), encoding = 'utf-8')

def parse_str(text: str):
  if text[:1] == "#":
    return text
  else:
    return ast.literal_eval(text)

# sends a command to LazPaint
def send(command: str, **keywords):
  if keywords is None:
    print(chr(27) + command)
  else:
    print(chr(27) + command + chr(29) + repr(keywords))
  if command[-1] == '?':
    return parse_str(input(''))
  else:
    return

def get_version(): # (major, minor, revision)
  return send("LazPaintGetVersion?")
