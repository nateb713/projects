from psychopy import visual, core
from psychopy.visual import Window, TextStim, shape, Rect, Circle, Polygon
from psychopy.event import waitKeys, getKeys, clearEvents
from psychopy.core import wait, getTime, Clock, quit
from psychopy import prefs, gui, logging
import sys
import os
import random

#constants
DISPSIZE = (800, 600)

#participant id
myDlg = gui.Dlg(title="ID number")
myDlg.addField('ID:')
dlgData = myDlg.show()
ID = str(dlgData[0])


_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
isExist = os.path.exists("data")

if not isExist:
    os.makedirs("data")

if os.path.isfile('data' + ID + '.csv'):
    dataFileName = 'data' + ID + '.csv'
else:
    dataFileName = 'data' + ID + '.csv'
log = open(dataFileName, 'w')

#escape
prefs.general['shutdownKey'] = 'escape'

#make clock
clock = Clock()

#create window
disp = Window(size=DISPSIZE, units='pix', color='black')
disp.mouseVisible = False

#create data file
header = 'id, trialNum, shape, color, hit, response, rt, presentTime, responseTime\n'
log.write(header)
log.close()


#fixation cross
cross = shape.ShapeStim(disp, units='pix', lineColor='white', fillColor='white', pos=(0,0), size=30, linewidth=3, vertices='cross')

# Define the basic colors
colors = [
    "black", "gray", "red", "green", "blue",
    "yellow", "cyan", "magenta", "purple", "pink", "orange",
    "brown", "teal", "gold", "navy", "lime", "maroon", "olive", "violet"
]

# Grid parameters
cols = 7  # Number of columns in the grid
rows = (len(colors) + cols - 1) // cols  # Calculate required rows
square_size = 80  # Size of each square
gap = 15  # Space between squares

# Calculate starting positions
start_x = -((cols - 1) * (square_size + gap)) / 2
start_y = ((rows - 1) * (square_size + gap)) / 2

# Create and draw the grid of colored squares
shape_objects = []
for idx, color in enumerate(colors):
    col = idx % cols
    row = idx // cols
    x_pos = start_x + col * (square_size + gap)
    y_pos = start_y - row * (square_size + gap * 2)

    rect = visual.Rect(win, width=square_size, height=square_size,
                       fillColor=color, lineColor="black", pos=(x_pos, y_pos))
    shape_objects.append(rect)

    # Create a text label for the color name
    label = visual.TextStim(win, text=color, pos=(x_pos, y_pos - square_size / 1.5),
                            height=15, color="black" if color != "black" else "white")
    shape_objects.append(label)

# Draw all shapes
for obj in shape_objects:
    obj.draw()
    win.flip()
    wait(3)


# Wait for a mouse click to close
from psychopy import event
mouse = event.Mouse()
while not mouse.getPressed()[0]:  # Wait until left mouse button is clicked
    pass

win.close()