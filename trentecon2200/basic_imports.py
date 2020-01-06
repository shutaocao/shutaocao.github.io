import numpy as np
import pandas as pd
# Pandas options
pd.options.display.max_columns = 30
pd.options.display.max_rows = 20
from scipy import linalg, optimize
from datetime import datetime, date, timedelta

# If in ipython, load autoreload extension
if 'ipython' in globals():
    print('\nWelcome to IPython!')
    ipython.magic('load_ext autoreload')
    ipython.magic('autoreload 2')

# Display all cell outputs in notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

from IPython.display import display
from IPython.display import Audio
from IPython.display import Video
from IPython.display import YouTubeVideo
from IPython.display import VimeoVideo
from IPython.display import HTML
from IPython.display import Latex
from IPython.display import Image
from IPython.display import FileLink
from IPython.display import SVG
from IPython.display import (
    display_pretty, display_html, display_jpeg,
    display_png, display_json, display_latex, display_svg
)

# Visualization
import chart_studio.plotly as pltly
import plotly.graph_objs as go
from plotly.offline import iplot, init_notebook_mode
init_notebook_mode(connected=True)
#import cufflinks as cf
#cf.go_offline(connected=True)
#cf.set_config_file(theme='pearl')
%matplotlib inline
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.patches as patches
plt.style.reload_library()
#plt.style.use("ggplot")
plt.style.use("seaborn-ticks")
