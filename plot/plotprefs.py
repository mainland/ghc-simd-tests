from matplotlib import rc

# http://www.sron.nl/~pault/

# Colorblind-safe colors
cbsafe1 = ["#66FF33", # lime green
           "#FF9933", # orange
           "#FF3333", # red
           "#336600", # moss green
           "#CC9999", # dusky pink
           "#CCCCCC", # grey
           "#000000", # black
           "#00CCFF", # blue
           "#9900CC", # purple
           "#009999"  # turquoise
           ]

# Excel color scheme
# From:
#   http://www.gilliganondata.com/index.php/2009/06/18/data-visualization-that-is-color-blind-friendly-excel-2007/
excel = ["#4572A7",
         "#AA4643",
         "#89A54E",
         "#71588F",
         "#4198AF",
         "#DB843D",
         "#93A9CF",
         "#D19392",
         "#B9CD96",
         "#A99BBD"
         ]

# From:
#   http://www.huyng.com/posts/sane-color-scheme-for-matplotlib/
sane = ["#348ABD",
        "#7A68A6",
        "#A60628",
        "#467821",
        "#CF4457",
        "#188487",
        "#E24A33"]
        
def config():
    rc('text', usetex=True)
    rc('text.latex', preview=True)

    FONTSIZE=16

    rc('font', family='serif', serif=['Times'], size=FONTSIZE)
    rc('axes', labelsize=FONTSIZE)
    rc('legend', fontsize=FONTSIZE)
    rc('xtick', labelsize=FONTSIZE)
    rc('ytick', labelsize=FONTSIZE)

    rc('lines', antialiased=True)
