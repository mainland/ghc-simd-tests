from matplotlib import rc

# Colorblind-safe colors
colors = ["#66FF33", # lime green
          "#FF9933", # orange
          "#FF3333", # red
          "#336600", # moss green
          "#CC9999", # dusky pink
          "#CCCCCC", # grey
          "#000000", # black
          "#00CCFF", # blue
          "#9900CC", # purple
          "#009999" # turquoise
          ]

def config():
    rc('text', usetex=True)
    rc('text.latex', preview=True)

    FONTSIZE=16

    rc('font', family='serif', serif=['Times'], size=FONTSIZE)
    rc('axes', labelsize=FONTSIZE)
    rc('legend', fontsize=FONTSIZE)
    rc('xtick', labelsize=FONTSIZE)
    rc('ytick', labelsize=FONTSIZE)
