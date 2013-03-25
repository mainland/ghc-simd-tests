#!/usr/bin/env python
import itertools
import logging
import math
import sys

import matplotlib

if sys.platform == 'darwin':
    matplotlib.use('Cairo')
else:
    matplotlib.use('GTKCairo')

from matplotlib import rc
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt

import numpy as np

from optparse import OptionParser

LABELS = {('dotp', 'scalar'):   'Haskell',
          ('dotp', 'vector'):   'Haksell (SSE)',
          ('dotp', 'cscalar'):  'Hand-written C',
          ('dotp', 'cmanual'):  'Hand-written C (SSE)',
          ('dotp', 'cblas'):    'Goto BLAS 1.13',
          ('dotp', 'dph'):      'DPH',
          ('dotp', 'dphmulti'): 'DPH (SSE)',

          ('rbf', 'scalar'):         'Vector library',
          ('rbf', 'vector'):         'Haskell',
          ('rbf', 'cmanual'):        'C (BLAS)',
          ('rbf', 'cmanual_int'):    'C (BLAS w/intermediate)',
          ('rbf', 'blitz'):          'Blitz++',
          ('rbf', 'boost'):          'Boost uBlas',
          ('rbf', 'eigen'):          'Eigen',
          ('rbf', 'eigen_abs'):      'Eigen (user abstraction)',
          ('rbf', 'eigen_abs_good'): 'Eigen (good norm2)',
          ('rbf', 'eigen_abs_bad'):  'Eigen (bad norm2)'}

def main():
    (opts, filepaths) = getOpts()

    matplotConfig(opts)

    data = {}

    for i in range(0, len(opts.dataset)):
        label = opts.dataset[i]
        filepath = filepaths[i]

        if opts.xdata == 'threads':
            data[label] = mlab.csv2rec(filepaths[i],
                                       comments='#',
                                       names=['func','variant','n','m','tmean','tmax','tmin','tstddev'])
        else:
            data[label] = mlab.csv2rec(filepaths[i],
                                       comments='#',
                                       names=['func','variant','n','tmean','tmax','tmin','tstddev'])

    if opts.time:
        plotTime(opts, data)

    if opts.ratio:
        plotRatio(opts, data)

    if opts.mflops:
        plotMFlops(opts, data)
            
    if opts.output:
        plt.savefig(opts.output, transparent=True)
    else:
        plt.show()

def getOpts():
    usage = "usage: %prog [options] FILE"
    parser = OptionParser(usage=usage)
    parser.add_option("-d", "--debug",
                      action="store_true", dest="debug")
    parser.add_option("-o", "--output",
                      action="store", type="string", dest="output")
                      
    parser.add_option("--dataset",
                      action="append", type="string", dest="dataset")
                      
    parser.add_option("--func",
                      action="store", type="string", dest="func")
    parser.add_option("--variant",
                      action="append", type="string", dest="variant")
                      
    parser.add_option("--sum",
                      action="store_true", dest="sum")
    parser.add_option("--dotp",
                      action="store_true", dest="dotp")
    parser.add_option("--rbf",
                      action="store_true", dest="rbf")

    parser.add_option("--xdata",
                      action="store", type="string", dest="xdata")
    parser.add_option("--bytes-factor",
                      action="store", type="float", dest="bytesfactor")
    parser.add_option("--flops-factor",
                      action="store", type="float", dest="flopsfactor")
                                            
    parser.add_option("--time",
                      action="store_true", dest="time")
    parser.add_option("--mflops",
                      action="store_true", dest="mflops")
    parser.add_option("--ratio",
                      action="store", type="string", dest="ratio")
    
    parser.add_option("--sec",
                      action="store_true", dest="sec")

    parser.add_option("--fontsize",
                      action="store", type="string", dest="fontsize")
    parser.add_option("--legend-fontsize",
                      action="store", type="string", dest="legendfontsize")
    
    parser.add_option("--legend-loc",
                      action="store", type="string", dest="legendLoc")
    parser.add_option("--sort-legend-at",
                      action="store", type="string", dest="sortLegendAt")
    parser.add_option("--errorbars",
                      action="store_true", dest="errorbars")
    
    parser.add_option("--l1",
                      action="store", type="string", dest="l1")
    parser.add_option("--l2",
                      action="store", type="string", dest="l2")
    parser.add_option("--l3",
                      action="store", type="string", dest="l3")
    parser.add_option("--cache-label-at",
                      action="store", type="float", dest="cacheLabelAt")
                      
    parser.add_option("--xmin",
                      action="store", type="string", dest="xmin")
    parser.add_option("--xmax",
                      action="store", type="string", dest="xmax")
    parser.add_option("--ymax",
                      action="store", type="string", dest="ymax")
    parser.add_option("--ymin",
                      action="store", type="string", dest="ymin")
                      
    parser.add_option("--text-color",
                      action="store", type="string", dest="textColor")
    
    parser.add_option("--gill",
                      action="store_true", dest="gill")
    parser.add_option("--palatino",
                      action="store_true", dest="palatino")
    parser.add_option("--segoe",
                      action="store_true", dest="segoe")
    (opts, args) = parser.parse_args()

    if opts.debug:
       level = logging.DEBUG
    else:
       level = logging.INFO

    logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s',
                        level=level)

    if len(args) == 0:
        parser.error("Must specify data file")

    if not opts.dataset or len(opts.dataset) != len(args):
        parser.error("Each data set must be given a data set label")
        
    if opts.sortLegendAt:
        opts.sortLegendAt = float(eval(opts.sortLegendAt))
        
    if opts.l1:
        opts.l1 = float(eval(opts.l1))
        
    if opts.l2:
        opts.l2 = float(eval(opts.l2))
        
    if opts.l3:
        opts.l3 = float(eval(opts.l3))
        
    if opts.xmin:
        opts.xmin = float(eval(opts.xmin))
        
    if opts.xmax:
        opts.xmax = float(eval(opts.xmax))
        
    if opts.ymin:
        opts.ymin = float(eval(opts.ymin))
        
    if opts.ymax:
        opts.ymax = float(eval(opts.ymax))

    return (opts, args)

def selectField(r, func, lbl, v):
    mask = (r["func"] == func) & (r[lbl] == v)
    return r[mask]
    
def plotTime(opts, data):
    if not opts.legendLoc:
        opts.legendLoc = 'upper left'
        
    plt.clf()
    plt.cla()
    
    ax = plt.subplot(111)

    CS = itertools.cycle(plotColors)
    MR = itertools.cycle(['+', '',   '.', '1',  '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])
    MS = itertools.cycle([10,   7,   10,  10,   10])

    for v in opts.variant:
        r = selectField(data['haskell'], opts.func, "variant", v)
        if opts.sec:
            t = r.tmean
        else:
            t = r.tmean*1000.0

        plt.errorbar(r['n'], t,
                     color=CS.next(),
                     marker=MR.next(),
                     linestyle=LS.next(),
                     markersize=MS.next(),
                     label=LABELS[(opts.func, v)])

    if opts.sec:
        plt.ylabel('Time (sec)')
    else:
        plt.ylabel('Time (ms)')
    
    ax.set_xscale('log', basex=2)
    ax.set_yscale('log', basex=2)

    if not opts.xmin and not opts.xmax and not opts.ymin and not opts.ymax:
        ax.set_autoscale_on(True)
    else:
        ax.set_autoscale_on(False)
        ax.set_xlim(xmin=opts.xmin, xmax=opts.xmax)
        ax.set_ylim(ymin=opts.ymin, ymax=opts.ymax)
    
    plt.legend(loc=opts.legendLoc, numpoints=1)

    if opts.xdata == 'threads':
        plt.xlabel('Number of Threads')
    else:
        plt.xlabel('Vector size (elements)')

    return ax

def plotRatio(opts, data):
    CS = itertools.cycle(plotColors)
    HS = itertools.cycle(['//', 'xx', '..'])

    BASELINE = opts.ratio
    
    baseline = selectField(data['haskell'], opts.func, 'variant', BASELINE)
    if opts.xdata == 'threads':
        baseline = baseline[::-1]
    baseline_t = baseline.tmean
    
    plt.clf()
    plt.cla()

    numGroups = len(baseline)
    groupSize = len(opts.variant)
    
    # the x locations for the groups
    ind = np.arange(numGroups)
    # the width of the bars
    width = 0.7
    
    ax = plt.subplot(111)

    ax.set_ylabel('Vector size (elements)')

    if not opts.xmin and not opts.xmax and not opts.ymin and not opts.ymax:
        ax.set_autoscale_on(True)
    else:
        ax.set_autoscale_on(True)

        if not opts.ymin:
            opts.ymin = 0.0

        if not opts.ymax:
            opts.ymax = 0.0
            
        ax.set_ylim(ymin=opts.ymin, ymax=opts.ymax)
    
    ax.set_xticks(ind+width/2)

    if opts.xdata == 'threads':
        ax.set_xticklabels( [r.m for r in baseline] )
    else:
        ax.set_xticklabels( ["$2^{%d}$" % int(math.log(r.n,2)) for r in baseline] )
        
    for line in ax.get_xticklines():
        line.set_marker(None)

    off = 0.0

    for v in opts.variant:
        r = selectField(data['haskell'], opts.func, "variant", v)
        if opts.xdata == 'threads':
            r = r[::-1]
        t = r.tmean
        tstddev = r.tstddev

        if opts.errorbars:
            yerr = tstddev/baseline_t
        else:
            yerr = None    

        ax.bar(ind+off, t/baseline_t, width/groupSize,
               color=CS.next(),
               #hatch=HS.next(),
               ecolor='k',
               yerr=yerr,
               label=LABELS[(opts.func, v)])
        off += width/groupSize

    plt.legend(loc='upper right', numpoints=1)

    if opts.xdata == 'threads':
        plt.xlabel('Number of Threads')
    else:
        plt.xlabel('Vector size (elements)')
        
    plt.ylabel('Execution Time Ratio')
        
def plotMFlops(opts, dataSets):
    if not opts.legendLoc:
        opts.legendLoc = 'upper left'

    if not opts.xmin:
        opts.xmin = 2*9

    if not opts.xmax:
        opts.xmax = 2**28

    if not opts.ymin:
        opts.ymin = 0

    if not opts.ymax:
        opts.ymax = 8000

    if not opts.cacheLabelAt:
        opts.cacheLabelAt = 3400
    
    plt.clf()
    plt.cla()
    
    ax = plt.subplot(111)

    numVariants = len(opts.variant)
    
    CS = itertools.cycle(plotColors[0:numVariants])
    MR = itertools.cycle(['+', '.',   'x', '1',  '2', '3'][0:numVariants])
    MS = itertools.cycle([7,   7,   7,  7,   10, 10][0:numVariants])
    LS = itertools.cycle(['-', '--', ':'])

    labeledData = {}
    
    for name in dataSets.keys():
        data = dataSets[name]

        ls = LS.next()
        
        for v in opts.variant:
            r = selectField(data, opts.func, "variant", v)

            label = LABELS[(opts.func, v)]

            if len(r) > 0 and len(dataSets) == 1 or (name != 'haskell' and label != 'Haskell' or name == 'haskell' and label == 'Haskell'):
                numBytes = opts.bytesfactor*r['n']

                flops = opts.flopsfactor*r.n/(r.tmean*1000*1000)
                flops_stddev = opts.flopsfactor*r.n/((r.tmean - r.tstddev)*1000*1000) - flops

                if not opts.errorbars:
                    flops_stddev = None

                # Don't want "Haskell (haskell)"
                if len(dataSets) != 1 and name.lower() != label.lower():
                    label = label + " (" + name + ")"

                labeledData[label] = (numBytes, flops)

                plt.errorbar(numBytes, flops,
                             yerr=flops_stddev,
                             color=CS.next(),
                             marker=MR.next(),
                             linestyle=ls,
                             markersize=MS.next(),
                             label=label)
            else:
                CS.next()
                MR.next()
                MS.next()   

    #
    # Plot cache sizes
    #
    COLOR=(0.3,0.3,0.3)
    ALPHA=0.5
    
    temp = 0.02*(opts.ymax-opts.ymin)
    YMID1 = opts.cacheLabelAt - temp
    YMID2 = opts.cacheLabelAt
    YMID3 = opts.cacheLabelAt + temp
    
    xLines = [("L1", opts.l1),
              ("L2", opts.l2),
              ("L3", opts.l3)]
    
    for (lbl, x) in xLines:
        if x:
            plt.vlines(x, opts.ymin, YMID1, colors=COLOR, alpha=ALPHA)
            plt.text(x, YMID2, lbl,
                     fontsize=16,
                     rotation='vertical',
                     horizontalalignment='center',
                     verticalalignment='center',
                     color=COLOR,
                     alpha=ALPHA)
            plt.vlines(x, YMID3, opts.ymax, colors=COLOR, alpha=ALPHA)

    plt.xlabel('Working Set (bytes)')
    plt.ylabel('MFlops')
    
    ax.set_xscale('log', basex=10)
    ax.set_yscale('linear')

    if not opts.xmin and not opts.xmax and not opts.ymin and not opts.ymax:
        ax.set_autoscale_on(True)
    else:
        ax.set_autoscale_on(False)
        ax.set_xlim(xmin=opts.xmin, xmax=opts.xmax)
        ax.set_ylim(ymin=opts.ymin, ymax=opts.ymax)
    
    leg = plt.legend(loc=opts.legendLoc, numpoints=1)

    if opts.sortLegendAt:
        handles, labels = ax.get_legend_handles_labels()

        def f((_, l)):
            (numBytes, flops) = labeledData[l]
            for i in range(0, len(numBytes)):
                if numBytes[i] == opts.sortLegendAt:
                    return flops[i]

            return None

        temp = zip(handles, labels)
        temp.sort(key=f, reverse=True)

        newHandles = [h for (h,_) in temp]
        newLabels = [l for (_,l) in temp]
        
        leg = ax.legend(newHandles, newLabels, loc=opts.legendLoc, numpoints=1)

    leg.legendPatch.set_alpha(0.0)
    
    if opts.textColor:
        ax.xaxis.label.set_color(opts.textColor)
        ax.yaxis.label.set_color(opts.textColor)
        ax.tick_params(axis='x', colors=opts.textColor)
        ax.tick_params(axis='y', colors=opts.textColor)

#
# Mac Keynote colors I use
#

DARKRED    = '#cc0000'
DARKBLUE   = '#0000cc'
SEAGREEN   = '#009e73'
DARKYELLOW = '#f0e442'

LIGHTGREY = '#414141'

# Excel color scheme
# From:
#   http://www.gilliganondata.com/index.php/2009/06/18/data-visualization-that-is-color-blind-friendly-excel-2007/
excelColors = ["#4572A7",
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
#
#   E24A33 : orange
#   7A68A6 : purple
#   348ABD : blue
#   188487 : turquoise
#   A60628 : red
#   CF4457 : pink
#   467821 : green

ORANGE    = '#E24A33'
PURPLE    = '#7A68A6'
BLUE      = '#348ABD'
TURQUOISE = '#188487'
RED       = '#A60628'
PINK      = '#CF4457'
GREEN     = '#467821'

saneColors = [BLUE,
              PURPLE,
              RED,
              GREEN,
              PINK,
              TURQUOISE,
              ORANGE]

# saneColors = ["#A60628",
#               "#348ABD",
#               "#467821",
#               "#E24A33",
#               "#188487",
#               "#CF4457",
#               "#7A68A6"]

plotColors = saneColors

def matplotConfig(opts):
    if opts.gill:
        font = {'family' : 'Gill Sans Std',
                'style'  : 'normal', 
                'weight' : 'light'}
        rc('font', **font)
    elif opts.palatino:
        rc('text', usetex=True)
        rc('text.latex', preview=True)
        rc('text.latex', preamble='\usepackage{mathpazo}')
        font = {'family' : 'Palatino',
                'style'  : 'normal', 
                'weight' : 'normal'}
        rc('font', **font)
    elif opts.segoe:
        font = {'family' : 'Segoe UI',
                'style'  : 'normal', 
                'weight' : 'normal'}
        rc('font', **font)
    else:
        rc('text', usetex=True)
        rc('text.latex', preview=True)
        rc('text.latex', preamble='\usepackage{mathptmx}')
        font = {'family' : 'Times New Roman',
                'style'  : 'normal', 
                'weight' : 'normal'}
        rc('font', **font)

    if not opts.fontsize:
        opts.fontsize = 20

    if not opts.legendfontsize:
        opts.legendfontsize = 16

    rc('legend', fontsize=opts.legendfontsize)
    rc('axes',   labelsize=opts.fontsize)
    rc('xtick',  labelsize=opts.fontsize)
    rc('ytick',  labelsize=opts.fontsize)

    rc('lines', antialiased=True)
    rc('figure', figsize=(12,9))

    if opts.textColor:
        rc('text', color=opts.textColor)

if __name__ == '__main__':
    main()
