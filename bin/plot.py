#!/usr/bin/env python
import logging
import math

import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
from matplotlib import rc

import itertools
import numpy as np

from optparse import OptionParser
    
def main():
    (opts, filepath) = getOpts()

    matplotConfig()
    
    if opts.sum:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','tmean','tmax','tmin','tstddev'])
        
        if opts.ratio:
            plotSeqPerformanceRatio(opts, 'sum', data)
        else:
            plotSeqTime(opts, 'sum', data)
    elif opts.dotp:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','tmean','tmax','tmin','tstddev'])

        if opts.ratio:
            plotSeqPerformanceRatio(opts, 'dotp', data)
        else:
            plotSeqTime(opts, 'dotp', data)
    elif opts.rbf:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','tmean','tmax','tmin','tstddev'])

        if opts.ratio:
            plotRbfPerformanceRatio(opts, 'rbf', data)
        else:
            plotSeqTime(opts, 'rbf', data)
    elif opts.pardotp:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','m','tmean','tmax','tmin','tstddev'])

        if opts.ratio:
            plotParPerformanceRatio(opts, 'dotp', data)
        else:
            plotParTime(opts, 'dotp', data)
            
    if opts.output:
        plt.savefig(opts.output, dpi=144)
    else:
        plt.show()

def getOpts():
    usage = "usage: %prog [options] FILE"
    parser = OptionParser(usage=usage)
    parser.add_option("-d", "--debug",
                      action="store_true", dest="debug")
    parser.add_option("--sum",
                      action="store_true", dest="sum")
    parser.add_option("--rbf",
                      action="store_true", dest="rbf")
    parser.add_option("--dotp",
                      action="store_true", dest="dotp")
    parser.add_option("--par-dotp",
                      action="store_true", dest="pardotp")
    parser.add_option("--ratio",
                      action="store_true", dest="ratio")
    parser.add_option("--sec",
                      action="store_true", dest="sec")    
    parser.add_option("--nsets",
                      action="store", type="int", dest="nsets")
    parser.add_option("--xmin",
                      action="store", type="string", dest="xmin")
    parser.add_option("--ymax",
                      action="store", type="string", dest="ymax")
    parser.add_option("--ymin",
                      action="store", type="string", dest="ymin")
    parser.add_option("-o", "--output",
                      action="store", type="string", dest="output")
    (opts, args) = parser.parse_args()

    if opts.debug:
       level = logging.DEBUG
    else:
       level = logging.INFO

    logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s',
                        level=level)

    if len(args) != 1:
        parser.error("Must specify data file")

    filepath = args[0]

    return (opts, filepath)

def selectField(r, func, lbl, v):
    mask = (r["func"] == func) & (r[lbl] == v)
    return r[mask]
    
def plotTime(opts, func, data, sets, x):
    plt.clf()
    plt.cla()
    
    ax = plt.subplot(111)

    CS = itertools.cycle(plotColors)
    MR = itertools.cycle(['+', '',   '.', '1',  '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])
    MS = itertools.cycle([10,   7,   10,  10,   10])

    for (f, label) in sets:
        r = selectField(data, func, "variant", f)
        if opts.sec:
            t = r.tmean
        else:
            t = r.tmean*1000.0

        plt.errorbar(r[x], t,
                     color=CS.next(),
                     marker=MR.next(),
                     linestyle=LS.next(),
                     markersize=MS.next(),
                     label=label)

    if opts.sec:
        plt.ylabel('Time (sec)')
    else:
        plt.ylabel('Time (ms)')

    return ax
    
def plotSeqTime(opts, func, data):
    sets = [('scalar',  'Vector library (scalar)'),
            ('vector',  'Vector library (SSE)'),
            ('cscalar', 'Hand-written C (scalar)'),
            ('cmanual', 'Hand-written C (SSE)'),
            ('cblas',   'Goto BLAS 1.13')]

    ax = plotTime(opts, func, data, sets, "n")
    
    ax.set_xscale('log', basex=2)
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=2**12*1.5, xmax=2**24*1.5)
    ax.set_ylim(ymin=10**(-3), ymax=60)
    
    plt.legend(loc='upper left', numpoints=1)
    plt.xlabel('Vector size (elements)')

def plotParTime(opts, func, data):
    sets = [('cmanual',  'C (SIMD)'),
            ('vector',   'Vector library (SIMD)'),
            ('dph',      'DPH'),
            ('dphmulti', 'DPH (SIMD)')]
    
    ax = plotTime(opts, func, data, sets, "m")
    
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=0, xmax=max(data.m) + 1)
    ax.set_ylim(ymin=10, ymax=1500)
    
    plt.legend(loc='upper right', numpoints=1)
    #legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    plt.xlabel('Threads')

def plotSeqPerformanceRatio(opts, func, data):
    CS = itertools.cycle(plotColors)
    HS = itertools.cycle(['//', 'xx', '..'])
    
    sets = [('cmanual', 'Hand-written C (SSE)'),
            ('vector',  'Vector library (SSE)'),
            ('cblas',   'Goto BLAS 1.13')]

    BASELINE = 'cmanual'
    
    baseline = selectField(data, func, 'variant', BASELINE)
    baseline_t = baseline.tmean
    
    plt.clf()
    plt.cla()

    numGroups = len(baseline)
    groupSize = len(sets)

    if opts.nsets:
        sets = sets[0:opts.nsets]
    
    # the x locations for the groups
    ind = np.arange(numGroups)
    # the width of the bars
    width = 0.7
    
    ax = plt.subplot(111)

    ax.set_ylabel('Vector size (elements)')
    ax.set_ylim(ymin=0.5, ymax=1.2)
    
    ax.set_xticks(ind+width)
    ax.set_xticklabels( ["$2^{%d}$" % int(math.log(r.n,2)) for r in baseline] )

    off = 0.0

    for (f, label) in sets:
        r = selectField(data, func, 'variant', f)
        t = r.tmean
        tstddev = r.tstddev

        ax.bar(ind+off, t/baseline_t, width/groupSize,
               color=CS.next(),
               #hatch=HS.next(),
               ecolor='k',
               yerr=tstddev/baseline_t,
               label=label)
        off += width/groupSize

    plt.legend(loc='upper right', numpoints=1)
    plt.xlabel('Vector size (elements)')
    plt.ylabel('Execution Time Ratio')

    

def plotRbfPerformanceRatio(opts, func, data):
    CS = itertools.cycle(plotColors)
    HS = itertools.cycle(['//', 'xx', '..'])
    
    sets = [('cmanual', 'Goto BLAS 1.13'),
            ('scalar',  'Vector library'),
            ('vector',  'Vector library (SSE)')]

    BASELINE = 'cmanual'
    
    baseline = selectField(data, func, 'variant', BASELINE)
    baseline_t = baseline.tmean
    
    plt.clf()
    plt.cla()

    numGroups = len(baseline)
    groupSize = len(sets)

    if opts.nsets:
        sets = sets[0:opts.nsets]
    
    # the x locations for the groups
    ind = np.arange(numGroups)
    # the width of the bars
    width = 0.7
    
    ax = plt.subplot(111)

    ax.set_ylabel('Vector size (elements)')
    ax.set_ylim(ymin=0.4, ymax=1.7)
    
    ax.set_xticks(ind+width)
    ax.set_xticklabels( ["$2^{%d}$" % int(math.log(r.n,2)) for r in baseline] )

    off = 0.0

    for (f, label) in sets:
        r = selectField(data, func, 'variant', f)
        t = r.tmean
        tstddev = r.tstddev

        ax.bar(ind+off, t/baseline_t, width/groupSize,
               color=CS.next(),
               #hatch=HS.next(),
               ecolor='k',
               yerr=tstddev/baseline_t,
               label=label)
        off += width/groupSize

    plt.legend(loc='upper right', numpoints=1)
    plt.xlabel('Vector size (elements)')
    plt.ylabel('Execution Time Ratio')

def plotParPerformanceRatio(opts, func, data):
    CS = itertools.cycle(plotColors)

    sets = [('cmanual',  'Hand-written C (SSE)'),
            ('vector',   'Vector library (SSE)'),
            ('dph',      'DPH'),
            ('dphmulti', 'DPH (SSE)')]

    BASELINE = 'cmanual'
    
    baseline = selectField(data, func, 'variant', BASELINE)
    baseline = baseline[::-1]
    baseline_t = baseline.tmean
    
    plt.clf()
    plt.cla()

    numGroups = len(baseline)
    groupSize = len(sets)
    
    # the x locations for the groups
    ind = np.arange(numGroups)
    # the width of the bars
    width = 0.7
    
    ax = plt.subplot(111)

    ax.set_ylabel('Vector size (elements)')
    if opts.ymin:
        ymin = float(opts.ymin)
    else:
        ymin = 0.5
    if opts.ymax:
        ymax = float(opts.ymax)
    else:
        ymax = 1.12   
    ax.set_ylim(ymin=ymin, ymax=ymax)
    
    ax.set_xticks(ind+width)
    ax.set_xticklabels( [r.m for r in baseline] )

    off = 0.0

    for (f, label) in sets:
        r = selectField(data, func, 'variant', f)
        r = r[::-1]
        t = r.tmean
        tstddev = r.tstddev

        c = CS.next()
        ax.bar(ind+off, t/baseline_t, width/groupSize,
               color=c,
               ecolor='k',
               yerr=tstddev/baseline_t,
               label=label)
        off += width/groupSize

    plt.legend(loc='upper right', numpoints=1)
    plt.xlabel('Number of Threads')
    plt.ylabel('Execution Time Ratio')

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
saneColors = ["#348ABD",
              "#7A68A6",
              "#A60628",
              "#467821",
              "#CF4457",
              "#188487",
              "#E24A33"]

plotColors = saneColors

def matplotConfig():
    rc('text', usetex=True)
    rc('text.latex', preview=True)
    rc('text.latex', preamble='\usepackage{times}')
    rc('font',**{'family':'serif','serif':['Times']})

    FONTSIZE=20
    LEGEND_FONTSIZE=16

    rc('legend', fontsize=LEGEND_FONTSIZE)
    rc('axes',   labelsize=FONTSIZE)
    rc('xtick',  labelsize=FONTSIZE)
    rc('ytick',  labelsize=FONTSIZE)

    rc('lines', antialiased=True)

if __name__ == '__main__':
    main()
