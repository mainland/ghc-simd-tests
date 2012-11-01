#!/usr/bin/env python
import logging

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
        
        plotBench(opts, 'sum', data)
        if opts.output:
            plt.savefig(opts.output, dpi=144)
        else:
            plt.show()
    elif opts.dotp:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','tmean','tmax','tmin','tstddev'])
        
        plotBench(opts, 'dotp', data)
        if opts.output:
            plt.savefig(opts.output, dpi=144)
        else:
            plt.show()
    elif opts.pardotp:
        data = mlab.csv2rec(filepath,
                            comments='#',
                            names=['func','variant','n','m','tmean','tmax','tmin','tstddev'])

        plotParBench(opts, 'dotp', data)
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
    parser.add_option("--dotp",
                      action="store_true", dest="dotp")
    parser.add_option("--par-dotp",
                      action="store_true", dest="pardotp")
    parser.add_option("--sec",
                      action="store_true", dest="sec")
    parser.add_option("--multivector",
                      action="store_true", dest="multivector")
    parser.add_option("--alts",
                      action="store_true", dest="alts")
    parser.add_option("--alt4",
                      action="store_true", dest="alt4")
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
    
def plotBench(opts, func, data):
    plt.clf()
    plt.cla()
    
    ax = plt.subplot(111)
    ax.set_xscale('log', basex=2)
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=512, xmax=2**25)
    ax.set_ylim(ymin=10**(-4), ymax=100)

    CS = itertools.cycle(saneColors)
    MR = itertools.cycle(['+', '',   '.', '1',  '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])
    MS = itertools.cycle([10,   7,   10,  10,   10])

    sets = [('scalar',  'Scalar (Haskell)'),
            ('cscalar', 'Scalar (C)'),
            ('manual',  'Manual (Haskell)'),
            ('cmanual', 'Manual (C)'),
            ('vector',  'vector')]
    
    if opts.multivector:
        sets += [('multivector', 'multivector')]
    
    if opts.alts:
        sets += [('vectoralt1', 'vector (alt 1)'),
                 ('vectoralt2', 'vector (alt 2)'),
                 ('vectoralt3', 'vector (alt 3)'),
                 ('vectoralt4', 'vector (alt 4)'),
                 ]
    
    if opts.alt4:
        sets += [('vectoralt4', 'vector (alt 4)'),
                 ]

    for (f, label) in sets:
        r = selectField(data, func, "variant", f)
        if opts.sec:
            t = r.tmean
        else:
            t = r.tmean*1000.0
             
        plt.errorbar(r.n, t,
                     color=CS.next(),
                     marker=MR.next(),
                     linestyle=LS.next(),
                     markersize=MS.next(),
                     label=label)

    plt.legend(loc='upper left', numpoints=1)
    plt.xlabel('Vector size (bytes)')
    if opts.sec:
        plt.ylabel('Time (sec)')
    else:
        plt.ylabel('Time (ms)')

def plotParBench(opts, func, data):
    plt.clf()
    plt.cla()
    
    ax = plt.subplot(111)
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=0, xmax=max(data.m) + 1)
    ax.set_ylim(ymin=10, ymax=1500)

    CS = itertools.cycle(saneColors)
    MR = itertools.cycle(['+', '',   '.', '1',  '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])
    MS = itertools.cycle([10,   7,   10,  10,   10])

    sets = [('cmanual',    'C (SIMD)'),
            ('vectoralt4', 'vector (SIMD)'),
            ('dph',        'DPH'),
            ('dphmulti',   'DPH (SIMD)'),
            ('dphpa',      'DPH (Parallel Array)')]

    for (f, label) in sets:
        r = selectField(data, func, "variant", f)
        if opts.sec:
            t = r.tmean
            terr = r.tstddev
        else:
            t = r.tmean*1000.0
            terr = r.tstddev*1000.0
            
        plt.errorbar(r.m, t,
                     yerr=terr,
                     color=CS.next(),
                     marker=MR.next(),
                     linestyle=LS.next(),
                     markersize=MS.next(),
                     label=label)
        
    plt.legend(loc='upper right', numpoints=1)
    #legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    plt.xlabel('Threads')
    if opts.sec:
        plt.ylabel('Time (sec)')
    else:
        plt.ylabel('Time (ms)')

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

def matplotConfig():
    rc('text', usetex=True)
    rc('text.latex', preview=True)
    rc('text.latex', preamble='\usepackage{times}')
    rc('font',**{'family':'serif','serif':['Times']})

    FONTSIZE=16
    LEGEND_FONTSIZE=14

    rc('axes', labelsize=FONTSIZE)
    rc('legend', fontsize=LEGEND_FONTSIZE)
    rc('xtick',  labelsize=FONTSIZE)
    rc('ytick',  labelsize=FONTSIZE)

    rc('lines', antialiased=True)

if __name__ == '__main__':
    main()

