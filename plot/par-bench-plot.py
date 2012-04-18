#!/usr/bin/env python
import logging

import itertools
import matplotlib.mlab as mlab
from matplotlib.pyplot import *
import numpy as np
from optparse import OptionParser
import plotprefs

def selectVariant(r, func, variant):
    mask = (r["func"] == func) & (r["variant"] == variant)
    return r[mask]

def selectN(r, func, n):
    mask = (r["func"] == func) & (r["n"] == n)
    return r[mask]

def selectM(r, func, m):
    mask = (r["func"] == func) & (r["m"] == n)
    return r[mask]
    
def main():
    global opts, args

    usage = "usage: %prog [options] [mailboxes]"
    parser = OptionParser(usage=usage)
    parser.add_option("-o", "--output",
                      action="store", type="string", dest="output")
    parser.add_option("-d", "--debug",
                      action="store_true", dest="debug")
    (opts, args) = parser.parse_args()

    if opts.debug:
       level = logging.DEBUG
    else:
       level = logging.INFO

    logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s',
                        level=level)

    plotprefs.config()

    data = mlab.csv2rec(args[0],
                        comments='#',
                        names=['func','variant','n','m','tmean','tmax','tmin','tstddev'])

    if opts.output:
        plotBench('dotp', data)
        savefig(opts.output, dpi=144)
    else:
        plotBench('dotp', data)
        show()
    
def plotBench(func, data):
    clf()
    cla()
    
    ax = subplot(111)
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=0, xmax=max(data.m) + 1)
    ax.set_ylim(ymin=10, ymax=1500)

    MARKERSIZE=10

    CS = itertools.cycle(plotprefs.sane)
    MS = itertools.cycle(['+', '',  '.', '1',   '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])

    sets = [('cmanual',    'C (SIMD)'),
            ('vectoralt4', 'vector (SIMD)'),
            ('dph',        'DPH'),
            ('dphmulti',   'DPH (SIMD)'),
            ('dphpa',      'DPH (Parallel Array)')]

    for (f, label) in sets:
        r = selectVariant(data, func, f)
        errorbar(r.m, r.tmean*1000.0,
                 yerr=r.tstddev*1000.0,
                 color=CS.next(),
                 marker=MS.next(),
                 linestyle=LS.next(),
                 markersize=MARKERSIZE,
                 label=label)

    legend(loc='upper right', numpoints=1)
    #legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    xlabel('Threads')
    ylabel('Time (ms)')
    
if __name__ == '__main__':
    main()
