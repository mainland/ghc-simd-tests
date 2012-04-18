#!/usr/bin/env python
import logging

import matplotlib.mlab as mlab
from matplotlib.pyplot import *
import itertools
import numpy as np
from optparse import OptionParser
import plotprefs

def selectVariant(r, func, variant):
    mask = (r["func"] == func) & (r["variant"] == variant)
    return r[mask]

def selectN(r, func, n):
    mask = (r["func"] == func) & (r["n"] == n)
    return r[mask]
    
def main():
    global opts, args

    usage = "usage: %prog [options] [mailboxes]"
    parser = OptionParser(usage=usage)
    parser.add_option("-d", "--debug",
                      action="store_true", dest="debug")
    parser.add_option("--sum",
                      action="store", type="string", dest="sum")
    parser.add_option("--dotp",
                      action="store", type="string", dest="dotp")
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
                        names=['func','variant','n','tmean','tmax','tmin','tstddev'])

    if opts.sum:
        plotBench('sum', data, plotMulti=True)
        savefig(opts.sum, dpi=144)
        
    if opts.dotp:
        plotBench('dotp', data, plotMulti=True)
        savefig(opts.dotp, dpi=144)

    if False:
        plotBench('sum', data, plotMulti=True)
        show()
        
    if True:
        plotBench('dotp', data, plotMulti=True)
        show()
        
    if False:
        print"Sum"
        r = selectN(data, 'sum', 16777216)
        for i in r:
           print "|%s|%f \plusmn %f|" % (i.variant, i.tmean*1000.0, i.tstddev*1000.0)
           
        print "Dotp"
        r = selectN(data, 'dotp', 16777216)
        for i in r:
           print "|%s|%f \plusmn %f|" % (i.variant, i.tmean*1000.0, i.tstddev*1000.0)
    
def plotBench(func, data, plotMulti=False, plotAlt=False):
    clf()
    cla()
    
    ax = subplot(111)
    ax.set_xscale('log', basex=2)
    ax.set_yscale('log', basex=2)
    ax.set_autoscale_on(False)
    ax.set_xlim(xmin=512, xmax=2**25)
    ax.set_ylim(ymin=10**(-4), ymax=100)

    MARKERSIZE=10

    CS = itertools.cycle(plotprefs.sane)
    MS = itertools.cycle(['+', '',  '.', '1',   '2'])
    LS = itertools.cycle(['-', '--', '-', '--', ':'])

    sets = [('scalar', 'Scalar'),
            ('cscalar', 'Scalar (C)'),
            ('manual', 'Manual'),
            ('cmanual', 'Manual (C)'),
            ('vector', 'vector')]
    
    if plotMulti:
        sets += [('multivector', 'multivector')]
    
    if plotAlt:
        sets += [('vectoralt1', 'vector (alt 1)'),
                 ('vectoralt2', 'vector (alt 2)'),
                 ('vectoralt3', 'vector (alt 3)'),
                 ('vectoralt4', 'vector (alt 4)'),
                 ]

    for (f, label) in sets:
        r = selectVariant(data, func, f)
        errorbar(r.n, r.tmean*1000.0,
                 color=CS.next(),
                 marker=MS.next(),
                 linestyle=LS.next(),
                 markersize=MARKERSIZE,
                 label=label)

    legend(loc='upper left', numpoints=1)
    xlabel('Vector size')
    ylabel('Time (ms)')

if __name__ == '__main__':
    main()
