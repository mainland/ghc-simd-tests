#!/usr/bin/env python
import logging

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

    if True:
        plotBench('sum', data, plotMulti=True)
        show()
        
    if False:
        plotBench('dotp', data, plotMulti=True)
        show()
        
    if True:
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

    r = selectVariant(data, func, 'scalar')
    errorbar(r.n, r.tmean*1000.0,
             fmt='r+-',
             markersize=MARKERSIZE,
             label='Scalar')
    
    r = selectVariant(data, func, 'cscalar')
    errorbar(r.n, r.tmean*1000.0,
             fmt='b--',
             markersize=MARKERSIZE,
             label='Scalar (C)')
    
    r = selectVariant(data, func, 'manual')
    errorbar(r.n, r.tmean*1000.0,
             fmt='r.-',
             markersize=MARKERSIZE,
             label='Manual')
    
    r = selectVariant(data, func, 'cmanual')
    errorbar(r.n, r.tmean*1000.0,
             fmt='b1--',
             markersize=MARKERSIZE,
             label='Manual (C)')
    
    r = selectVariant(data, func, 'vector')
    errorbar(r.n, r.tmean*1000.0,
             fmt='g2:',
             markersize=MARKERSIZE,
             label='vector')
    
    if plotMulti:
        r = selectVariant(data, func, 'multivector')
        errorbar(r.n, r.tmean*1000.0,
                 fmt='k-',
                 markersize=MARKERSIZE,
                 label='multivector')
    
    if plotAlt:
        r = selectVariant(data, func, 'vectoralt1')
        errorbar(r.n, r.tmean*1000.0,
                 fmt='r.-.',
                 markersize=MARKERSIZE,
                 label='vector (alt 1)')
        
        r = selectVariant(data, func, 'vectoralt2')
        errorbar(r.n, r.tmean*1000.0,
                 fmt='g.-.',
                 markersize=MARKERSIZE,
                 label='vector (alt 2)')
        
        r = selectVariant(data, func, 'vectoralt3')
        errorbar(r.n, r.tmean*1000.0,
                 fmt='b.-',
                 markersize=MARKERSIZE,
                 label='vector (alt 3)')

        
    legend(loc='upper left', numpoints=1)
    xlabel('Vector size')
    ylabel('Time (ms)')

if __name__ == '__main__':
    main()
