
import matplotlib as mpl
mpl.use('Agg')

import pylab

from scipy import *
#from scipy import special
#from scipy import stats
from numpy import random


def plot_gMega():

    f = open('gMega.csv','r')
    w = []
    v = []
    for l in f:
        r = l.split(';')
        w.append(r[0])
        v.append(r[1])



    #n=100
    #mu_x = 0.0
    #sig_x = pi * (4.91/3600.) / 180

    #x = random.normal(mu_x, sig_x, n)

    fig = pylab.figure(figsize=(10,10))
    ax1 = fig.add_subplot(111)

    #cnt, bins, ptchs = ax1.hist(x,lw=3, histtype='step',normed=True,color='r', label=r'$x$')
    #print "cnt = %s" % (repr(cnt))
    #print "bins = %s" % (repr(bins))
    #x_gauss = 1/(sig_x*sqrt(2*pi)) * exp(- (bins - mu_x)**2 / (2 * sig_x**2) )
    ax1.plot(w, v, 'g--', lw=3, label='g')

    ax1.set_xlabel(r"$x$", size='x-large')
    ax1.set_ylabel(r'$N$', size='x-large')
    for i in range(len(ax1.get_xticklabels())):
        ax1.get_xticklabels()[i].set_size('x-large')
    for i in range(len(ax1.get_yticklabels())):
        ax1.get_yticklabels()[i].set_size('x-large')
    ##ax1.set_ylim(ymin=0,ymax=10)
    ax1.grid(True)
    #ax1.legend(loc='upper right')
    pylab.savefig("gMega.eps",dpi=600, bbox_inches='tight')
    print "display gMega.eps &"

def plot_Z():

    z = arange(0,10,0.1)
    Z = exp(-z**2/4)*special.iv(0,z**2/4)

    fig = pylab.figure(figsize=(10,10))
    ax1 = fig.add_subplot(111)

    ax1.plot(z, Z, 'b--', lw=3)
    ax1.set_xlabel(r'$z$', size='x-large')
    ax1.set_ylabel(r'$f_Z(z)$', size='x-large')
    for i in range(len(ax1.get_xticklabels())):
        ax1.get_xticklabels()[i].set_size('x-large')
    for i in range(len(ax1.get_yticklabels())):
        ax1.get_yticklabels()[i].set_size('x-large')
    #ax1.set_ylim(ymin=0,ymax=10)
    ax1.grid(True)
    pylab.savefig("z_Z.eps",dpi=600, bbox_inches='tight')
    print "display z_Z.eps"

def main():
    #plot_Y()
    plot_gMega()
    #plot_Z()

if __name__ == "__main__":
    main()
