# Adam Shaw, November 2009
# run this with 'python speedup.py'

# Please see EDIT ME! below for customization.

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import collect_data
import utils
import pylab

pylab.rcParams['xtick.major.pad']='16'
pylab.rcParams['ytick.major.pad']='16'



### Cosmetics

h1 = fm.FontProperties()
h1.set_size(60)

h2 = fm.FontProperties()
h2.set_size(72)

h3 = fm.FontProperties()
h3.set_size(76)

h4 = fm.FontProperties()
h4.set_size(52)

for h in [h1, h2, h3]:
  h.set_family('Times New Roman')

# generates a big list of distinct format strings
# pre: len(colors) == len(shapes) - 1
# if precondition isn't met, this iteration pattern doesn't necessarily work!
def mash(colors, shapes):
  clen = len(colors)
  slen = len(shapes)
  assert clen == (slen - 1)
  retval = []
  for i in range(0, clen*slen):
    c = colors[i % clen]
    s = shapes[i % slen]
    retval.append(c + s)
    i = i+1
  return retval     

fmts = mash(['r', 'g', 'b', 'k', 'm', 'r'], 
            ['d', 'v', 'o', '^', 's', 'p', 'h'])

lcs=['b', 'g', 'r', 'b', 'g', 'r', 'b', 'b', 'g', 'r', 'b', 'g', 'r', 'b']

fmt_index = 0 

# next_fmt : _ -> string
# cycles around the global list fmts, providing the next fmt string in it
# maintains a stateful index into that list
# side effect: increments that index on each call,
#   wrapping around (to 0) at len(fmts)
def next_fmt():
  global fmts
  global fmt_index
  f = fmts[fmt_index]
  fmt_index = (fmt_index + 1) % len(fmts)
  return f

# reset_fmts : _ -> _
def reset_fmts():
  global fmt_index
  fmt_index = 0

### Utilities

### Plotting

# # stdev_adhockery
# def stdev_adhockery(pars):
#   indexed_medians = utils.medians(pars)
#   stdevs = utils.stdevs(pars)
#   assert(len(indexed_medians) == len(stdevs))
#   for i in range(0, len(stdevs)):
#     mx, my = indexed_medians[i]
#     dx, dy = stdevs[i]
#     assert(mx == dx)
#     my_str  = "%.6f" % my
#     dy_str  = "%.6f" % dy
#     pct_str = "%.6f" % (dy/my)
#     print (str(mx) + '\t' + my_str + '\t' + dy_str + '\t' + pct_str)

# errorbars : ((int, float) list, (int, float) list) -> _
def errorbars(spss, devss):
  assert len(spss) == len(devss)
  foo = ['lazy', 'eager', 'flat']
  for i in range(0, len(spss)):
    print foo[i]
    sps  = spss[i]
    devs = devss[i]
    assert len(sps) == len(devs)
    for j in range(0, len(sps)):
      (sx, sy) = sps[j]
      (dx, dy) = devs[j]
      assert sx == dx
      plt.errorbar(sx, sy, yerr=dy, ecolor='black')

# plot : string * (string, float, (int, float) list) list -> _
# args:
# - a filename to output the plot,
# and each item in the list of triples is
# - a title,
# - a list of lines
def plot(filename, 
         lines,
         biggestX,
         biggestY,
         dashes,
         connect_dots=False,
         chart_title='Speedups', 
         xax_label='number of processors', 
         yax_label='speedup',
         legend_loc='lower center',
         linecolors=lcs,
         formats=fmts,
         marker=None, # (markersize, markeredgewidth)
         dimensions=None, # (width, height) in inches
         heightIn=None,
         prefix=None,
         show_errorbars=False,
         xaxlabs=None,
         yaxvals=None
         ):
  # size of the figure if dimensions are specified
  if (dimensions != None):
    plt.figure(figsize=dimensions)
  axes = plt.gca()
  axes.set_xlim([-1.0, biggestX + 1.0])
  axes.set_ylim([0.0, biggestY + 10.5])
  axes.xaxis.set_ticks_position('bottom') # as opposed to 'top' or 'both'
  plt.title(chart_title, fontproperties=h1)
  plt.xlabel(xax_label, fontproperties=h2)
  if (yaxvals == None):
    yaxvals=np.arange(0.0, biggestY, 10.0)
  xaxvals=np.arange(0., biggestX + 1.0)
  if (xaxlabs != None):
    plt.xticks (xaxvals, xaxlabs, fontproperties=h1)
  else:
    plt.xticks(xaxvals, fontproperties=h1)
  plt.ylabel(yax_label, fontproperties=h2)
  plt.yticks(yaxvals, fontproperties=h1)
  # accumulators
  legend_text  = []
  speedupsList = []
  stdevsList   = []
  # if formats are given, use them:
  fmtIdx = 0
  # plot each speedup curve and accumulate
  for title, xypairs in lines:
    xs, ys = utils.unzip(xypairs)
    f = formats[fmtIdx]
    f = f[1]
    fmtIdx = (fmtIdx + 1) % len(formats)
    if connect_dots:
      if f.endswith('-'):
        pass
      else:
        f = f + "-"
    if dashes[fmtIdx]:
      linestyle='dashed'
    else:
      linestyle='solid'
    if (marker == None):
      plt.plot(xs, ys, f, linestyle=linestyle, markeredgecolor=linecolors[fmtIdx])
    else:
      sz, wd = marker
      plt.plot(xs, ys, f, 
               markersize=sz, 
               color=linecolors[fmtIdx],
               markeredgewidth=wd,
               linestyle=linestyle, 
               antialiased=True,
               linewidth=3.9)
    legend_text.append(title)
#    speedupsList.append(sps)
#    stdevs = utils.stdevs(pars)
#    stdevsList.append(stdevs)
  # make error bars
#  if show_errorbars:
#    errorbars(speedupsList, stdevsList)
  # build the legend
  plt.legend(legend_text, prop=h1, loc=legend_loc)
  fl = filename + '.eps'
  if (prefix != None):
    fl = prefix + '/' + fl
  plt.savefig(fl, dpi=300)
  plt.savefig(filename + '.pdf', dpi=300, transparent=True)
  print ('GENERATED and SAVED file ' + fl)
  #plt.show()

# plot ('test', [('line 1', [(1, 10.0), (2, 20.0), (3, 30.0)])],
#       biggestX=4,
#       biggestY=100.0,
#       chart_title='',
#       connect_dots=True,
#       formats=['r+', 'gx', 'b|'],
#       marker=(8.0, 1.5))
