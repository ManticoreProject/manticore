# Adam Shaw Oct 2009

import numpy as np

# uniq : t list -> t list
# pre  : the list is sorted per some order
# pre  : != is well-defined on the list element type
# post : the output list is sorted in the same order
# post : the output list contains exactly one of each item present

def uniq(sorted_list):
  retval = []
  curr = sorted_list[0]
  retval.append(curr)
  for n in sorted_list:
    if n != curr:
      retval.append(n)
      curr = n
  return retval

# unzip : (a, b) list -> (a list, b list)

def unzip(pairs):
  xs = []
  ys = []
  for (x, y) in pairs:
    xs.append(x)
    ys.append(y)
  return (xs, ys)

# unzip3 : (a, b, c) list -> (a list, b list, c list)

def unzip3(triples):
  xs = []
  ys = []
  zs = []
  for (x, y, z) in triples:
    xs.append(x)
    ys.append(y)
    zs.append(z)
  return (xs, ys, zs)

# unzip3 : (a, b, c, d) list -> (a list, b list, c list, d list)

def unzip4(quads):
  xs = []
  ys = []
  zs = []
  hs = []
  for (x, y, z, h) in quads:
    xs.append(x)
    ys.append(y)
    zs.append(z)
    hs.append(h)
  return (xs, ys, zs, hs)

# cpmap : ((a, b) -> c) * a list * b list -> c list
# map a function over the cartesian product of two lists

def cpmap(f, xs, ys):
  retval = []
  for x in xs:
    for y in ys:
      retval.append(f(x,y))
  return retval

# agg_by : (value list -> value) * (index, value) list -> (index, value) list
# Given an aggregating function and a list of index, value pairs,
#   return pairs where each index appears exactly once and all values
#   for that index have been aggregated with the given function.

def agg_by(f, pairs):
  retval = []
  if len(pairs) == 0:
    return retval
  def with_index(i):
    def wix(rec):
      return rec[0] == i
    return wix
  def select_index(x):
    return x[0]
  def select_value(x):
    return x[1]
  firsts = map(select_index, pairs)
  uniqs = uniq(np.sort(firsts))
  for u in uniqs:
    curr_values = map(select_value, filter(with_index(u), pairs))
    agg = f(curr_values)
    retval.append((u, agg))
  return retval

# medians : (index, value) list -> (index, value) list

def medians(pairs):
  return agg_by(np.median, pairs)

# stdevs : (index, value) list -> (index, value) list

def stdevs(pairs):
  return agg_by(np.std, pairs)

# url_last : string -> string
# extract the last arc from the given url
# ex: url_last('https://foo/bar/baz/scott/baio') ==> 'baio'

def url_last(url):
  toks = url.split('/')
  toks.reverse()
  return(toks[0])

# find_by_url : (int * string * string) * (int * string * string) list -> int or False
# find the context id matching the given benchmark url
# the arguments are
# - a context_id, url and compiler branch for a benchmark
# - a list of context_ids, urls and compiler branches for several benchmarks
# a baseline is considered found when its url matches the par url exactly
def find_by_url(bench_url, benchs):
  for b in benchs:
    x_id, x_url, x_branch = b
    if (x_url == bench_url):
      return(x_id)
  # if you make it this far...
  return(False)
