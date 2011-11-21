#!/usr/bin/python

# Written by Mike Rainey (bionic.oldsmobile@gmail.com)

import collect_data as get
import speedup as s
import utils
import matplotlib.pyplot as plt
import connect_manticore_db as db
import human_readable
import line_plot
import math
import sys
import numpy as np
import branches

n_procs=48

# if len(sys.argv) != 2:
#   sys.exit("expected an integer argument for the number of procs")

# n_procs=int(sys.argv[1])

splitting_strategies = [
#  command-line       long desc       has parameter
  ("ETS",          "ETS",       True),
  ("ebs-ap",          "ETS_AP",       True),
  ("LTS",             "LTS   ",       True),
  ("lps",             "LTS_P   ",       True),
  ("SEQ",              "SEQ    ",       False) 
]

baseline_strategy=("SEQ",              "SEQ",      False)

def find_mc_context_ids(experiment_id, strat):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND compiler = 'pmlc' \
       AND input LIKE '%" + strat[0] + "%'"
  return(get.detup(db.select_values(q)))

def find_mc_context_ids3(experiment_id):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND compiler = 'pmlc'"
  return(get.detup(db.select_values(q)))

#       AND max_leaf_size = " + str(mls) + " \

def find_mc_context_ids2(experiment_id, strat, mls):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND compiler = 'pmlc' \
       AND input LIKE '%-max-leaf-size " + strat[0] + "%'"
  return(get.detup(db.select_values(q)))

def find_mlton_context_ids(experiment_id, strat):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND compiler = 'mlton' \
       AND input LIKE '%" + strat[0] + "%'"
  return(get.detup(db.select_values(q)))

def id_of_strategy(input):
  p=input.split(" ")
  return(p[1])

def is_lbs1(input):
  p=input.split(" ")
  return(p[1] == "lbs" and p[2] == "1")

def is_ebsap(input):
  p=input.split(" ")
  return(p[1] == "ebs-ap")

def is_ebsap1(input):
  p=input.split(" ")
  return(p[1] == "ebs-ap" and p[2] == "1")

def is_ebssp(input):
  p=input.split(" ")
  return(p[1] == "ebs-sp")

def is_ok_ebsap(input):
  if is_ebsap(input):
    p=input.split(" ")
    return(p[3] == "4")
  else:
    return(True)

def max_leaf_size_of_input(input):
  p=input.split(" ")
  return float(p[1])

def param_of_strategy(input):
  p=input.split(" ")
  if is_ebsap(input):
    return(p[2] + "," + p[3])
  else:
    return(p[2])
  
def pretty_strategy(strat, cl):
  if (strat[2]):
      return(strat[1] + "(" + param_of_strategy(cl) + ")")
  else:
      return(strat[1] + "()")

def input_of_context_id(context_id):
  q = "SELECT input FROM contexts \
       WHERE context_id = " + str(context_id)
  return(db.select_values(q))[0][0]

def max_leaf_size_of_context_id(context_id):
  q = "SELECT max_leaf_size FROM contexts \
       WHERE context_id = " + str(context_id)
  return(db.select_values(q))[0][0]

def stat_of_context_id(context_id):
    q = "SELECT AVG(runs.time_sec), STDDEV(runs.time_sec) \
         FROM runs \
         INNER JOIN contexts ON contexts.context_id = runs.context_id \
         WHERE contexts.context_id = " + str(context_id) 
    return(db.select_values(q)[0])

def stat_of_seq_context_id(context_id):
    q = "SELECT AVG(runs.time_sec), STDDEV(runs.time_sec) \
         FROM runs \
         INNER JOIN contexts ON contexts.context_id = runs.context_id \
         WHERE contexts.context_id = " + str(context_id)
    return(db.select_values(q)[0])

def stat_of_par_context_id(context_id, n_procs, branch):
    q = "SELECT AVG(runs.time_sec), STDDEV(runs.time_sec) \
         FROM runs \
         INNER JOIN contexts ON contexts.context_id = runs.context_id \
         WHERE contexts.context_id = " + str(context_id) + " \
         AND   runs.n_procs = " + str(n_procs)
    return(db.select_values(q)[0])

def branch_of_context_id(context_id):
  q = "SELECT compiler_src_url \
       FROM contexts where context_id = " + str(context_id)
  url=db.select_values(q)[0][0]
  if url == branches.SWP.url():
    return(branches.SWP)
  else:
    return(branches.Trunk)

def compare_pts(pt1, pt2):
    if(pt1[2][0] < pt2[2][0]):
        return -1
    elif(pt1[2][0] == pt2[2][0]):
        return 0
    else:
        return 1

def ff (x):
  if x < 1430:
    return(True)
  else:
    return(False)



def print_pt(pt):
    print (pt[1]),
    print "\t\t\t",
    (navg,nstddev)=pt[2]
    print(human_readable.percent(navg) + "%\t\t\t" + human_readable.percent(nstddev) + "%\t\t\t" + human_readable.speedup(pt[3]) + "\t\t\t" + human_readable.speedup(pt[4]))


def cmp(x,y):
  if x[0] < y[0]:
    return -1
  elif x[0] == y[0]:
    return 0
  else:
    return 1

def compare_wall_clock (experiment_id, n_procs):
  pts=[]
  maxtime=0.0
  ctx_ids=filter(get.is_context_parallel, find_mc_context_ids3(experiment_id))
  print 'ctx_ids'
  print ctx_ids
  for ctx_id in ctx_ids:
    input=input_of_context_id(ctx_id)
    max_leaf_size=max_leaf_size_of_input(input)
    (avg,stddev)=stat_of_par_context_id(ctx_id, n_procs, branches.SWP)
    branch=branch_of_context_id(ctx_id)
    pts.append((branch, input, '', max_leaf_size, (avg, stddev)))
    print avg
    print branch
    maxtime=max(maxtime,avg)
  print 'pts'
  print pts
#  pts.sort(compare_pts)
  norms=[]
  # get baseline performance
  print experiment_id
  baseline_id=find_mc_context_ids3(experiment_id)[0]
  (blavg,blstddev)=stat_of_seq_context_id(baseline_id)
  print 'blavg'
  print blavg
  print norms
  # get performance of sequential ellision of lbs(1) 
  ids=filter(get.is_context_sequential, find_mc_context_ids3(experiment_id))
  ids=filter(get.is_context_manticore,ids)
#  (lbs_se_avg,_)=stat_of_context_id(ids[0])
  lbs_se_avg=0.0
  #(mlt_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[4])[0])
  mlt_avg=0.1
  lbs_ovhd=lbs_se_avg
#(lbs_se_avg-blavg) / blavg
#(lbs_se_avg / blavg) - 1.0
  # get performance of each other strategy
  for (branch, input, ps, max_leaf_size, (avg, stddev)) in pts:
    norms.append((branch, input, ps, avg, stddev, max_leaf_size))
#  (mlton_lbs_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[2])[0])
  mlton_lbs_avg=0.0
  mltlbsovhd=(mlton_lbs_avg-mlt_avg) / mlt_avg
#(mlton_lbs_avg / mlt_avg) - 1.0 
  return ((maxtime, blavg, mlt_avg, mltlbsovhd, lbs_ovhd, norms))

#ids=(772, 773, 774, 775, 776, 777, 778, 779)
#ids=(781, 782, 784, 785, 786, 787, 807)
#ids=(808, 809, 810, 811, 812, 813, 814, 815)
#ids=(837,838,841,844,845,846,847)

#ids=(885, 886, 887, 888, 889, 890, 891)

ids=(924,925,926,927)

#ids=(885, 886)

def plotall(name, leaf_sizes, legend_loc):
  lines=[]
  maxX=0.0
  maxY=0.0
  for experiment_id in ids:
    problem_name=get.problem_name_of_experiment(experiment_id)
    #print problem_name
    (maxtime, bl_avg, mlt_avg, mltlbsovhd, lbs_ovhd, norms)=compare_wall_clock(experiment_id, n_procs)
    lab=problem_name
    ns=norms
    xys=[]
    print problem_name
    print ns
    print bl_avg
    print 'n_procs'
    print n_procs
    for (branch, input, pinput, avg, stddev, max_leaf_size) in ns:
      if max_leaf_size == 500:
        x=math.log(512, 2)
      else:
        x=math.log(max_leaf_size, 2)
      y=(bl_avg / avg)
      xys.append((x,y))
      maxX=max(x,maxX)
      maxY=max(y,maxY)
    xys.sort(cmp)
    print xys
    lines.append((lab,xys))
  yaxvals=np.arange(0, 49, 8)
  xaxlabs=[]
  for i in range (0,13):
    xaxlabs.append(('$2^{'+str(i)+'}$'))
  line_plot.plot(name, lines, maxX, 48,chart_title='',connect_dots=True,
                 #formats=['r+', 'b+', 'r-', 'b-', 'rx', 'bx', 'r^', 'b^', 'ro', 'bo'],
                 formats=['b+', 'b,', 'b.', 'b1', 'b2', 'b3', 'b4', 'b<', 'b>', 'b|'],
                 dashes=[False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
                 yax_label='speedup',
                 xax_label='leaf size',
                 dimensions=(35,23),
                 legend_loc=legend_loc,
                 yaxvals=yaxvals,
                 xaxlabs=xaxlabs,
                 marker=(43.0, 4.0))


#plotall(splitting_strategies[1][0]+"-"+str(n_procs), [splitting_strategies[1]], 'upper right')

plotall('leaf-size-48', [1, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192], 'upper right')

benchmark_names = [
  ("barnes-hut", "Barnes Hut"),
  ("dense-matrix-multiply", "DMM"),
  ("nested-sums", "Nested Sums"),
  ("quicksort", "Quicksort"),
  ("smvm", "SMVM"),
  ("tree-rootfix", "Tree Rootfix"),
  ("id-raytracer", "Raytracer")
]

def pretty_bench_name(n):
  for nam in benchmark_names:
    if nam[0] == n:
      return nam[1]
