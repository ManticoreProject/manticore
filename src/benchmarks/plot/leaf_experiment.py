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

def find_mc_context_ids3(experiment_id, mls):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND compiler = 'pmlc' \
       AND input LIKE '%" + str(mls) + "%'"
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
  return(p[1] == "LTS" and p[2] == "1")

def is_ebsap(input):
  p=input.split(" ")
  return(p[1] == "ebs-ap")

def is_ebsap1(input):
  p=input.split(" ")
  return(p[1] == "ebs-ap" and p[2] == "1")

def is_ebssp(input):
  p=input.split(" ")
  return(p[1] == "ETS")

def is_ok_ebsap(input):
  if is_ebsap(input):
    p=input.split(" ")
    return(p[3] == "4")
  else:
    return(True)

def param_of_mls(input):
  p=input.split(" ")
  return p[1]

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

def stat_of_context_id(context_id):
    q = "SELECT AVG(runs.time_sec), STDDEV(runs.time_sec) \
         FROM runs \
         INNER JOIN contexts ON contexts.context_id = runs.context_id \
         WHERE contexts.context_id = " + str(context_id) 
    return(db.select_values(q)[0])

def stat_of_par_context_id(context_id, n_procs):
    q = "SELECT AVG(runs.time_sec), STDDEV(runs.time_sec) \
         FROM runs \
         INNER JOIN contexts ON contexts.context_id = runs.context_id \
         WHERE contexts.context_id = " + str(context_id) + "\
         AND   runs.n_procs = " + str(n_procs)
    return(db.select_values(q)[0])

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

def compare_normalized (experiment_id, n_procs):
  pts=[]
  for strat in splitting_strategies:
    ctx_ids=filter(get.is_context_parallel, find_mc_context_ids(experiment_id, strat))
    for ctx_id in ctx_ids:
      input=input_of_context_id(ctx_id)
      (avg,stddev)=stat_of_par_context_id(ctx_id, n_procs)
      if is_ok_ebsap(input):
        pts.append((input, '', (avg, stddev)))
  pts.sort(compare_pts)
  (binput, bps, (bavg, bstddev))=pts.pop(0)
  norms=[]
  # get baseline performance
  baseline_id=find_mc_context_ids(experiment_id, splitting_strategies[4])[0]
  (blavg,blstddev)=stat_of_context_id(baseline_id)
  # get performance of sequential ellision of lbs(1) 
  ids=filter(get.is_context_sequential, find_mc_context_ids(experiment_id, splitting_strategies[2]))
  ids=filter(get.is_context_manticore,ids)
  (lbs_se_avg,_)=stat_of_context_id(ids[0])
  (mlt_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[4])[0])
  lbs_ovhd=(lbs_se_avg-blavg) / blavg
#  lbs_ovhd=(lbs_se_avg / blavg) - 1.0
  # get performance of each other strategy
  for (input, ps, (avg, stddev)) in pts:
    error=(bavg + stddev) / bavg
    speedup=blavg / avg
    mltspeedup=mlt_avg / avg
    norms.append((input, ps, (1.0 - (bavg / avg), error - 1.0), speedup, mltspeedup))
  bspeedup=blavg / bavg
  mltspeedup=mlt_avg / bavg
  (mlton_lbs_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[2])[0])
  mltlbsovhd=(mlton_lbs_avg-mlt_avg) / mlt_avg
#  mltlbsovhd=(mlton_lbs_avg / mlt_avg) - 1.0 
  berror=((bavg + bstddev) / bavg) - 1.0
  seqpmlcvsmlton=mlt_avg / blavg
  return ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)


def print_pt(pt):
    print (pt[1]),
    print "\t\t\t",
    (navg,nstddev)=pt[2]
    print(human_readable.percent(navg) + "%\t\t\t" + human_readable.percent(nstddev) + "%\t\t\t" + human_readable.speedup(pt[3]) + "\t\t\t" + human_readable.speedup(pt[4]))

def print_compare_normalized (experiment_id, n_procs):
  problem_name=get.problem_name_of_experiment(experiment_id)
  print ("Problem: " + problem_name)
  print ("Number of Cores: " + str(n_procs))
  ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)=compare_normalized(experiment_id, n_procs)
  print ("Best splitting strategy: " + bps + " (Error= " + human_readable.percent(berror) + "%) (Speedup vs. SeqPMLC= " + human_readable.speedup(bspeedup) + ") (Speedup vs. mlton= " + human_readable.speedup(mltspeedup) + ")")
  print ("Overhead of LBS vs. NS (PMLC)= " + human_readable.percent(lbs_ovhd)) + '%'
  print ("Overhead of LBS vs. NS (mlton)= " + human_readable.percent(mltlbsovhd))  + '%' 
  print ("Sequential PMLC vs. mlton (both NS)= " + human_readable.percent(seqpmlcvsmlton) + "%")
  print("Splitting strategy\t\tPct. slower\t\tError\t\t\tSpeedup vs. SeqPMLC\tSpeedup vs. mlton")
  for norm in norms:
    print_pt(norm)

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
  for strat in splitting_strategies:
    ctx_ids=filter(get.is_context_parallel, find_mc_context_ids(experiment_id, strat))
    for ctx_id in ctx_ids:
      input=input_of_context_id(ctx_id)
      (avg,stddev)=stat_of_par_context_id(ctx_id, n_procs)
      maxtime=max(maxtime,avg)
      if is_ok_ebsap(input):
        pts.append((input, pretty_strategy(strat, input), (avg, stddev)))
  pts.sort(compare_pts)
  norms=[]
  # get baseline performance
  baseline_id=find_mc_context_ids(experiment_id, splitting_strategies[4])[0]
  (blavg,blstddev)=stat_of_context_id(baseline_id)
  # get performance of sequential ellision of lbs(1) 
  ids=filter(get.is_context_sequential, find_mc_context_ids(experiment_id, splitting_strategies[2]))
  ids=filter(get.is_context_manticore,ids)
  (lbs_se_avg,_)=stat_of_context_id(ids[0])
  (mlt_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[4])[0])
  lbs_ovhd=lbs_se_avg
#(lbs_se_avg-blavg) / blavg
#(lbs_se_avg / blavg) - 1.0
  # get performance of each other strategy
  for (input, ps, (avg, stddev)) in pts:
    norms.append((input, ps, avg, stddev))
  (mlton_lbs_avg,_)=stat_of_context_id(find_mlton_context_ids(experiment_id, splitting_strategies[2])[0])
  mltlbsovhd=(mlton_lbs_avg-mlt_avg) / mlt_avg
#(mlton_lbs_avg / mlt_avg) - 1.0 
  return ((maxtime, blavg, mlt_avg, mltlbsovhd, lbs_ovhd, norms))

leaf_sizes=(0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384)

def ets (experiment_id, n_procs):
  pts=[]
  maxtime=0.0
  for leaf_size in leaf_sizes:
   ctx_ids=filter(get.is_context_parallel, find_mc_context_ids3(experiment_id,leaf_size))
   for ctx_id in ctx_ids:
     input=input_of_context_id(ctx_id)
     (avg,stddev)=stat_of_par_context_id(ctx_id, n_procs)
     maxtime=max(maxtime,avg)
     if is_ok_ebsap(input):
       pts.append((input, '', (avg, stddev)))
  pts.sort(compare_pts)
  norms=[]
  # get baseline performance
  baseline_id=find_mc_context_ids(experiment_id, splitting_strategies[4])[0]
  (blavg,blstddev)=stat_of_context_id(baseline_id)
  for (input, ps, (avg, stddev)) in pts:
    norms.append((input, ps, avg, stddev))
  return ((maxtime, blavg, norms))

#ids=(772, 773, 774, 775, 776, 777, 778, 779)
#ids=(781, 782, 784, 785, 786, 787, 807)
#ids=(808, 809, 810, 811, 812, 813, 814, 815)
#ids=(837,838,841,844,845,846,847)

#ids=(848,849,850,851,852,853,854)

#ids=(902,903,904,905,906)

#ids=(918,919,920,921,922,923)

ids=(924,925,926,927,928,929)

# for id in ids:
#   print_compare_normalized(id, n_procs)

def plotall(name, strats, legend_loc):
  lines=[]
  maxX=0.0
  maxY=0.0
  for experiment_id in ids:
    problem_name=get.problem_name_of_experiment(experiment_id)
    #print problem_name
    (maxtime, bl_avg, norms)=ets(experiment_id, n_procs)
    for strat in strats:
      lab=pretty_bench_name(problem_name)
      ns=norms
      xys=[]
      for (input, pinput, avg, stddev) in ns:
        if is_ebsap(input):
          s=param_of_strategy(input)
          (k,v)=s.split(",")
          x=float(k)
        else:
          x=int(param_of_mls(input))
          x=float(math.log(x,2))
        y=(bl_avg / avg)
        #print (lab, avg, y, bl_avg)
        xys.append((x,y))
        maxX=max(x,maxX)
        maxY=max(y,maxY)
      xys.sort(cmp)
      lines.append((lab,xys))
  yaxvals=np.arange(0, 49, 8)
  xaxlabs=[]
  for i in range (0,15):
    xaxlabs.append(('$2^{'+str(i)+'}$'))
  line_plot.plot('mls'+name, lines, 14, 49.0,chart_title='',connect_dots=True,
                 #formats=['r+', 'b+', 'r-', 'b-', 'rx', 'bx', 'r^', 'b^', 'ro', 'bo'],
                 linecolors=['#000000', 'r', 'b', 'g', 'b', 'r', 'b', 'r', 'g', 'r', 'b', 'g', 'r', 'b'],
                 formats=['b+', 'b,', 'b.', 'b1', 'b2', 'b3', 'b4', 'b<', 'b>', 'b|'],
                 dashes=[False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
                 yax_label='speedup',
                 xax_label='$M$',
                 dimensions=(35,23),
                 legend_loc=legend_loc,
                 marker=(43.0, 4.0),
                 yaxvals=yaxvals,
                 xaxlabs=xaxlabs)


#plotall(splitting_strategies[1][0]+"-"+str(n_procs), [splitting_strategies[1]], 'upper right')
#plotall(splitting_strategies[2][0]+"-"+str(n_procs), [splitting_strategies[2]], 'upper right')

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

plotall(splitting_strategies[0][0]+"-"+str(n_procs), [splitting_strategies[0]], 'upper right')

# #speedup vs pmlc
# print("\\begin{tabular}{l | r}")
# print("Program & T_{MLton} & T_{Seq} & T_{16} & Speedup (T_{Seq} / T_{16}) \\\\\\hline")
# for experiment_id in ids:
#   problem_name=get.problem_name_of_experiment(experiment_id)
#   problem_name=pretty_bench_name(problem_name)
#   print(problem_name + " & "),
#   ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)=compare_normalized(experiment_id)
#   if is_lbs1(binput):
#     speedup=bspeedup
#   else:
#     for norm in norms:
#       if is_lbs1(norm[0]):
#         speedup=norm[3]
#   print(human_readable.speedup(speedup) + "\\\\")
# print("\\end{tabular}")

# #speedup vs mlton
# print("\\begin{tabular}{l | r}")
# print("Program & Speedup \\\\\\hline")
# for experiment_id in ids:
#   problem_name=get.problem_name_of_experiment(experiment_id)
#   problem_name=pretty_bench_name(problem_name)
#   print(problem_name + " & "),
#   ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)=compare_normalized(experiment_id)
#   if is_lbs1(binput):
#     speedup=mltspeedup
#   else:
#     for norm in norms:
#       if is_lbs1(norm[0]):
#         speedup=norm[4]
#   print(human_readable.speedup(speedup) + "\\\\")
# print("\\end{tabular}")

# #lbs overhead pmlc
# print("\\begin{tabular}{l | r}")
# print("Benchmark & Speedup \\\\\\hline")
# for experiment_id in ids:
#   problem_name=get.problem_name_of_experiment(experiment_id)
#   problem_name=pretty_bench_name(problem_name)
#   print(problem_name + " & "),
#   ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)=compare_normalized(experiment_id)
#   print(human_readable.percent(lbs_ovhd) + "\\%\\\\")
# print("\\end{tabular}")

# #lbs overhead mlton
# print("\\begin{tabular}{l | r}")
# print("Benchmark & Speedup \\\\\\hline")
# for experiment_id in ids:
#   problem_name=get.problem_name_of_experiment(experiment_id)
#   problem_name=pretty_bench_name(problem_name)
#   print(problem_name + " & "),
#   ((binput, bps, bspeedup, mltspeedup, mltlbsovhd, seqpmlcvsmlton, berror, lbs_ovhd, (bavg, bstddev)), norms)=compare_normalized(experiment_id)
#   print(human_readable.percent(mltlbsovhd) + "\\%\\\\")
# print("\\end{tabular}")

