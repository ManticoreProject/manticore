# Adam Shaw, November 2009
# run this with 'python speedup.py'

# Please see EDIT ME! below for customization.

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import utils
import rope_splitting_experiment as r
import collect_data as get
import human_readable as h
import speedups as spds

for experiment_id in spds.ids:
    (maxtime, bl_avg, mlt_avg, mltlbsovhd, lbs_ovhd, norms)=r.compare_wall_clock(experiment_id, 48)
    problem_name=get.problem_name_of_experiment(experiment_id)
    problem_name=r.pretty_bench_name(problem_name)
    (input, ps, lbs_avg, lbs_std)=filter(lambda (input, ps, avg, err): r.is_lbs1(input), norms)[0]
    lbs_speedup=bl_avg / lbs_avg
    print (problem_name + " & " + h.speedup(mlt_avg, digits=2) + " & " + h.speedup(bl_avg,digits=2) + " & " + h.speedup(lbs_ovhd,digits=2) + " & " + h.speedup(lbs_avg,digits=2) + " & " 
           + h.speedup(lbs_speedup,digits=2)
           + "\\\\")
