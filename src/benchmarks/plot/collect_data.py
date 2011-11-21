# Adam Shaw, October 2009

import sys
import connect_manticore_db as db
import numpy as np
import utils
 
# baseline_times : int -> float list
# TODO: when the resultset is empty, the context_id is probably ill chosen
#   inform the user of this in some nice way
def baseline_times(context_id):
  q = "SELECT time_sec \
       FROM runs \
       WHERE n_procs=0 \
       AND context_id='" + str(context_id) + "'"
  return(db.select_values(q))

# parallel_times : int -> (int, float) list
# TODO: when the resultset is empty, the context_id is probably ill chosen
#   inform the user of this in some nice way
def parallel_times(context_id):
  q = "SELECT n_procs, time_sec \
       FROM runs \
       WHERE n_procs>0 \
       AND context_id = '" + str(context_id) + "'"
  v = db.select_values(q)
  if (len(v) == 0):
    print("no parallel times found for context id " + context_id)
    print("halting")
    sys.exit(1)
  return v

# med_time : int -> float
def med_time(context_id):
  ts = baseline_times(context_id)
  m = np.median(ts)
  return m

# med_baseline_time : int -> float
# median basline time, given a context id
def med_baseline_time(context_id):
  ts = baseline_times(context_id)
  # note: np.median returns a seq of length 1
  med = np.median(ts)  
  # assert len(med) == 1
  return med

# med_parallel_times : int -> (int, float) list
# median parallel times for all numbers of procs
def med_parallel_times(context_id):
  ts = parallel_times(context_id)
  m = utils.medians(ts)
  return m

# show_problem_by_compiler : string * string -> unit
def show_problem_by_compiler(prob, comp):
  print (prob + " by " + comp + ":")
  q = "SELECT context_id, language, compiler, seq_compilation, datetime, bench_url \
       FROM contexts \
       WHERE bench_url LIKE '%" + prob + "%' \
       AND compiler LIKE '%" + comp + "%' \
       ORDER BY datetime ASC"
  db.show_select(q)

# show_parallel_contexts : string -> unit
def show_parallel_contexts(problem_name):
  print ("parallel " + problem_name + ":")
  q = "SELECT context_id, language, seq_compilation, datetime, bench_url \
       FROM contexts \
       WHERE bench_url LIKE '%" + problem_name + "%' \
       AND NOT(seq_compilation) \
       ORDER BY datetime ASC"
  db.show_select(q)

# show_seq_elisions : string -> unit
def show_seq_elisions(problem):
  print ("sequential elisions of " + problem + ":")
  q = "SELECT context_id, language, seq_compilation, datetime, bench_url \
       FROM contexts \
       WHERE bench_url LIKE '%" + problem + "%' \
       AND seq_compilation \
       AND language='Manticore' \
       ORDER BY datetime ASC"
  db.show_select(q)

# Tool to choose the most recent runs of each benchmark for each branch.

# NOTE: These queries are fragile and depend on some assumptions:
# - the set of distinct benchmark programs is those whose
#   url includes "benchmarks/programs" 
# - there are three branches (incl. the trunk as a branch) -- exactly these:
#     https://smlnj-gforge.cs.uchicago.edu/svn/manticore/branches/flat-heap
#     https://smlnj-gforge.cs.uchicago.edu/svn/manticore/branches/swp
#     https://smlnj-gforge.cs.uchicago.edu/svn/manticore/trunk
# These branch names live in the module branches.py.

# detup : singleton tuple list -> list
# FIXME I'm getting lists of 1-tuples for certain return values
#   and I have to select the data out of each one tuple.
#   This seems unidiomatic, but I'm just trying to get the code to run.
def detup(singletons):
  retval = []
  for s in singletons:
    retval.append(s[0])
  return retval

# distinct_bench_urls : _ -> string list
def distinct_bench_urls():
  q = "SELECT DISTINCT(bench_url) \
       FROM contexts \
       WHERE bench_url LIKE '%benchmarks/prog%'"
  return(detup(db.select_values(q)))

# distinct_n_procs : int -> int list
# takes a context id and returns a list of distinct values of n_procs
# used by the runs corresponding to the context
def distinct_n_procs(context_id):
  q = "SELECT DISTINCT(n_procs) \
       FROM runs \
       WHERE context_id = " + str(context_id)
  return(map(int, detup(db.select_values(q))))

# most_recent : string * string * bool -> int
# grabs most recent with default problem size (i.e. empty input)
def most_recent(bench, branch, seq_elision):
  seq = "seq_compilation"
  if (not(seq_elision)):
    seq = "NOT(" + seq + ")"
  q = "SELECT context_id \
       FROM contexts \
       WHERE bench_url='" + bench + "' \
       AND compiler_src_url='" + branch.url() + "' \
       AND " + seq + " \
       AND input='' \
       ORDER BY datetime DESC \
       LIMIT 1;"
  v = detup(db.select_values(q))
  if len(v) == 0:
    return False
  elif len(v) == 1:
    return v[0]
  else:
    raise Exception("too many")

def most_recent_seq(bench, branch):
  return(most_recent(bench, branch, True))

def most_recent_par(bench, branch):
  return(most_recent(bench, branch, False))

# all_most_recent : string * bool -> (int * string * string) list
def all_most_recent(branch, seq):
  retval = []
  bs = distinct_bench_urls()
  for bench in bs:
    p = most_recent(bench, branch, seq)
    if (p == False):
      # do nothing
      z = 0
    else:
      retval.append((p, bench, branch))
  return retval  

# most_recent_pars : string -> (int * string * string) list
def most_recent_pars(branch):
  return(all_most_recent(branch, False))

# most_recent_seqs: string -> (int * string * string) list
def most_recent_seqs(branch):
  return(all_most_recent(branch, True))

# most_recent_mlton : unit -> (int, string, 'mlton') list
def most_recent_mlton():
  retval = []
  bs = distinct_bench_urls()
  for b in bs:
    q = "SELECT context_id \
         FROM contexts \
         WHERE bench_url='" + b + "' \
         AND compiler='mlton' \
         ORDER BY datetime DESC \
         LIMIT 1;"
    v = detup(db.select_values(q))
    if len(v) == 0:
      pass # that's a python noop
    elif len(v) == 1:
       retval.append([v[0], b, 'mlton'])
    else:
      raise Exception("too many")
  return(retval)

# most_recent_mlton : unit -> (int, string, 'mlton') list
def most_recent_smlnj():
  retval = []
  bs = distinct_bench_urls()
  for b in bs:
    q = "SELECT context_id \
         FROM contexts \
         WHERE bench_url='" + b + "' \
         AND compiler='SMLNJ' \
         ORDER BY datetime DESC \
         LIMIT 1;"
    v = detup(db.select_values(q))
    if len(v) == 0:
      pass # that's a python noop
    elif len(v) == 1:
       retval.append([v[0], b, 'mlton'])
    else:
      raise Exception("too many")
  return(retval)

# gc_stat : (string, int, int) -> (string * string)
# takes a column name from the gc stats table, a context id, and a number of processors
# and returns the average and standard deviation for that particular stat from the
# database
def gc_stat(column_name, context_id, n_procs):
  q = "SELECT AVG(" + column_name + "), STDDEV(" + column_name + ") \
       FROM gc INNER JOIN runs ON runs.run_id = gc.run_id \
       WHERE runs.context_id = " + str(context_id) + " AND runs.n_procs = " + str(n_procs)
  v = db.select_values(q)
  if (len(v) == 0):
    print ("no gc stats found for " + str(context_id) + " on nprocs= " + str(n_procs))
    print ("halting")
    sys.exit(1)
  elif (len(v) == 1):
    return v[0]
  print ("get_gc_stat: bogus record")
  sys.exit(1)

####################### tests

# print (most_recent_mlton())  

# for b in [SWP, FlatHeap, Trunk]:
#   print "pars:"
#   for rec in most_recent_pars(b):
#     print rec
#   print "seqs:"
#   for rec in most_recent_seqs(b):
#     print rec

# print "bye"

# most_recent_experiment : string -> int list
# takes a problem name and returns the experiment id of the most recent experiment
# matching the given name
def most_recent_experiment(problem_name):
  q = "SELECT experiment_id FROM experiments \
       INNER JOIN problems ON experiments.problem_id = problems.problem_id \
       WHERE problems.problem_name = '" + problem_name + "' \
       ORDER BY experiments.datetime DESC LIMIT 1"
  return(detup(db.select_values(q)))[0]

def strictIf(c, t, f):
  if c:
    return t
  else:
    return f

# most_recent_pml_bench : (string, string, string) -> int list
# takes an experiment id and a branch and a sequential compilation option (true for
# sequential and false for parallel and returns a list containing the most recent context id
def most_recent_pml_bench(experiment_id, branch, seq):
  seqStr = strictIf(seq, 'true', 'false')
  q = "SELECT context_id \
       FROM contexts \
       INNER JOIN experiments ON contexts.experiment_id = experiments.experiment_id \
       WHERE experiments.experiment_id = " + str(experiment_id) + " \
       AND contexts.compiler_src_url = '" + branch.url() + "' \
       AND contexts.seq_compilation = " + seqStr + " \
       ORDER BY experiments.datetime DESC LIMIT 1"
  res = detup(db.select_values(q))
  if (len(res) == 0):
    return False
  elif (len(res) == 1):
    return res[0]
  else:
    raise Exception("expected 0 or 1 values, got more!")

# most_recent_mlton_bench : (string, string) -> int list
# takes an experiment id and returns a list containing the most context id
def most_recent_mlton_bench(experiment_id):
  q = "SELECT context_id \
       FROM contexts \
       INNER JOIN experiments ON contexts.experiment_id = experiments.experiment_id \
       WHERE experiments.experiment_id = " + str(experiment_id) + " \
       AND contexts.compiler = 'mlton' \
       ORDER BY experiments.datetime DESC LIMIT 1"
  res = detup(db.select_values(q))
  if (len(res) == 0):
    return False
  elif (len(res) == 1):
    return res[0]
  else:
    raise Exception("expected 0 or 1 values, got more!")

# most_recent_smlnj_bench : (string, string) -> int list
# takes an experiment id and returns a list containing the most context id
def most_recent_smlnj_bench(experiment_id):
  q = "SELECT context_id \
       FROM contexts \
       INNER JOIN experiments ON contexts.experiment_id = experiments.experiment_id \
       WHERE experiments.experiment_id = " + str(experiment_id) + " \
       AND contexts.compiler = 'SMLNJ' \
       ORDER BY experiments.datetime DESC LIMIT 1"
  res = detup(db.select_values(q))
  if (len(res) == 0):
    return False
  elif (len(res) == 1):
    return res[0]
  else:
    raise Exception("expected 0 or 1 values; got more!")

# different_bench_inputs : (int, string) -> string list
# takes an experiment id and benchmark url and returns the different inputs used in the 
# benchmark. each of these inputs are distinct from all the others.
def different_bench_inputs(experiment_id, bench_url):
  q = "SELECT DISTINCT(input) FROM contexts \
       INNER JOIN experiments ON experiments.experiment_id = contexts.experiment_id \
       WHERE experiments.experiment_id = " + str(experiment_id) + " \
       AND bench_url = '" + bench_url + "'"
  return(db.select_values(q))

# different_bench_urls : int -> string list
# takes an experiment id and returns the different benchmarks used in the corresponding 
# experiment. each of these urls are distinct from all the others.
def different_bench_urls(experiment_id):
  q = "SELECT DISTINCT(bench_url) FROM contexts \
       INNER JOIN experiments ON experiments.experiment_id = contexts.experiment_id \
       WHERE experiments.experiment_id = " + str(experiment_id)
  return(db.select_values(q))

# find_context_ids : (int, string, string) -> int list
# find all the context ids corresponding to an experiment id, benchmark url, specific
# benchmark input value and compiler source url (or branch)
def find_context_ids(experiment_id, bench_url, bench_input, compiler_src_url, seq_compilation):
  q = "SELECT DISTINCT(context_id) FROM contexts \
       WHERE experiment_id = " + str(experiment_id) + " \
       AND bench_url    = '" + bench_url         + "'" + " \
       AND input        = '" + str(bench_input)       + "'" + " \
       AND compiler_src_url = '" + compiler_src_url       + "' \
       AND seq_compilation = " + seq_compilation
  return(db.select_values(q))

def problem_name_of_experiment(experiment_id):
  q = "SELECT problem_name FROM problems \
       INNER JOIN experiments ON experiments.problem_id = problems.problem_id \
       WHERE experiments.experiment_id = " + str(experiment_id)
  return(db.select_values(q)[0][0])

def is_context_sequential(context_id):
  q = "SELECT seq_compilation FROM contexts WHERE context_id = " + str(context_id)
  return(db.select_values(q)[0][0] == 't')

def is_context_parallel(context_id):
  q = "SELECT seq_compilation FROM contexts WHERE context_id = " + str(context_id)
  return(db.select_values(q)[0][0] == 'f')

def is_context_manticore(context_id):
  q = "SELECT compiler FROM contexts WHERE context_id = " + str(context_id)
  return(db.select_values(q)[0][0] == 'pmlc')

def is_context_mlton(context_id):
  q = "SELECT compiler FROM contexts WHERE context_id = " + str(context_id)
  return(db.select_values(q)[0][0] == 'mlton')
