# Adam Shaw, October 2009

import pg

# connect_r : unit -> pgobject
def connect_r():
  c = pg.connect(dbname='manticore',
                 host='liliput.cs.uchicago.edu',
                 user='manticorer',
                 passwd='Pic5shah')
  return c

# connect_rw : unit -> pgobject
# left unimplemented for now...it's a sharp (ie dangerous) tool

# select_values : string -> tuple list
# consumes a select query
# returns a list of tuples representing the result of that query
def select_values(q):
  c = connect_r()
  r = c.query(q)
  v = r.getresult()
  c.close()
  return v  

# show_select : string -> _
# consumes a select query
# prints results as tuples, line by line
def show_select(q):
  v = select_values(q)
  for rec in v:
    print rec
  print ""
