# Adam Shaw, Nov 2009

prefix = 'https://smlnj-gforge.cs.uchicago.edu/svn/manticore/'

class Branch:
  def __init__(self, suffix, pretty):
    self.suffix = suffix
    self.pretty = pretty
  def __repr__(self):
    return(self.suffix)
  def url(self):
    global prefix
    return (prefix + self.suffix)
  def pretty_name(self):
    return(self.pretty)

# These are essentially constants to be used in other modules.
SWP = Branch('branches/swp', 'improved')
FlatHeap = Branch('branches/flat-heap', 'unified')
Trunk = Branch('trunk', 'partitioned')
