# Adam Shaw, Nov 2009
# based on sample from www.5dollarwhitebox.org/drupal/node/84

# units : (int * string) list
# each pair is the power of 2 for each unit and its one-letter abbreviation
units = [(80, 'Y'), # yottabytes 
         (70, 'Z'), # zettabytes
         (60, 'E'), # exabytes
         (50, 'P'), # petabytes
         (40, 'T'), # terabytes
         (30, 'G'), # gigabytes
         (20, 'M'), # megabytes
         (10, 'k')] # kilobytes

# bytes : int -> string
# only abbreviates up to yottabytes :)
# digits is the number of digits to the right of the decimal point
# ex: bytes(1234567890) => '1.15G'
# ex: bytes(1234567890, digits=1) => '1.1G'
def bytes(b, digits=2):
  global units
  bytes = float(b)
  for exp, abbrev in units:
    sz = 2 ** exp
    if (bytes >= sz):
      fmt = '%.' + str(digits) + 'f' + abbrev
      return(fmt % (bytes / sz))
    # end if
  # end for
  fmt = '%.' + str(digits) + 'f' + 'B'
  return(fmt % bytes) # plain ol' bytes

def percent(p, digits=1):
  p=float(p)*100.0
  fmt='%.' + str(digits) + 'f'
  return(fmt % p)

def speedup(p, digits=1):
  p=float(p)
  fmt='%.' + str(digits) + 'f'
  return(fmt % p)
