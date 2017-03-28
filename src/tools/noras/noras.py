

import sys
import re

if len(sys.argv) != 2:
    print "wrong number of args to noras.py!"

RET_INSTR = "\tretq\n"
# CALL_PATTERN = re.compile("^\tcallq\t(.+)$")
# LABEL_PATTERN = re.compile("^([a-zA-Z0-9_]+):$")

RET_REPLACEMENT = "\tpopq\t%rbp\n\tjmpq\t*%rbp\n"

# MANTICORE_FUN_PAT = re.compile(".+_[ABCDEF0-9]{4}$")

def output(s):
    sys.stdout.write(s)

# we don't want to emit push;jmp for calls into the GC
# since we use callq for that in CPS
# def isBanished(lab):
#     matchesManticoreFun = re.match(MANTICORE_FUN_PAT, lab)
#     return (matchesManticoreFun is None)

def replace(content):
    for i in range(0, len(content)):
        line = content[i]
        
        if line.startswith(RET_INSTR):
            output(RET_REPLACEMENT)
            continue
            
        output(line)
        
        ####
        # it turns out that you can't emit a push of a 64-bit immediate, so our hope
        # of cheaply emitting a pushq afterLab ; jmp callTarg  is hopeless.
        # luckily we're still not using the RAS if we never emit a ret.
        ####

        # callResult = re.match(CALL_PATTERN, line)
        # if callResult is None:
        #     output(line)
        #     continue
        # 
        # callTarg = callResult.group(1)
        # 
        # if isBanished(callTarg):
        #     output(line)
        #     continue
        # 
        # # result matched a call, check if there's a label after it too
        # nextLine = content[i+1]
        # labelResult = re.match(LABEL_PATTERN, nextLine)
        # if labelResult is None:
        #     output(line)
        #     continue
        #     
        # afterLab = labelResult.group(1)
        # 
        # jmpKind = ''
        # if callTarg.startswith("*"):
        #     jmpKind = "\tjmpq\t"
        # else:
        #     jmpKind = "\tjmp\t"
        # 
        # # emit a push ; jmp
        # pushAddr = "\tpushq\t" + afterLab + "\n"
        # jmpInstr = jmpKind + callTarg + "\n"
        # 
        # output(pushAddr)
        # output(jmpInstr)
        
        
    

with open(sys.argv[1], 'r') as orig:
    content = orig.readlines()
    replace(content)
