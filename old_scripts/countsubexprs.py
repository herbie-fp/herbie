import re
import math
import warnings

import sys
maxLength = math.inf
numberToSelect = 1000
fileName = sys.argv[1]

def subExprs(s):
    ret = []
    stack = []
    for i, c in enumerate(s):
        if c == '(':
            if len(stack) < maxLength: 
                stack.append(i)
        elif c == ')':
            if stack:
                start = stack.pop()  
                subexpr = s[start:i+1]  
                ret.append(subexpr)
    return ret

def renameVars(s):
    toReplace = list(re.findall(r" (?!binary32|binary64)[a-zA-Z_][\*.a-zA-Z0-9_-]*",s))
    toReplace = set(toReplace)
    toReplace = [i.strip() for i in toReplace]
    for i,oldVar in enumerate(toReplace):
        s = re.sub(fr' {oldVar}\b',fr' z{i}',s)
    return s

def isImpl(s):
    return ('approx' not in s) and ('.f64' in s or '.f32' in s) 

def isBasicFunction(s):
    return re.match(r'(\(\S+\.(f64|f32) z0( z1)?\))|(\(\S+\.(f64|f32) z1 z0\))',s)

lines = []
final = {}
exprs = dict()

with open(fileName) as f:
    for i,line in enumerate(f):
        # warnings.warn(i)

        for subExpr in subExprs(line):
            if(not isImpl(subExpr)): 
                continue
            if('if' in subExpr or '<' in subExpr or '=' in subExpr or '>' in subExpr): 
                continue

            renamed = renameVars(subExpr)
            if(isBasicFunction(renamed)): 
                continue

            if('z0' not in renamed):
                continue

            if(renamed not in exprs):
                exprs[renamed] = 1
            else:
                exprs[renamed] +=1

sortedExprs = [k for k, v in sorted(exprs.items(), key=lambda item: item[1])]
# print(exprs)

for i in range(max(0,len(sortedExprs)-numberToSelect), len(sortedExprs)):
    print(sortedExprs[i],',',exprs[sortedExprs[i]])