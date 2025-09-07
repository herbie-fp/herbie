import sys
fileName = sys.argv[1]

with open(file=fileName) as f:
    lines = list(f)

for i, line in enumerate(lines):
    splitLine = line.split(',')
    expr = splitLine[0].strip()
    count = splitLine[1].strip()
    expr = expr.replace(".f32","").replace(".f64","").replace("#s(literal","").replace("binary64)","").replace("binary32)","").replace("(PI)","PI")
    print("(FPCore (",end="")
    for j in range(100):
        if("z"+str(j) in expr):
            print("z"+str(j)+ " ",end="")
    print(") ",end="")
    print(expr.strip().replace("neg","-")+') ,'+count)