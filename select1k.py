with open("subexprs") as f:
    lines = f.readlines()
    for i in range(len(lines)-1000,len(lines)):
        print(lines[i].strip())