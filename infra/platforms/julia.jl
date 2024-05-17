# Compiled code from the core
println("compiled_core_code")

# Input points handling
x0 = [
    1.0,
    2.0,
    3.0
]

x1 = [
    4.0,
    5.0,
    6.0
]

# Main loop
i = 0
start = time_ns()
while i < 100
    try
        for j in i:99
            foo(x0[j+1], x1[j+1])
            i += 1
        end
    catch
        i += 1
    end
end
end_time = time_ns()
diff = (10 ^ -6) * (end_time - start)
println("$diff ms")
