import mpmath

def generate_payne_hanek_constants(period_func, name="2PI_3"):
    # Set working precision to 500 decimal places (approx 1660 bits).
    # This is more than enough to safely generate 1150 bits of the inverse table.
    mpmath.mp.dps = 500
    
    # Evaluate the arbitrary-precision period
    P = period_func()
    
    # ---------------------------------------------------------
    # 1. Generate Double-Double Representation (PERIOD_HI & LO)
    # ---------------------------------------------------------
    # Cast to standard IEEE 754 double (Python's native float)
    p_hi_float = float(P)
    
    # Cast back to mpmath to perform exact high-precision subtraction
    # This isolates the exact residual error bounded by 2^-54
    p_lo_float = float(P - mpmath.mpf(p_hi_float))
    
    # ---------------------------------------------------------
    # 2. Generate Inverse Period Table (1 / P)
    # ---------------------------------------------------------
    inv_P = 1 / P
    
    # We ONLY want the fractional part. The integer part of 1/P 
    # only contributes to the integer part of the final Payne-Hanek 
    # multiplication, which is dropped anyway. Extracting only the 
    # fraction ensures the MSB of table[0] perfectly aligns with 2^-1.
    frac_inv_P = inv_P - mpmath.floor(inv_P)
    
    # We need ~1150 bits to cover standard double precision range.
    # 18 words * 64 bits/word = 1152 bits.
    num_words = 18
    table = []
    
    for _ in range(num_words):
        # Shift radix point 64 bits to the right
        frac_inv_P *= 2**64
        # Extract the integer part as the next 64-bit word
        chunk = int(mpmath.floor(frac_inv_P))
        table.append(chunk)
        # Remove the extracted bits, leaving the remaining fraction
        frac_inv_P -= chunk
        
    # ---------------------------------------------------------
    # 3. Output C Code
    # ---------------------------------------------------------
    print(f"// =========================================================")
    print(f"// Payne-Hanek Constants for: {name}")
    print(f"// Generated using arbitrary precision arithmetic (mpmath)")
    print(f"// =========================================================\n")
    
    # Use hex floats (.hex()) to guarantee bit-perfect transfer to C
    print(f"// Target Period P")
    print(f"const double PERIOD_HI = {p_hi_float.hex()}; // {p_hi_float}")
    print(f"const double PERIOD_LO = {p_lo_float.hex()}; // {p_lo_float}\n")
    
    print(f"// 1/P Fractional Binary Expansion (1152 bits)")
    print("const uint64_t INV_PERIOD_TABLE[] = {")
    for i, word in enumerate(table):
        print(f"    0x{word:016X}ULL, // Word {i:02d}")
    print("};")

if __name__ == "__main__":
    # Define your arbitrary period here. 
    # Using a lambda ensures it is evaluated with the high mp.dps context.
    period = lambda: 2 * mpmath.mp.pi / 3
    
    generate_payne_hanek_constants(period, name="2*PI / 3")