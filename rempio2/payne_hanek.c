#include <stdint.h>
#include <math.h>

// ============================================================================
// INJECTED CONSTANTS (Populate these during library synthesis)
// ============================================================================

// 1. The Inverse Period Table: Binary expansion of 1 / P.
// Must be generated to at least ~1150 bits (approx. 18 uint64_t words).
// The first bit of the first word corresponds to the 2^-1 place.
extern const uint64_t INV_PERIOD_TABLE[];

// 2. The Target Period P (Represented as a Double-Double)
// PERIOD_HI is the standard double-precision representation of P.
// PERIOD_LO is the residual error (P_true - PERIOD_HI), bounded by 2^-54.
extern const double PERIOD_HI;
extern const double PERIOD_LO;

// ============================================================================
// REDUCTION ALGORITHM
// ============================================================================

typedef struct {
    double remainder;
    int quadrant;
} ReductionResult;

ReductionResult custom_payne_hanek(double x) {
    ReductionResult result;
    
    // Extract IEEE 754 bits
    union { double f; uint64_t i; } bits = { .f = x };
    uint64_t sign = bits.i & 0x8000000000000000ULL;
    bits.i ^= sign; // Absolute value
    
    int exponent = ((bits.i >> 52) & 0x7FF) - 1023;
    uint64_t mantissa = (bits.i & 0xFFFFFFFFFFFFFULL) | 0x0010000000000000ULL;

    // Fast-path bypass placeholder: 
    // If exponent is small enough, use Cody-Waite or simple fmod here.
    if (exponent < 0) {
        result.remainder = x;
        result.quadrant = 0;
        return result;
    }

    // --- 1. The Sliding Window (128-bit extraction) ---
    int word_idx  = exponent / 64;
    int bit_shift = exponent % 64;

    // Fetch 3 words (192 bits) to guarantee we can form a fully aligned 128-bit window
    uint64_t t0 = INV_PERIOD_TABLE[word_idx];
    uint64_t t1 = INV_PERIOD_TABLE[word_idx + 1];
    uint64_t t2 = INV_PERIOD_TABLE[word_idx + 2];

    uint64_t w_high, w_low;
    
    // Prevent Undefined Behavior: C standards do not allow shifting by 64
    if (bit_shift == 0) {
        w_high = t0;
        w_low  = t1;
    } else {
        w_high = (t0 << bit_shift) | (t1 >> (64 - bit_shift));
        w_low  = (t1 << bit_shift) | (t2 >> (64 - bit_shift));
    }

    // --- 2. High-Precision Multiplication ---
    // Multiply the 53-bit mantissa by the 128-bit window.
    // We treat the 128-bit window as (w_high * 2^64 + w_low).
    unsigned __int128 prod_high = (unsigned __int128)mantissa * w_high;
    unsigned __int128 prod_low  = (unsigned __int128)mantissa * w_low;

    // Add the carry from the low product to the high product
    prod_high += (prod_low >> 64);

    // --- 3. Quadrant & Fraction Extraction ---
    // The integer part sits in the top 64 bits of prod_high.
    // The highly accurate fraction sits in the bottom 64 bits.
    uint64_t integer_part = (uint64_t)(prod_high >> 64);
    uint64_t fraction_bits = (uint64_t)prod_high;

    result.quadrant = integer_part & 3; // Assuming you need a 4-quadrant split

    // Convert the 64-bit integer fraction into a double in [0.0, 1.0)
    double f = (double)fraction_bits / 18446744073709551616.0;

    // --- 4. Centering ---
    // Shift the range from [0, 1) to [-0.5, 0.5] for optimal polynomial evaluation
    if (f >= 0.5) {
        f -= 1.0;
        result.quadrant = (result.quadrant + 1) & 3;
    }

    // --- 5. Double-Double Reconstruction ---
    // Multiply the accurate fraction back by the target period.
    // f * (PERIOD_HI + PERIOD_LO)
    double reduced = (f * PERIOD_HI) + (f * PERIOD_LO);

    // Restore sign
    union { double f; uint64_t i; } res_bits = { .f = reduced };
    res_bits.i ^= sign;
    
    result.remainder = res_bits.f;
    return result;
}