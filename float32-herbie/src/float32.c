#include <math.h>
#include "float32.h"

#define NULL    0

float neg_f32(float a)
{
    return -a;
}

float add_f32(float a, float b)
{
    return a + b;
}

float sub_f32(float a, float b)
{
    return a - b;
}

float mul_f32(float a, float b)
{
    return a * b;
}

// Avoid segfaulting
float div_f32(float a, float b)
{
    return (b == 0.0f) ? NAN : (a / b);
}