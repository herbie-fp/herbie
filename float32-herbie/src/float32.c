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

// Avoid error
float div_f32(float a, float b)
{
    return (b == 0.0f) ? NAN : (a / b);
}

int f32_to_ordinal(float a)
{
    int *pa;

    if (a < 0.0f)
    {
        a = 0.0f - a;
        pa = (int*)(&a);
        return -(*pa);
    }
    else
    {
        pa = (int*)(&a);
        return *pa;
    }
}

float ordinal_to_f32(int a)
{
    float *pa;

    if (a < 0)
    {
        a *= -1;
        pa = (float*)(&a);
        return -(*pa);
    }
    else
    {
        pa = (float*)(&a);
        return *pa;
    }
}