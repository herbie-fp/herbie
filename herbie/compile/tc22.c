#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.4";

double f_if(float x) {
        float r2791 = x;
        float r2792 = 1.0;
        float r2793 = r2791 + r2792;
        float r2794 = 3.0;
        float r2795 = r2792 / r2794;
        float r2796 = pow(r2793, r2795);
        float r2797 = pow(r2791, r2795);
        float r2798 = r2796 - r2797;
        return r2798;
}

double f_id(double x) {
        double r2799 = x;
        double r2800 = 1.0;
        double r2801 = r2799 + r2800;
        double r2802 = 3.0;
        double r2803 = r2800 / r2802;
        double r2804 = pow(r2801, r2803);
        double r2805 = pow(r2799, r2803);
        double r2806 = r2804 - r2805;
        return r2806;
}


double f_of(float x) {
        float r2807 = x;
        float r2808 = 1.0;
        float r2809 = r2807 + r2808;
        float r2810 = 0.3333333333333333;
        float r2811 = pow(r2809, r2810);
        float r2812 = 3.0;
        float r2813 = pow(r2811, r2812);
        float r2814 = pow(r2807, r2810);
        float r2815 = pow(r2814, r2812);
        float r2816 = r2813 - r2815;
        float r2817 = r2811 * r2811;
        float r2818 = log(r2807);
        float r2819 = r2818 * r2810;
        float r2820 = exp(r2819);
        float r2821 = r2820 * r2820;
        float r2822 = r2811 * r2820;
        float r2823 = r2821 + r2822;
        float r2824 = r2817 + r2823;
        float r2825 = r2816 / r2824;
        return r2825;
}

double f_od(double x) {
        double r2826 = x;
        double r2827 = 7380138868922986.0;
        bool r2828 = r2826 < r2827;
        double r2829 = 1.0;
        double r2830 = r2826 + r2829;
        double r2831 = 0.3333333333333333;
        double r2832 = pow(r2830, r2831);
        double r2833 = 3.0;
        double r2834 = pow(r2832, r2833);
        double r2835 = pow(r2826, r2831);
        double r2836 = sqrt(r2835);
        double r2837 = r2836 * r2836;
        double r2838 = pow(r2837, r2833);
        double r2839 = r2834 - r2838;
        double r2840 = r2832 * r2832;
        double r2841 = log(r2826);
        double r2842 = r2841 * r2831;
        double r2843 = exp(r2842);
        double r2844 = r2843 * r2843;
        double r2845 = r2832 * r2835;
        double r2846 = r2844 + r2845;
        double r2847 = r2840 + r2846;
        double r2848 = r2839 / r2847;
        double r2849 = sqrt(r2832);
        double r2850 = r2849 + r2836;
        double r2851 = 0.04243827160493827;
        double r2852 = r2829 / r2826;
        double r2853 = 0.16666666666666666;
        double r2854 = pow(r2852, r2853);
        double r2855 = r2851 * r2854;
        double r2856 = r2826 * r2826;
        double r2857 = r2826 * r2856;
        double r2858 = r2829 / r2857;
        double r2859 = r2855 * r2858;
        double r2860 = -0.06944444444444445;
        double r2861 = r2860 * r2854;
        double r2862 = r2826 * r2826;
        double r2863 = r2829 / r2862;
        double r2864 = r2861 * r2863;
        double r2865 = r2853 * r2854;
        double r2866 = r2865 * r2852;
        double r2867 = r2864 + r2866;
        double r2868 = r2859 + r2867;
        double r2869 = r2850 * r2868;
        double r2870 = r2828 ? r2848 : r2869;
        return r2870;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2871, r2872, r2873, r2874, r2875, r2876, r2877, r2878;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2871);
        mpfr_init_set_str(r2872, "1", 10, MPFR_RNDN);
        mpfr_init(r2873);
        mpfr_init_set_str(r2874, "3", 10, MPFR_RNDN);
        mpfr_init(r2875);
        mpfr_init(r2876);
        mpfr_init(r2877);
        mpfr_init(r2878);
}

double f_im(double x) {
        mpfr_set_d(r2871, x, MPFR_RNDN);
        ;
        mpfr_add(r2873, r2871, r2872, MPFR_RNDN);
        ;
        mpfr_div(r2875, r2872, r2874, MPFR_RNDN);
        mpfr_pow(r2876, r2873, r2875, MPFR_RNDN);
        mpfr_pow(r2877, r2871, r2875, MPFR_RNDN);
        mpfr_sub(r2878, r2876, r2877, MPFR_RNDN);
        return mpfr_get_d(r2878, MPFR_RNDN);
}

static mpfr_t r2879, r2880, r2881, r2882, r2883, r2884, r2885, r2886, r2887, r2888, r2889, r2890, r2891, r2892, r2893, r2894, r2895, r2896, r2897;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2879);
        mpfr_init_set_str(r2880, "1", 10, MPFR_RNDN);
        mpfr_init(r2881);
        mpfr_init_set_str(r2882, "1/3", 10, MPFR_RNDN);
        mpfr_init(r2883);
        mpfr_init_set_str(r2884, "3", 10, MPFR_RNDN);
        mpfr_init(r2885);
        mpfr_init(r2886);
        mpfr_init(r2887);
        mpfr_init(r2888);
        mpfr_init(r2889);
        mpfr_init(r2890);
        mpfr_init(r2891);
        mpfr_init(r2892);
        mpfr_init(r2893);
        mpfr_init(r2894);
        mpfr_init(r2895);
        mpfr_init(r2896);
        mpfr_init(r2897);
}

double f_fm(double x) {
        mpfr_set_d(r2879, x, MPFR_RNDN);
        ;
        mpfr_add(r2881, r2879, r2880, MPFR_RNDN);
        ;
        mpfr_pow(r2883, r2881, r2882, MPFR_RNDN);
        ;
        mpfr_pow(r2885, r2883, r2884, MPFR_RNDN);
        mpfr_pow(r2886, r2879, r2882, MPFR_RNDN);
        mpfr_pow(r2887, r2886, r2884, MPFR_RNDN);
        mpfr_sub(r2888, r2885, r2887, MPFR_RNDN);
        mpfr_mul(r2889, r2883, r2883, MPFR_RNDN);
        mpfr_log(r2890, r2879, MPFR_RNDN);
        mpfr_mul(r2891, r2890, r2882, MPFR_RNDN);
        mpfr_exp(r2892, r2891, MPFR_RNDN);
        mpfr_mul(r2893, r2892, r2892, MPFR_RNDN);
        mpfr_mul(r2894, r2883, r2892, MPFR_RNDN);
        mpfr_add(r2895, r2893, r2894, MPFR_RNDN);
        mpfr_add(r2896, r2889, r2895, MPFR_RNDN);
        mpfr_div(r2897, r2888, r2896, MPFR_RNDN);
        return mpfr_get_d(r2897, MPFR_RNDN);
}

static mpfr_t r2898, r2899, r2900, r2901, r2902, r2903, r2904, r2905, r2906, r2907, r2908, r2909, r2910, r2911, r2912, r2913, r2914, r2915, r2916, r2917, r2918, r2919, r2920, r2921, r2922, r2923, r2924, r2925, r2926, r2927, r2928, r2929, r2930, r2931, r2932, r2933, r2934, r2935, r2936, r2937, r2938, r2939, r2940, r2941, r2942;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2898);
        mpfr_init_set_str(r2899, "7380138868922986.0", 10, MPFR_RNDN);
        mpfr_init(r2900);
        mpfr_init_set_str(r2901, "1", 10, MPFR_RNDN);
        mpfr_init(r2902);
        mpfr_init_set_str(r2903, "1/3", 10, MPFR_RNDN);
        mpfr_init(r2904);
        mpfr_init_set_str(r2905, "3", 10, MPFR_RNDN);
        mpfr_init(r2906);
        mpfr_init(r2907);
        mpfr_init(r2908);
        mpfr_init(r2909);
        mpfr_init(r2910);
        mpfr_init(r2911);
        mpfr_init(r2912);
        mpfr_init(r2913);
        mpfr_init(r2914);
        mpfr_init(r2915);
        mpfr_init(r2916);
        mpfr_init(r2917);
        mpfr_init(r2918);
        mpfr_init(r2919);
        mpfr_init(r2920);
        mpfr_init(r2921);
        mpfr_init(r2922);
        mpfr_init_set_str(r2923, "55/1296", 10, MPFR_RNDN);
        mpfr_init(r2924);
        mpfr_init_set_str(r2925, "1/6", 10, MPFR_RNDN);
        mpfr_init(r2926);
        mpfr_init(r2927);
        mpfr_init(r2928);
        mpfr_init(r2929);
        mpfr_init(r2930);
        mpfr_init(r2931);
        mpfr_init_set_str(r2932, "-5/72", 10, MPFR_RNDN);
        mpfr_init(r2933);
        mpfr_init(r2934);
        mpfr_init(r2935);
        mpfr_init(r2936);
        mpfr_init(r2937);
        mpfr_init(r2938);
        mpfr_init(r2939);
        mpfr_init(r2940);
        mpfr_init(r2941);
        mpfr_init(r2942);
}

double f_dm(double x) {
        mpfr_set_d(r2898, x, MPFR_RNDN);
        ;
        mpfr_set_si(r2900, mpfr_cmp(r2898, r2899) < 0, MPFR_RNDN);
        ;
        mpfr_add(r2902, r2898, r2901, MPFR_RNDN);
        ;
        mpfr_pow(r2904, r2902, r2903, MPFR_RNDN);
        ;
        mpfr_pow(r2906, r2904, r2905, MPFR_RNDN);
        mpfr_pow(r2907, r2898, r2903, MPFR_RNDN);
        mpfr_sqrt(r2908, r2907, MPFR_RNDN);
        mpfr_mul(r2909, r2908, r2908, MPFR_RNDN);
        mpfr_pow(r2910, r2909, r2905, MPFR_RNDN);
        mpfr_sub(r2911, r2906, r2910, MPFR_RNDN);
        mpfr_mul(r2912, r2904, r2904, MPFR_RNDN);
        mpfr_log(r2913, r2898, MPFR_RNDN);
        mpfr_mul(r2914, r2913, r2903, MPFR_RNDN);
        mpfr_exp(r2915, r2914, MPFR_RNDN);
        mpfr_mul(r2916, r2915, r2915, MPFR_RNDN);
        mpfr_mul(r2917, r2904, r2907, MPFR_RNDN);
        mpfr_add(r2918, r2916, r2917, MPFR_RNDN);
        mpfr_add(r2919, r2912, r2918, MPFR_RNDN);
        mpfr_div(r2920, r2911, r2919, MPFR_RNDN);
        mpfr_sqrt(r2921, r2904, MPFR_RNDN);
        mpfr_add(r2922, r2921, r2908, MPFR_RNDN);
        ;
        mpfr_div(r2924, r2901, r2898, MPFR_RNDN);
        ;
        mpfr_pow(r2926, r2924, r2925, MPFR_RNDN);
        mpfr_mul(r2927, r2923, r2926, MPFR_RNDN);
        mpfr_mul(r2928, r2898, r2898, MPFR_RNDN);
        mpfr_mul(r2929, r2898, r2928, MPFR_RNDN);
        mpfr_div(r2930, r2901, r2929, MPFR_RNDN);
        mpfr_mul(r2931, r2927, r2930, MPFR_RNDN);
        ;
        mpfr_mul(r2933, r2932, r2926, MPFR_RNDN);
        mpfr_mul(r2934, r2898, r2898, MPFR_RNDN);
        mpfr_div(r2935, r2901, r2934, MPFR_RNDN);
        mpfr_mul(r2936, r2933, r2935, MPFR_RNDN);
        mpfr_mul(r2937, r2925, r2926, MPFR_RNDN);
        mpfr_mul(r2938, r2937, r2924, MPFR_RNDN);
        mpfr_add(r2939, r2936, r2938, MPFR_RNDN);
        mpfr_add(r2940, r2931, r2939, MPFR_RNDN);
        mpfr_mul(r2941, r2922, r2940, MPFR_RNDN);
        if (mpfr_get_si(r2900, MPFR_RNDN)) { mpfr_set(r2942, r2920, MPFR_RNDN); } else { mpfr_set(r2942, r2941, MPFR_RNDN); };
        return mpfr_get_d(r2942, MPFR_RNDN);
}

