#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.5";

double f_if(float x, float eps) {
        float r2943 = x;
        float r2944 = eps;
        float r2945 = r2943 + r2944;
        float r2946 = cos(r2945);
        float r2947 = cos(r2943);
        float r2948 = r2946 - r2947;
        return r2948;
}

double f_id(double x, double eps) {
        double r2949 = x;
        double r2950 = eps;
        double r2951 = r2949 + r2950;
        double r2952 = cos(r2951);
        double r2953 = cos(r2949);
        double r2954 = r2952 - r2953;
        return r2954;
}


double f_of(float x, float eps) {
        float r2955 = eps;
        float r2956 = -0.00012136113218730316;
        bool r2957 = r2955 < r2956;
        float r2958 = cos(r2955);
        float r2959 = x;
        float r2960 = cos(r2959);
        float r2961 = r2958 * r2960;
        float r2962 = 3.0;
        float r2963 = pow(r2961, r2962);
        float r2964 = sin(r2959);
        float r2965 = sin(r2955);
        float r2966 = r2964 * r2965;
        float r2967 = pow(r2966, r2962);
        float r2968 = r2963 - r2967;
        float r2969 = r2961 * r2961;
        float r2970 = r2966 * r2966;
        float r2971 = r2969 + r2970;
        float r2972 = r2961 * r2966;
        float r2973 = r2971 + r2972;
        float r2974 = r2968 / r2973;
        float r2975 = r2974 - r2960;
        float r2976 = 0.00010899295011768118;
        bool r2977 = r2955 < r2976;
        float r2978 = 0.3333333333333333;
        float r2979 = 4.0;
        float r2980 = pow(r2955, r2979);
        float r2981 = r2978 * r2980;
        float r2982 = -2.0;
        float r2983 = r2955 * r2959;
        float r2984 = r2982 * r2983;
        float r2985 = -1.0;
        float r2986 = r2955 * r2955;
        float r2987 = r2985 * r2986;
        float r2988 = r2984 + r2987;
        float r2989 = r2981 + r2988;
        float r2990 = r2960 + r2961;
        float r2991 = r2965 * r2964;
        float r2992 = r2990 - r2991;
        float r2993 = r2989 / r2992;
        float r2994 = log(r2969);
        float r2995 = exp(r2994);
        float r2996 = r2995 - r2970;
        float r2997 = r2961 + r2991;
        float r2998 = r2996 / r2997;
        float r2999 = r2998 - r2960;
        float r3000 = r2977 ? r2993 : r2999;
        float r3001 = r2957 ? r2975 : r3000;
        return r3001;
}

double f_od(double x, double eps) {
        double r3002 = eps;
        double r3003 = -2.0254979742508256e-16;
        bool r3004 = r3002 < r3003;
        double r3005 = cos(r3002);
        double r3006 = x;
        double r3007 = cos(r3006);
        double r3008 = r3005 * r3007;
        double r3009 = sin(r3002);
        double r3010 = sin(r3006);
        double r3011 = r3009 * r3010;
        double r3012 = -r3011;
        double r3013 = r3012 - r3007;
        double r3014 = r3008 + r3013;
        double r3015 = 0.004305878025937987;
        bool r3016 = r3002 < r3015;
        double r3017 = 0.041666666666666664;
        double r3018 = 1.0;
        double r3019 = r3002 * r3002;
        double r3020 = r3019 * r3019;
        double r3021 = r3020 * r3018;
        double r3022 = r3018 * r3021;
        double r3023 = r3017 * r3022;
        double r3024 = -1.0;
        double r3025 = r3002 * r3018;
        double r3026 = r3006 * r3025;
        double r3027 = r3024 * r3026;
        double r3028 = -0.5;
        double r3029 = r3002 * r3002;
        double r3030 = r3029 * r3018;
        double r3031 = r3018 * r3030;
        double r3032 = r3028 * r3031;
        double r3033 = r3027 + r3032;
        double r3034 = r3023 + r3033;
        double r3035 = r3016 ? r3034 : r3014;
        double r3036 = r3004 ? r3014 : r3035;
        return r3036;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r3037, r3038, r3039, r3040, r3041, r3042;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3037);
        mpfr_init(r3038);
        mpfr_init(r3039);
        mpfr_init(r3040);
        mpfr_init(r3041);
        mpfr_init(r3042);
}

double f_im(double x, double eps) {
        mpfr_set_d(r3037, x, MPFR_RNDN);
        mpfr_set_d(r3038, eps, MPFR_RNDN);
        mpfr_add(r3039, r3037, r3038, MPFR_RNDN);
        mpfr_cos(r3040, r3039, MPFR_RNDN);
        mpfr_cos(r3041, r3037, MPFR_RNDN);
        mpfr_sub(r3042, r3040, r3041, MPFR_RNDN);
        return mpfr_get_d(r3042, MPFR_RNDN);
}

static mpfr_t r3043, r3044, r3045, r3046, r3047, r3048, r3049, r3050, r3051, r3052, r3053, r3054, r3055, r3056, r3057, r3058, r3059, r3060, r3061, r3062, r3063, r3064, r3065, r3066, r3067, r3068, r3069, r3070, r3071, r3072, r3073, r3074, r3075, r3076, r3077, r3078, r3079, r3080, r3081, r3082, r3083, r3084, r3085, r3086, r3087, r3088, r3089;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3043);
        mpfr_init_set_str(r3044, "-0.00012136113218730316", 10, MPFR_RNDN);
        mpfr_init(r3045);
        mpfr_init(r3046);
        mpfr_init(r3047);
        mpfr_init(r3048);
        mpfr_init(r3049);
        mpfr_init_set_str(r3050, "3", 10, MPFR_RNDN);
        mpfr_init(r3051);
        mpfr_init(r3052);
        mpfr_init(r3053);
        mpfr_init(r3054);
        mpfr_init(r3055);
        mpfr_init(r3056);
        mpfr_init(r3057);
        mpfr_init(r3058);
        mpfr_init(r3059);
        mpfr_init(r3060);
        mpfr_init(r3061);
        mpfr_init(r3062);
        mpfr_init(r3063);
        mpfr_init_set_str(r3064, "0.00010899295011768118", 10, MPFR_RNDN);
        mpfr_init(r3065);
        mpfr_init_set_str(r3066, "1/3", 10, MPFR_RNDN);
        mpfr_init_set_str(r3067, "4", 10, MPFR_RNDN);
        mpfr_init(r3068);
        mpfr_init(r3069);
        mpfr_init_set_str(r3070, "-2", 10, MPFR_RNDN);
        mpfr_init(r3071);
        mpfr_init(r3072);
        mpfr_init_set_str(r3073, "-1", 10, MPFR_RNDN);
        mpfr_init(r3074);
        mpfr_init(r3075);
        mpfr_init(r3076);
        mpfr_init(r3077);
        mpfr_init(r3078);
        mpfr_init(r3079);
        mpfr_init(r3080);
        mpfr_init(r3081);
        mpfr_init(r3082);
        mpfr_init(r3083);
        mpfr_init(r3084);
        mpfr_init(r3085);
        mpfr_init(r3086);
        mpfr_init(r3087);
        mpfr_init(r3088);
        mpfr_init(r3089);
}

double f_fm(double x, double eps) {
        mpfr_set_d(r3043, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r3045, mpfr_cmp(r3043, r3044) < 0, MPFR_RNDN);
        mpfr_cos(r3046, r3043, MPFR_RNDN);
        mpfr_set_d(r3047, x, MPFR_RNDN);
        mpfr_cos(r3048, r3047, MPFR_RNDN);
        mpfr_mul(r3049, r3046, r3048, MPFR_RNDN);
        ;
        mpfr_pow(r3051, r3049, r3050, MPFR_RNDN);
        mpfr_sin(r3052, r3047, MPFR_RNDN);
        mpfr_sin(r3053, r3043, MPFR_RNDN);
        mpfr_mul(r3054, r3052, r3053, MPFR_RNDN);
        mpfr_pow(r3055, r3054, r3050, MPFR_RNDN);
        mpfr_sub(r3056, r3051, r3055, MPFR_RNDN);
        mpfr_mul(r3057, r3049, r3049, MPFR_RNDN);
        mpfr_mul(r3058, r3054, r3054, MPFR_RNDN);
        mpfr_add(r3059, r3057, r3058, MPFR_RNDN);
        mpfr_mul(r3060, r3049, r3054, MPFR_RNDN);
        mpfr_add(r3061, r3059, r3060, MPFR_RNDN);
        mpfr_div(r3062, r3056, r3061, MPFR_RNDN);
        mpfr_sub(r3063, r3062, r3048, MPFR_RNDN);
        ;
        mpfr_set_si(r3065, mpfr_cmp(r3043, r3064) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r3068, r3043, r3067, MPFR_RNDN);
        mpfr_mul(r3069, r3066, r3068, MPFR_RNDN);
        ;
        mpfr_mul(r3071, r3043, r3047, MPFR_RNDN);
        mpfr_mul(r3072, r3070, r3071, MPFR_RNDN);
        ;
        mpfr_mul(r3074, r3043, r3043, MPFR_RNDN);
        mpfr_mul(r3075, r3073, r3074, MPFR_RNDN);
        mpfr_add(r3076, r3072, r3075, MPFR_RNDN);
        mpfr_add(r3077, r3069, r3076, MPFR_RNDN);
        mpfr_add(r3078, r3048, r3049, MPFR_RNDN);
        mpfr_mul(r3079, r3053, r3052, MPFR_RNDN);
        mpfr_sub(r3080, r3078, r3079, MPFR_RNDN);
        mpfr_div(r3081, r3077, r3080, MPFR_RNDN);
        mpfr_log(r3082, r3057, MPFR_RNDN);
        mpfr_exp(r3083, r3082, MPFR_RNDN);
        mpfr_sub(r3084, r3083, r3058, MPFR_RNDN);
        mpfr_add(r3085, r3049, r3079, MPFR_RNDN);
        mpfr_div(r3086, r3084, r3085, MPFR_RNDN);
        mpfr_sub(r3087, r3086, r3048, MPFR_RNDN);
        if (mpfr_get_si(r3065, MPFR_RNDN)) { mpfr_set(r3088, r3081, MPFR_RNDN); } else { mpfr_set(r3088, r3087, MPFR_RNDN); };
        if (mpfr_get_si(r3045, MPFR_RNDN)) { mpfr_set(r3089, r3063, MPFR_RNDN); } else { mpfr_set(r3089, r3088, MPFR_RNDN); };
        return mpfr_get_d(r3089, MPFR_RNDN);
}

static mpfr_t r3090, r3091, r3092, r3093, r3094, r3095, r3096, r3097, r3098, r3099, r3100, r3101, r3102, r3103, r3104, r3105, r3106, r3107, r3108, r3109, r3110, r3111, r3112, r3113, r3114, r3115, r3116, r3117, r3118, r3119, r3120, r3121, r3122, r3123, r3124;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3090);
        mpfr_init_set_str(r3091, "-2.0254979742508256e-16", 10, MPFR_RNDN);
        mpfr_init(r3092);
        mpfr_init(r3093);
        mpfr_init(r3094);
        mpfr_init(r3095);
        mpfr_init(r3096);
        mpfr_init(r3097);
        mpfr_init(r3098);
        mpfr_init(r3099);
        mpfr_init(r3100);
        mpfr_init(r3101);
        mpfr_init(r3102);
        mpfr_init_set_str(r3103, "0.004305878025937987", 10, MPFR_RNDN);
        mpfr_init(r3104);
        mpfr_init_set_str(r3105, "1/24", 10, MPFR_RNDN);
        mpfr_init_set_str(r3106, "1", 10, MPFR_RNDN);
        mpfr_init(r3107);
        mpfr_init(r3108);
        mpfr_init(r3109);
        mpfr_init(r3110);
        mpfr_init(r3111);
        mpfr_init_set_str(r3112, "-1", 10, MPFR_RNDN);
        mpfr_init(r3113);
        mpfr_init(r3114);
        mpfr_init(r3115);
        mpfr_init_set_str(r3116, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r3117);
        mpfr_init(r3118);
        mpfr_init(r3119);
        mpfr_init(r3120);
        mpfr_init(r3121);
        mpfr_init(r3122);
        mpfr_init(r3123);
        mpfr_init(r3124);
}

double f_dm(double x, double eps) {
        mpfr_set_d(r3090, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r3092, mpfr_cmp(r3090, r3091) < 0, MPFR_RNDN);
        mpfr_cos(r3093, r3090, MPFR_RNDN);
        mpfr_set_d(r3094, x, MPFR_RNDN);
        mpfr_cos(r3095, r3094, MPFR_RNDN);
        mpfr_mul(r3096, r3093, r3095, MPFR_RNDN);
        mpfr_sin(r3097, r3090, MPFR_RNDN);
        mpfr_sin(r3098, r3094, MPFR_RNDN);
        mpfr_mul(r3099, r3097, r3098, MPFR_RNDN);
        mpfr_neg(r3100, r3099, MPFR_RNDN);
        mpfr_sub(r3101, r3100, r3095, MPFR_RNDN);
        mpfr_add(r3102, r3096, r3101, MPFR_RNDN);
        ;
        mpfr_set_si(r3104, mpfr_cmp(r3090, r3103) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r3107, r3090, r3090, MPFR_RNDN);
        mpfr_mul(r3108, r3107, r3107, MPFR_RNDN);
        mpfr_mul(r3109, r3108, r3106, MPFR_RNDN);
        mpfr_mul(r3110, r3106, r3109, MPFR_RNDN);
        mpfr_mul(r3111, r3105, r3110, MPFR_RNDN);
        ;
        mpfr_mul(r3113, r3090, r3106, MPFR_RNDN);
        mpfr_mul(r3114, r3094, r3113, MPFR_RNDN);
        mpfr_mul(r3115, r3112, r3114, MPFR_RNDN);
        ;
        mpfr_mul(r3117, r3090, r3090, MPFR_RNDN);
        mpfr_mul(r3118, r3117, r3106, MPFR_RNDN);
        mpfr_mul(r3119, r3106, r3118, MPFR_RNDN);
        mpfr_mul(r3120, r3116, r3119, MPFR_RNDN);
        mpfr_add(r3121, r3115, r3120, MPFR_RNDN);
        mpfr_add(r3122, r3111, r3121, MPFR_RNDN);
        if (mpfr_get_si(r3104, MPFR_RNDN)) { mpfr_set(r3123, r3122, MPFR_RNDN); } else { mpfr_set(r3123, r3102, MPFR_RNDN); };
        if (mpfr_get_si(r3092, MPFR_RNDN)) { mpfr_set(r3124, r3102, MPFR_RNDN); } else { mpfr_set(r3124, r3123, MPFR_RNDN); };
        return mpfr_get_d(r3124, MPFR_RNDN);
}

