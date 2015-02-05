#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.6";

double f_if(float N) {
        float r3125 = N;
        float r3126 = 1.0;
        float r3127 = r3125 + r3126;
        float r3128 = log(r3127);
        float r3129 = log(r3125);
        float r3130 = r3128 - r3129;
        return r3130;
}

double f_id(double N) {
        double r3131 = N;
        double r3132 = 1.0;
        double r3133 = r3131 + r3132;
        double r3134 = log(r3133);
        double r3135 = log(r3131);
        double r3136 = r3134 - r3135;
        return r3136;
}


double f_of(float N) {
        float r3137 = N;
        float r3138 = 61.794137954711914;
        bool r3139 = r3137 < r3138;
        float r3140 = 1.0;
        float r3141 = r3137 + r3140;
        float r3142 = r3141 / r3137;
        float r3143 = log(r3142);
        float r3144 = 0.3333333333333333;
        float r3145 = 3.0;
        float r3146 = pow(r3137, r3145);
        float r3147 = r3144 / r3146;
        float r3148 = r3140 / r3137;
        float r3149 = -0.5;
        float r3150 = r3137 * r3137;
        float r3151 = r3149 / r3150;
        float r3152 = r3148 + r3151;
        float r3153 = r3147 + r3152;
        float r3154 = r3139 ? r3143 : r3153;
        return r3154;
}

double f_od(double N) {
        double r3155 = N;
        double r3156 = 9389.102692440676;
        bool r3157 = r3155 < r3156;
        double r3158 = 1.0;
        double r3159 = r3155 + r3158;
        double r3160 = r3159 / r3155;
        double r3161 = log(r3160);
        double r3162 = 0.3333333333333333;
        double r3163 = r3155 * r3155;
        double r3164 = r3155 * r3163;
        double r3165 = r3158 / r3164;
        double r3166 = r3162 * r3165;
        double r3167 = -0.5;
        double r3168 = r3155 * r3155;
        double r3169 = r3158 / r3168;
        double r3170 = r3167 * r3169;
        double r3171 = r3158 / r3155;
        double r3172 = r3158 * r3171;
        double r3173 = r3170 + r3172;
        double r3174 = r3166 + r3173;
        double r3175 = r3157 ? r3161 : r3174;
        return r3175;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r3176, r3177, r3178, r3179, r3180, r3181;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3176);
        mpfr_init_set_str(r3177, "1", 10, MPFR_RNDN);
        mpfr_init(r3178);
        mpfr_init(r3179);
        mpfr_init(r3180);
        mpfr_init(r3181);
}

double f_im(double N) {
        mpfr_set_d(r3176, N, MPFR_RNDN);
        ;
        mpfr_add(r3178, r3176, r3177, MPFR_RNDN);
        mpfr_log(r3179, r3178, MPFR_RNDN);
        mpfr_log(r3180, r3176, MPFR_RNDN);
        mpfr_sub(r3181, r3179, r3180, MPFR_RNDN);
        return mpfr_get_d(r3181, MPFR_RNDN);
}

static mpfr_t r3182, r3183, r3184, r3185, r3186, r3187, r3188, r3189, r3190, r3191, r3192, r3193, r3194, r3195, r3196, r3197, r3198, r3199;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3182);
        mpfr_init_set_str(r3183, "61.794137954711914", 10, MPFR_RNDN);
        mpfr_init(r3184);
        mpfr_init_set_str(r3185, "1", 10, MPFR_RNDN);
        mpfr_init(r3186);
        mpfr_init(r3187);
        mpfr_init(r3188);
        mpfr_init_set_str(r3189, "1/3", 10, MPFR_RNDN);
        mpfr_init_set_str(r3190, "3", 10, MPFR_RNDN);
        mpfr_init(r3191);
        mpfr_init(r3192);
        mpfr_init(r3193);
        mpfr_init_set_str(r3194, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r3195);
        mpfr_init(r3196);
        mpfr_init(r3197);
        mpfr_init(r3198);
        mpfr_init(r3199);
}

double f_fm(double N) {
        mpfr_set_d(r3182, N, MPFR_RNDN);
        ;
        mpfr_set_si(r3184, mpfr_cmp(r3182, r3183) < 0, MPFR_RNDN);
        ;
        mpfr_add(r3186, r3182, r3185, MPFR_RNDN);
        mpfr_div(r3187, r3186, r3182, MPFR_RNDN);
        mpfr_log(r3188, r3187, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r3191, r3182, r3190, MPFR_RNDN);
        mpfr_div(r3192, r3189, r3191, MPFR_RNDN);
        mpfr_div(r3193, r3185, r3182, MPFR_RNDN);
        ;
        mpfr_mul(r3195, r3182, r3182, MPFR_RNDN);
        mpfr_div(r3196, r3194, r3195, MPFR_RNDN);
        mpfr_add(r3197, r3193, r3196, MPFR_RNDN);
        mpfr_add(r3198, r3192, r3197, MPFR_RNDN);
        if (mpfr_get_si(r3184, MPFR_RNDN)) { mpfr_set(r3199, r3188, MPFR_RNDN); } else { mpfr_set(r3199, r3198, MPFR_RNDN); };
        return mpfr_get_d(r3199, MPFR_RNDN);
}

static mpfr_t r3200, r3201, r3202, r3203, r3204, r3205, r3206, r3207, r3208, r3209, r3210, r3211, r3212, r3213, r3214, r3215, r3216, r3217, r3218, r3219, r3220;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3200);
        mpfr_init_set_str(r3201, "9389.102692440676", 10, MPFR_RNDN);
        mpfr_init(r3202);
        mpfr_init_set_str(r3203, "1", 10, MPFR_RNDN);
        mpfr_init(r3204);
        mpfr_init(r3205);
        mpfr_init(r3206);
        mpfr_init_set_str(r3207, "1/3", 10, MPFR_RNDN);
        mpfr_init(r3208);
        mpfr_init(r3209);
        mpfr_init(r3210);
        mpfr_init(r3211);
        mpfr_init_set_str(r3212, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r3213);
        mpfr_init(r3214);
        mpfr_init(r3215);
        mpfr_init(r3216);
        mpfr_init(r3217);
        mpfr_init(r3218);
        mpfr_init(r3219);
        mpfr_init(r3220);
}

double f_dm(double N) {
        mpfr_set_d(r3200, N, MPFR_RNDN);
        ;
        mpfr_set_si(r3202, mpfr_cmp(r3200, r3201) < 0, MPFR_RNDN);
        ;
        mpfr_add(r3204, r3200, r3203, MPFR_RNDN);
        mpfr_div(r3205, r3204, r3200, MPFR_RNDN);
        mpfr_log(r3206, r3205, MPFR_RNDN);
        ;
        mpfr_mul(r3208, r3200, r3200, MPFR_RNDN);
        mpfr_mul(r3209, r3200, r3208, MPFR_RNDN);
        mpfr_div(r3210, r3203, r3209, MPFR_RNDN);
        mpfr_mul(r3211, r3207, r3210, MPFR_RNDN);
        ;
        mpfr_mul(r3213, r3200, r3200, MPFR_RNDN);
        mpfr_div(r3214, r3203, r3213, MPFR_RNDN);
        mpfr_mul(r3215, r3212, r3214, MPFR_RNDN);
        mpfr_div(r3216, r3203, r3200, MPFR_RNDN);
        mpfr_mul(r3217, r3203, r3216, MPFR_RNDN);
        mpfr_add(r3218, r3215, r3217, MPFR_RNDN);
        mpfr_add(r3219, r3211, r3218, MPFR_RNDN);
        if (mpfr_get_si(r3202, MPFR_RNDN)) { mpfr_set(r3220, r3206, MPFR_RNDN); } else { mpfr_set(r3220, r3219, MPFR_RNDN); };
        return mpfr_get_d(r3220, MPFR_RNDN);
}

