#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE section 3.11";

double f_if(float x) {
        float r2164 = x;
        float r2165 = exp(r2164);
        float r2166 = 1.0;
        float r2167 = r2165 - r2166;
        float r2168 = r2165 / r2167;
        return r2168;
}

double f_id(double x) {
        double r2169 = x;
        double r2170 = exp(r2169);
        double r2171 = 1.0;
        double r2172 = r2170 - r2171;
        double r2173 = r2170 / r2172;
        return r2173;
}


double f_of(float x) {
        float r2174 = x;
        float r2175 = -0.12133512995968665;
        bool r2176 = r2174 < r2175;
        float r2177 = exp(r2174);
        float r2178 = 3.0;
        float r2179 = pow(r2177, r2178);
        float r2180 = 1.0;
        float r2181 = r2179 - r2180;
        float r2182 = r2177 + r2180;
        float r2183 = r2177 * r2177;
        float r2184 = r2182 + r2183;
        float r2185 = r2181 / r2184;
        float r2186 = r2177 / r2185;
        float r2187 = 0.06589843470368517;
        bool r2188 = r2174 < r2187;
        float r2189 = 0.5;
        float r2190 = 0.08333333333333333;
        float r2191 = r2190 * r2174;
        float r2192 = r2180 / r2174;
        float r2193 = r2191 + r2192;
        float r2194 = r2189 + r2193;
        float r2195 = r2180 / r2177;
        float r2196 = r2195 * r2195;
        float r2197 = r2180 - r2196;
        float r2198 = r2180 + r2195;
        float r2199 = r2197 / r2198;
        float r2200 = r2180 / r2199;
        float r2201 = r2188 ? r2194 : r2200;
        float r2202 = r2176 ? r2186 : r2201;
        return r2202;
}

double f_od(double x) {
        double r2203 = x;
        double r2204 = -0.001953357836734073;
        bool r2205 = r2203 < r2204;
        double r2206 = exp(r2203);
        double r2207 = r2206 * r2206;
        double r2208 = 1.0;
        double r2209 = r2207 - r2208;
        double r2210 = r2206 + r2208;
        double r2211 = r2209 / r2210;
        double r2212 = r2206 / r2211;
        double r2213 = 0.08333333333333333;
        double r2214 = r2203 * r2208;
        double r2215 = r2213 * r2214;
        double r2216 = 0.5;
        double r2217 = r2208 * r2208;
        double r2218 = r2216 * r2217;
        double r2219 = r2208 / r2203;
        double r2220 = r2219 * r2208;
        double r2221 = r2208 * r2220;
        double r2222 = r2218 + r2221;
        double r2223 = r2215 + r2222;
        double r2224 = r2205 ? r2212 : r2223;
        return r2224;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2225, r2226, r2227, r2228, r2229;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2225);
        mpfr_init(r2226);
        mpfr_init_set_str(r2227, "1", 10, MPFR_RNDN);
        mpfr_init(r2228);
        mpfr_init(r2229);
}

double f_im(double x) {
        mpfr_set_d(r2225, x, MPFR_RNDN);
        mpfr_exp(r2226, r2225, MPFR_RNDN);
        ;
        mpfr_sub(r2228, r2226, r2227, MPFR_RNDN);
        mpfr_div(r2229, r2226, r2228, MPFR_RNDN);
        return mpfr_get_d(r2229, MPFR_RNDN);
}

static mpfr_t r2230, r2231, r2232, r2233, r2234, r2235, r2236, r2237, r2238, r2239, r2240, r2241, r2242, r2243, r2244, r2245, r2246, r2247, r2248, r2249, r2250, r2251, r2252, r2253, r2254, r2255, r2256, r2257, r2258;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2230);
        mpfr_init_set_str(r2231, "-0.12133512995968665", 10, MPFR_RNDN);
        mpfr_init(r2232);
        mpfr_init(r2233);
        mpfr_init_set_str(r2234, "3", 10, MPFR_RNDN);
        mpfr_init(r2235);
        mpfr_init_set_str(r2236, "1", 10, MPFR_RNDN);
        mpfr_init(r2237);
        mpfr_init(r2238);
        mpfr_init(r2239);
        mpfr_init(r2240);
        mpfr_init(r2241);
        mpfr_init(r2242);
        mpfr_init_set_str(r2243, "0.06589843470368517", 10, MPFR_RNDN);
        mpfr_init(r2244);
        mpfr_init_set_str(r2245, "1/2", 10, MPFR_RNDN);
        mpfr_init_set_str(r2246, "1/12", 10, MPFR_RNDN);
        mpfr_init(r2247);
        mpfr_init(r2248);
        mpfr_init(r2249);
        mpfr_init(r2250);
        mpfr_init(r2251);
        mpfr_init(r2252);
        mpfr_init(r2253);
        mpfr_init(r2254);
        mpfr_init(r2255);
        mpfr_init(r2256);
        mpfr_init(r2257);
        mpfr_init(r2258);
}

double f_fm(double x) {
        mpfr_set_d(r2230, x, MPFR_RNDN);
        ;
        mpfr_set_si(r2232, mpfr_cmp(r2230, r2231) < 0, MPFR_RNDN);
        mpfr_exp(r2233, r2230, MPFR_RNDN);
        ;
        mpfr_pow(r2235, r2233, r2234, MPFR_RNDN);
        ;
        mpfr_sub(r2237, r2235, r2236, MPFR_RNDN);
        mpfr_add(r2238, r2233, r2236, MPFR_RNDN);
        mpfr_mul(r2239, r2233, r2233, MPFR_RNDN);
        mpfr_add(r2240, r2238, r2239, MPFR_RNDN);
        mpfr_div(r2241, r2237, r2240, MPFR_RNDN);
        mpfr_div(r2242, r2233, r2241, MPFR_RNDN);
        ;
        mpfr_set_si(r2244, mpfr_cmp(r2230, r2243) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r2247, r2246, r2230, MPFR_RNDN);
        mpfr_div(r2248, r2236, r2230, MPFR_RNDN);
        mpfr_add(r2249, r2247, r2248, MPFR_RNDN);
        mpfr_add(r2250, r2245, r2249, MPFR_RNDN);
        mpfr_div(r2251, r2236, r2233, MPFR_RNDN);
        mpfr_mul(r2252, r2251, r2251, MPFR_RNDN);
        mpfr_sub(r2253, r2236, r2252, MPFR_RNDN);
        mpfr_add(r2254, r2236, r2251, MPFR_RNDN);
        mpfr_div(r2255, r2253, r2254, MPFR_RNDN);
        mpfr_div(r2256, r2236, r2255, MPFR_RNDN);
        if (mpfr_get_si(r2244, MPFR_RNDN)) { mpfr_set(r2257, r2250, MPFR_RNDN); } else { mpfr_set(r2257, r2256, MPFR_RNDN); };
        if (mpfr_get_si(r2232, MPFR_RNDN)) { mpfr_set(r2258, r2242, MPFR_RNDN); } else { mpfr_set(r2258, r2257, MPFR_RNDN); };
        return mpfr_get_d(r2258, MPFR_RNDN);
}

static mpfr_t r2259, r2260, r2261, r2262, r2263, r2264, r2265, r2266, r2267, r2268, r2269, r2270, r2271, r2272, r2273, r2274, r2275, r2276, r2277, r2278, r2279, r2280;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2259);
        mpfr_init_set_str(r2260, "-0.001953357836734073", 10, MPFR_RNDN);
        mpfr_init(r2261);
        mpfr_init(r2262);
        mpfr_init(r2263);
        mpfr_init_set_str(r2264, "1", 10, MPFR_RNDN);
        mpfr_init(r2265);
        mpfr_init(r2266);
        mpfr_init(r2267);
        mpfr_init(r2268);
        mpfr_init_set_str(r2269, "1/12", 10, MPFR_RNDN);
        mpfr_init(r2270);
        mpfr_init(r2271);
        mpfr_init_set_str(r2272, "1/2", 10, MPFR_RNDN);
        mpfr_init(r2273);
        mpfr_init(r2274);
        mpfr_init(r2275);
        mpfr_init(r2276);
        mpfr_init(r2277);
        mpfr_init(r2278);
        mpfr_init(r2279);
        mpfr_init(r2280);
}

double f_dm(double x) {
        mpfr_set_d(r2259, x, MPFR_RNDN);
        ;
        mpfr_set_si(r2261, mpfr_cmp(r2259, r2260) < 0, MPFR_RNDN);
        mpfr_exp(r2262, r2259, MPFR_RNDN);
        mpfr_mul(r2263, r2262, r2262, MPFR_RNDN);
        ;
        mpfr_sub(r2265, r2263, r2264, MPFR_RNDN);
        mpfr_add(r2266, r2262, r2264, MPFR_RNDN);
        mpfr_div(r2267, r2265, r2266, MPFR_RNDN);
        mpfr_div(r2268, r2262, r2267, MPFR_RNDN);
        ;
        mpfr_mul(r2270, r2259, r2264, MPFR_RNDN);
        mpfr_mul(r2271, r2269, r2270, MPFR_RNDN);
        ;
        mpfr_mul(r2273, r2264, r2264, MPFR_RNDN);
        mpfr_mul(r2274, r2272, r2273, MPFR_RNDN);
        mpfr_div(r2275, r2264, r2259, MPFR_RNDN);
        mpfr_mul(r2276, r2275, r2264, MPFR_RNDN);
        mpfr_mul(r2277, r2264, r2276, MPFR_RNDN);
        mpfr_add(r2278, r2274, r2277, MPFR_RNDN);
        mpfr_add(r2279, r2271, r2278, MPFR_RNDN);
        if (mpfr_get_si(r2261, MPFR_RNDN)) { mpfr_set(r2280, r2268, MPFR_RNDN); } else { mpfr_set(r2280, r2279, MPFR_RNDN); };
        return mpfr_get_d(r2280, MPFR_RNDN);
}

