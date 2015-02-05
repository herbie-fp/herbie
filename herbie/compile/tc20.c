#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.2.1";

double f_if(float a, float b_2F2, float c) {
        float r2585 = b_2F2;
        float r2586 = -r2585;
        float r2587 = r2585 * r2585;
        float r2588 = a;
        float r2589 = c;
        float r2590 = r2588 * r2589;
        float r2591 = r2587 - r2590;
        float r2592 = sqrt(r2591);
        float r2593 = r2586 - r2592;
        float r2594 = r2593 / r2588;
        return r2594;
}

double f_id(double a, double b_2F2, double c) {
        double r2595 = b_2F2;
        double r2596 = -r2595;
        double r2597 = r2595 * r2595;
        double r2598 = a;
        double r2599 = c;
        double r2600 = r2598 * r2599;
        double r2601 = r2597 - r2600;
        double r2602 = sqrt(r2601);
        double r2603 = r2596 - r2602;
        double r2604 = r2603 / r2598;
        return r2604;
}


double f_of(float a, float b_2F2, float c) {
        float r2605 = b_2F2;
        float r2606 = -1.7035773121170497e-31;
        bool r2607 = r2605 < r2606;
        float r2608 = a;
        float r2609 = c;
        float r2610 = r2608 * r2609;
        float r2611 = -r2605;
        float r2612 = r2605 * r2605;
        float r2613 = r2612 - r2610;
        float r2614 = sqrt(r2613);
        float r2615 = r2611 + r2614;
        float r2616 = r2608 * r2615;
        float r2617 = r2610 / r2616;
        float r2618 = 1010939067367424.0;
        bool r2619 = r2605 < r2618;
        float r2620 = r2611 / r2608;
        float r2621 = 1.0;
        float r2622 = r2621 / r2608;
        float r2623 = r2614 * r2622;
        float r2624 = r2620 - r2623;
        float r2625 = -2.0;
        float r2626 = r2605 / r2608;
        float r2627 = r2625 * r2626;
        float r2628 = 0.5;
        float r2629 = r2609 / r2605;
        float r2630 = r2628 * r2629;
        float r2631 = r2627 + r2630;
        float r2632 = r2619 ? r2624 : r2631;
        float r2633 = r2607 ? r2617 : r2632;
        return r2633;
}

double f_od(double a, double b_2F2, double c) {
        double r2634 = b_2F2;
        double r2635 = -1.1021750489264112e-79;
        bool r2636 = r2634 < r2635;
        double r2637 = 1.0;
        double r2638 = c;
        double r2639 = a;
        double r2640 = r2637 * r2637;
        double r2641 = r2639 * r2640;
        double r2642 = r2638 * r2641;
        double r2643 = r2637 * r2642;
        double r2644 = -r2634;
        double r2645 = r2634 * r2634;
        double r2646 = r2639 * r2638;
        double r2647 = r2645 - r2646;
        double r2648 = sqrt(r2647);
        double r2649 = r2644 + r2648;
        double r2650 = r2639 * r2649;
        double r2651 = r2643 / r2650;
        double r2652 = 1.0595371144044192e+95;
        bool r2653 = r2634 < r2652;
        double r2654 = r2638 * r2639;
        double r2655 = r2645 - r2654;
        double r2656 = sqrt(r2655);
        double r2657 = r2644 - r2656;
        double r2658 = r2637 / r2639;
        double r2659 = r2657 * r2658;
        double r2660 = 0.5;
        double r2661 = r2637 / r2634;
        double r2662 = r2637 / r2638;
        double r2663 = r2661 / r2662;
        double r2664 = r2663 / r2637;
        double r2665 = r2660 * r2664;
        double r2666 = -2.0;
        double r2667 = r2637 / r2661;
        double r2668 = r2667 / r2637;
        double r2669 = r2668 / r2639;
        double r2670 = r2666 * r2669;
        double r2671 = r2665 + r2670;
        double r2672 = r2653 ? r2659 : r2671;
        double r2673 = r2636 ? r2651 : r2672;
        return r2673;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2674, r2675, r2676, r2677, r2678, r2679, r2680, r2681, r2682, r2683;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r2674);
        mpfr_init(r2675);
        mpfr_init(r2676);
        mpfr_init(r2677);
        mpfr_init(r2678);
        mpfr_init(r2679);
        mpfr_init(r2680);
        mpfr_init(r2681);
        mpfr_init(r2682);
        mpfr_init(r2683);
}

double f_im(double a, double b_2F2, double c) {
        mpfr_set_d(r2674, b_2F2, MPFR_RNDN);
        mpfr_neg(r2675, r2674, MPFR_RNDN);
        mpfr_mul(r2676, r2674, r2674, MPFR_RNDN);
        mpfr_set_d(r2677, a, MPFR_RNDN);
        mpfr_set_d(r2678, c, MPFR_RNDN);
        mpfr_mul(r2679, r2677, r2678, MPFR_RNDN);
        mpfr_sub(r2680, r2676, r2679, MPFR_RNDN);
        mpfr_sqrt(r2681, r2680, MPFR_RNDN);
        mpfr_sub(r2682, r2675, r2681, MPFR_RNDN);
        mpfr_div(r2683, r2682, r2677, MPFR_RNDN);
        return mpfr_get_d(r2683, MPFR_RNDN);
}

static mpfr_t r2684, r2685, r2686, r2687, r2688, r2689, r2690, r2691, r2692, r2693, r2694, r2695, r2696, r2697, r2698, r2699, r2700, r2701, r2702, r2703, r2704, r2705, r2706, r2707, r2708, r2709, r2710, r2711, r2712;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r2684);
        mpfr_init_set_str(r2685, "-1.7035773121170497e-31", 10, MPFR_RNDN);
        mpfr_init(r2686);
        mpfr_init(r2687);
        mpfr_init(r2688);
        mpfr_init(r2689);
        mpfr_init(r2690);
        mpfr_init(r2691);
        mpfr_init(r2692);
        mpfr_init(r2693);
        mpfr_init(r2694);
        mpfr_init(r2695);
        mpfr_init(r2696);
        mpfr_init_set_str(r2697, "1010939067367424.0", 10, MPFR_RNDN);
        mpfr_init(r2698);
        mpfr_init(r2699);
        mpfr_init_set_str(r2700, "1", 10, MPFR_RNDN);
        mpfr_init(r2701);
        mpfr_init(r2702);
        mpfr_init(r2703);
        mpfr_init_set_str(r2704, "-2", 10, MPFR_RNDN);
        mpfr_init(r2705);
        mpfr_init(r2706);
        mpfr_init_set_str(r2707, "1/2", 10, MPFR_RNDN);
        mpfr_init(r2708);
        mpfr_init(r2709);
        mpfr_init(r2710);
        mpfr_init(r2711);
        mpfr_init(r2712);
}

double f_fm(double a, double b_2F2, double c) {
        mpfr_set_d(r2684, b_2F2, MPFR_RNDN);
        ;
        mpfr_set_si(r2686, mpfr_cmp(r2684, r2685) < 0, MPFR_RNDN);
        mpfr_set_d(r2687, a, MPFR_RNDN);
        mpfr_set_d(r2688, c, MPFR_RNDN);
        mpfr_mul(r2689, r2687, r2688, MPFR_RNDN);
        mpfr_neg(r2690, r2684, MPFR_RNDN);
        mpfr_mul(r2691, r2684, r2684, MPFR_RNDN);
        mpfr_sub(r2692, r2691, r2689, MPFR_RNDN);
        mpfr_sqrt(r2693, r2692, MPFR_RNDN);
        mpfr_add(r2694, r2690, r2693, MPFR_RNDN);
        mpfr_mul(r2695, r2687, r2694, MPFR_RNDN);
        mpfr_div(r2696, r2689, r2695, MPFR_RNDN);
        ;
        mpfr_set_si(r2698, mpfr_cmp(r2684, r2697) < 0, MPFR_RNDN);
        mpfr_div(r2699, r2690, r2687, MPFR_RNDN);
        ;
        mpfr_div(r2701, r2700, r2687, MPFR_RNDN);
        mpfr_mul(r2702, r2693, r2701, MPFR_RNDN);
        mpfr_sub(r2703, r2699, r2702, MPFR_RNDN);
        ;
        mpfr_div(r2705, r2684, r2687, MPFR_RNDN);
        mpfr_mul(r2706, r2704, r2705, MPFR_RNDN);
        ;
        mpfr_div(r2708, r2688, r2684, MPFR_RNDN);
        mpfr_mul(r2709, r2707, r2708, MPFR_RNDN);
        mpfr_add(r2710, r2706, r2709, MPFR_RNDN);
        if (mpfr_get_si(r2698, MPFR_RNDN)) { mpfr_set(r2711, r2703, MPFR_RNDN); } else { mpfr_set(r2711, r2710, MPFR_RNDN); };
        if (mpfr_get_si(r2686, MPFR_RNDN)) { mpfr_set(r2712, r2696, MPFR_RNDN); } else { mpfr_set(r2712, r2711, MPFR_RNDN); };
        return mpfr_get_d(r2712, MPFR_RNDN);
}

static mpfr_t r2713, r2714, r2715, r2716, r2717, r2718, r2719, r2720, r2721, r2722, r2723, r2724, r2725, r2726, r2727, r2728, r2729, r2730, r2731, r2732, r2733, r2734, r2735, r2736, r2737, r2738, r2739, r2740, r2741, r2742, r2743, r2744, r2745, r2746, r2747, r2748, r2749, r2750, r2751, r2752;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r2713);
        mpfr_init_set_str(r2714, "-1.1021750489264112e-79", 10, MPFR_RNDN);
        mpfr_init(r2715);
        mpfr_init_set_str(r2716, "1", 10, MPFR_RNDN);
        mpfr_init(r2717);
        mpfr_init(r2718);
        mpfr_init(r2719);
        mpfr_init(r2720);
        mpfr_init(r2721);
        mpfr_init(r2722);
        mpfr_init(r2723);
        mpfr_init(r2724);
        mpfr_init(r2725);
        mpfr_init(r2726);
        mpfr_init(r2727);
        mpfr_init(r2728);
        mpfr_init(r2729);
        mpfr_init(r2730);
        mpfr_init_set_str(r2731, "1.0595371144044192e+95", 10, MPFR_RNDN);
        mpfr_init(r2732);
        mpfr_init(r2733);
        mpfr_init(r2734);
        mpfr_init(r2735);
        mpfr_init(r2736);
        mpfr_init(r2737);
        mpfr_init(r2738);
        mpfr_init_set_str(r2739, "1/2", 10, MPFR_RNDN);
        mpfr_init(r2740);
        mpfr_init(r2741);
        mpfr_init(r2742);
        mpfr_init(r2743);
        mpfr_init(r2744);
        mpfr_init_set_str(r2745, "-2", 10, MPFR_RNDN);
        mpfr_init(r2746);
        mpfr_init(r2747);
        mpfr_init(r2748);
        mpfr_init(r2749);
        mpfr_init(r2750);
        mpfr_init(r2751);
        mpfr_init(r2752);
}

double f_dm(double a, double b_2F2, double c) {
        mpfr_set_d(r2713, b_2F2, MPFR_RNDN);
        ;
        mpfr_set_si(r2715, mpfr_cmp(r2713, r2714) < 0, MPFR_RNDN);
        ;
        mpfr_set_d(r2717, c, MPFR_RNDN);
        mpfr_set_d(r2718, a, MPFR_RNDN);
        mpfr_mul(r2719, r2716, r2716, MPFR_RNDN);
        mpfr_mul(r2720, r2718, r2719, MPFR_RNDN);
        mpfr_mul(r2721, r2717, r2720, MPFR_RNDN);
        mpfr_mul(r2722, r2716, r2721, MPFR_RNDN);
        mpfr_neg(r2723, r2713, MPFR_RNDN);
        mpfr_mul(r2724, r2713, r2713, MPFR_RNDN);
        mpfr_mul(r2725, r2718, r2717, MPFR_RNDN);
        mpfr_sub(r2726, r2724, r2725, MPFR_RNDN);
        mpfr_sqrt(r2727, r2726, MPFR_RNDN);
        mpfr_add(r2728, r2723, r2727, MPFR_RNDN);
        mpfr_mul(r2729, r2718, r2728, MPFR_RNDN);
        mpfr_div(r2730, r2722, r2729, MPFR_RNDN);
        ;
        mpfr_set_si(r2732, mpfr_cmp(r2713, r2731) < 0, MPFR_RNDN);
        mpfr_mul(r2733, r2717, r2718, MPFR_RNDN);
        mpfr_sub(r2734, r2724, r2733, MPFR_RNDN);
        mpfr_sqrt(r2735, r2734, MPFR_RNDN);
        mpfr_sub(r2736, r2723, r2735, MPFR_RNDN);
        mpfr_div(r2737, r2716, r2718, MPFR_RNDN);
        mpfr_mul(r2738, r2736, r2737, MPFR_RNDN);
        ;
        mpfr_div(r2740, r2716, r2713, MPFR_RNDN);
        mpfr_div(r2741, r2716, r2717, MPFR_RNDN);
        mpfr_div(r2742, r2740, r2741, MPFR_RNDN);
        mpfr_div(r2743, r2742, r2716, MPFR_RNDN);
        mpfr_mul(r2744, r2739, r2743, MPFR_RNDN);
        ;
        mpfr_div(r2746, r2716, r2740, MPFR_RNDN);
        mpfr_div(r2747, r2746, r2716, MPFR_RNDN);
        mpfr_div(r2748, r2747, r2718, MPFR_RNDN);
        mpfr_mul(r2749, r2745, r2748, MPFR_RNDN);
        mpfr_add(r2750, r2744, r2749, MPFR_RNDN);
        if (mpfr_get_si(r2732, MPFR_RNDN)) { mpfr_set(r2751, r2738, MPFR_RNDN); } else { mpfr_set(r2751, r2750, MPFR_RNDN); };
        if (mpfr_get_si(r2715, MPFR_RNDN)) { mpfr_set(r2752, r2730, MPFR_RNDN); } else { mpfr_set(r2752, r2751, MPFR_RNDN); };
        return mpfr_get_d(r2752, MPFR_RNDN);
}

