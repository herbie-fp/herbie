#define _POSIX_C_SOURCE 199309L
#include <tgmath.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdbool.h>
#include <signal.h>

#ifndef NARGS
#define NARGS 1
#endif


#if NARGS == 1
#define ARGS(t) t
#define INVOKE(f,args) f(args[0])
#define RANDARGS(argsName) argsName[0] = rand_double()
#elif NARGS == 2
#define ARGS(t) t, t
#define INVOKE(f,args) f(args[0], args[1])
#define RANDARGS(argsName) \
  argsName[0] = rand_double();\
  argsName[1] = rand_double()
#elif NARGS == 3
#define ARGS(t) t, t, t
#define INVOKE(f,args) f(args[0], args[1], args[2])
#define RANDARGS(argsName) \
  argsName[0] = rand_double();\
  argsName[1] = rand_double();\
  argsName[2] = rand_double()
#elif NARGS == 4
#define ARGS(t) t, t, t, t
#define INVOKE(f,args) f(args[0], args[1], args[2], args[3])
#define RANDARGS(argsName) \
  argsName[0] = rand_double();\
  argsName[1] = rand_double();\
  argsName[2] = rand_double();\
  argsName[3] = rand_double()
#elif NARGS == 5
#define ARGS(t) t, t, t, t, t
#define INVOKE(f,args) f(args[0], args[1], args[2], args[3], args[4])
#define RANDARGS() \
  argsName[0] = rand_double();\
  argsName[1] = rand_double();\
  argsName[2] = rand_double();\
  argsName[3] = rand_double();\
  argsName[4] = rand_double()
#elif NARGS == 6
#define ARGS(t) t, t, t, t, t, t
#define INVOKE(f,args) f(args[0], args[1], args[2], args[3], args[4], args[5])
#define RANDARGS() \
  argsName[0] = rand_double();\
  argsName[1] = rand_double();\
  argsName[2] = rand_double();\
  argsName[3] = rand_double();\
  argsName[4] = rand_double();\
  argsName[5] = rand_double()
#else
#define ARGS(t) abort()
#define INVOKE(f,args) abort()
#define RANDARGS() abort()
#endif

void setup_mpfr_f_im(void);
double f_id(ARGS(double));
double f_im(ARGS(double));
double f_od(ARGS(double));
double f_om(ARGS(double));
extern char *name;

unsigned long long ulpd(double x, double y) {
        if (x == 0) x = fabs(x); // -0 == 0
        if (y == 0) y = fabs(y); // -0 == 0

        if (x != x && y != y) return 0;
        if (x != x) return LLONG_MIN; // Maximum error
        if (y != y) return LLONG_MIN; // Maximum error

        long long xx = *((long long*) &x);
        xx = xx < 0 ? LLONG_MIN - xx : xx;

        long long yy = *((long long*) &y);
        yy = yy < 0 ? LLONG_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}
double rand_double() {
        long long c0 = rand()&0xffff;
        long long c1 = rand()&0xffff;
        long long c2 = rand()&0xffff;
        long long c3 = rand()&0xffff;
        long long c = ((c3 << 48) | (c2 << 32) | (c1<<16) | c0);
        return *(double*)&c;
}
char ordinaryd(double x) {
        return 1 / x != 0 && x == x;
}
unsigned int maxOrigErr, maxImprErr, numUnimproved, maxUnimproved;
unsigned long long pointsTested;
void finish(){
  printf("%u,%u,%u,%u,%llu\n", maxOrigErr, maxImprErr, numUnimproved, maxUnimproved, pointsTested);
  exit(0);
}

int main(int argc, char** argv){
  struct timespec start, cur;
  double args[NARGS];
  double exact, origApprox, improvedApprox;
  bool ordinaryArgs;
  int i;
  unsigned int origErr, imprErr, unimprovement;

  time_t timeout = 10;
  if (argc > 1){
    timeout = atoi(argv[1]);
  }

  // Set up the initial time variables
  clock_gettime(CLOCK_MONOTONIC, &start);
  cur = start;

  // Set up mpfr
  setup_mpfr_f_im();

  // Handle interrupts properly.
  struct sigaction sigIntHandler;
  sigIntHandler.sa_handler = finish;
  sigemptyset(&sigIntHandler.sa_mask);
  sigIntHandler.sa_flags = 0;
  sigaction(SIGINT, &sigIntHandler, NULL);
  
  // While the allotted time hasn't passed
  while(cur.tv_sec - start.tv_sec < timeout){
    // Get some randome arguments
    RANDARGS(args);

    // See if they're all normal floats
    ordinaryArgs = true;
    for(i = 0; i < NARGS; ++i){
      if (!ordinaryd(args[i])){
	ordinaryArgs = false;
	break;
      }
    }
    // If not, skip them.
    if(!ordinaryArgs)
      continue;

    // Get the exact result from those args
    exact = INVOKE(f_im, args);

    // If that isn't exact, skip it.
    if(!ordinaryd(exact))
      continue;

    // Get the approxmiate answers with the input and output programs
    origApprox = INVOKE(f_id, args);
    improvedApprox = INVOKE(f_od, args);

    // Get the errors for both.
    origErr = ulpd(origApprox, exact);
    imprErr = ulpd(improvedApprox, exact);

    // Update the maxes
    if (maxOrigErr < origErr) maxOrigErr = origErr;
    if (maxImprErr < imprErr) maxImprErr = imprErr;

    // Update unimprovement numbers
    if (origErr < imprErr){
      numUnimproved++;
      unimprovement = imprErr - origErr;
      if (maxUnimproved < unimprovement) maxUnimproved = unimprovement;
    }
    clock_gettime(CLOCK_MONOTONIC, &cur);
    ++pointsTested;
  }
  finish();
}
