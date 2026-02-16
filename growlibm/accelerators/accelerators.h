#ifndef GROWLIBM_ACCELERATORS_H
#define GROWLIBM_ACCELERATORS_H

#ifdef __cplusplus
extern "C" {
#endif

double sinprod(double x, double y);
double cosprod(double x, double y);
double sinquot(double x, double y);
double cosquot(double x, double y);
double log1pmd(double x);
double invgud(double x);
double logtan(double x);
double verdcos(double x);
double hypot(double x, double y);


#ifdef __cplusplus
}
#endif

#endif /* GROWLIBM_ACCELERATORS_H */
