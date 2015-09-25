//
// Created by tefx on 9/24/15.
//

#ifndef MOWSC_SELECT_H
#define MOWSC_SELECT_H
#include <stdbool.h>

#define ZERO 1e-20
#define INF 1e42

double distance(double x0, double y0, double x1, double y1);
bool pareto_dominate_2d(double x0, double y0, double x1, double y1);

int cmp_double(const void*, const void*);
void normalise(double* ls, size_t n);


#endif //MOWSC_SELECT_H
