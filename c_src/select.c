//
// Created by tefx on 9/24/15.
//

#include <math.h>
#include <stddef.h>
#include "select.h"

bool pareto_dominate_2d(double x0, double y0, double x1, double y1){
    int xr = cmp_double(&x0, &x1);
    int yr = cmp_double(&y0, &y1);

    if (xr == 0 && yr < 0)
        return true;
    else if (xr < 0 && yr <= 0)
        return true;
    else
        return false;
}

int cmp_double(const void* pa, const void* pb){
    double a = *(const double*)pa;
    double b = *(const double*)pb;

    if (fabs(a-b)<=ZERO)
        return 0;
    else if (a<b)
        return -1;
    else
        return 1;
}

double distance(double x0, double y0, double x1, double y1){
    return sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1));
}


void normalise(double* ls, size_t n){
    double min = ls[0];
    double max = ls[0];

    for (size_t i=0;i<n;i++){
        if (ls[i] < min) min = ls[i];
        if (ls[i] > max) max = ls[i];
    }

    for (size_t i=0;i<n;i++){
        ls[i] = (ls[i] - min) / (max - min);
    }
}
