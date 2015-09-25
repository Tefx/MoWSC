#define _GNU_SOURCE

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>
#include "spea2.h"

int cmp_fit(const void* pi, const void* pj, void* c);

bool less_by_nearest(double* a, double* b, size_t n);
void remove_distance(size_t n, size_t k, double* dis, bool* sel);

void compute_distances(double* xs, double* ys, size_t n, double* dis);
void compute_fitness(double* xs, double* ys, size_t n, double* dis, double* fit);

void spea2_tunc(bool* sel, double* dis, size_t n);

void spea2_select_2d(double* xs, double* ys, size_t n, size_t m, size_t* res){
    double* fit = (double*)malloc(sizeof(double)*n);
    double* dis = (double*)malloc(sizeof(double)*n*n*2);
    bool* sel = (bool*)malloc(sizeof(bool)*n);
    size_t* unsel;
    size_t n_sel=0;

    normalise(xs, n);
    normalise(ys, n);

    compute_distances(xs, ys, n, dis);
    compute_fitness(xs, ys, n, dis, fit);

    for (size_t i=0;i<n;i++)
        if (fit[i]<1){
            sel[i] = true;
            n_sel++;
        } else
            sel[i] = false;

    if (n_sel<m){
        unsel = (size_t*)malloc(sizeof(size_t)*(n-n_sel));

        for (size_t i=0,j=0;i<n;i++)
            if (!sel[i])
                unsel[j++] = i;

        qsort_r(unsel, n-n_sel, sizeof(size_t), cmp_fit, fit);

        for (size_t i=0;i<m-n_sel;i++)
            sel[unsel[i]] = true;

        free(unsel);
    } else if (n_sel>m){
        for (size_t i=0;i<n;i++)
            if (!sel[i])
                remove_distance(n, i, dis, sel);

        for (size_t i=0;i<n_sel-m;i++)
            spea2_tunc(sel, dis, n);
    }

    for (size_t i=0,j=0;i<n;i++)
        if (sel[i])
            res[j++] = i;

    free(sel);
    free(dis);
    free(fit);
}

void compute_distances(double* xs, double* ys, size_t n, double* dis){
    for (size_t i=0;i<n;i++){
        dis[i*n+i] = -1;
        for (size_t j=i+1;j<n;j++)
            dis[i*n+j] = dis[j*n+i] = distance(xs[i], ys[i], xs[j], ys[j]);
    }

    memcpy(dis+n*n, dis, sizeof(double)*n*n);

    for (size_t i=0;i<n;i++)
        qsort(dis+i*n, n, sizeof(double), cmp_double);
}

void compute_fitness(double* xs, double* ys, size_t n, double* dis, double* fit){
    double *s = (double*)malloc(sizeof(double)*n);
    size_t k = (size_t)lrint(sqrt(n));

    for (size_t i=0;i<n;i++) s[i] = fit[i] = 0;

    for (size_t i=0;i<n;i++)
        for (size_t j=0;j<n;j++)
            if (pareto_dominate_2d(xs[i], ys[i], xs[j], ys[j]))
                s[i] += 1;

    for (size_t i=0;i<n;i++)
        for (size_t j=0;j<n;j++)
            if (pareto_dominate_2d(xs[j], ys[j], xs[i], ys[i]))
                fit[i] += s[j];

    for (size_t i=0;i<n;i++)
        fit[i] += 1.0 / (dis[i*n+k] + 2);

    free(s);
}

void spea2_tunc(bool* sel, double* dis, size_t n){
    size_t worst = 0;

    while (!sel[worst]) worst++;

    for (size_t i=worst+1;i<n;i++){
        if (sel[i] && less_by_nearest(dis+i*n, dis+worst*n, n))
            worst = i;
    }

    sel[worst] = false;
    remove_distance(n, worst, dis, sel);
}

int cmp_fit(const void* pi, const void* pj, void* c) {
    double* fit = (double*) c;
    size_t i = *(const size_t*)pi;
    size_t j = *(const size_t*)pj;

    return cmp_double(fit+i, fit+j);
}

bool less_by_nearest(double* a, double* b, size_t n){
    size_t i=0, j=0;

    while (i<n && j<n)
        if (a[i]<-0.5)
            i++;
        else if (b[j]<-0.5)
            j++;
        else if (cmp_double(a+i, b+i)==0)
            i++,j++;
        else if (a[i]<b[j])
            return true;
        else
            return false;

    return false;
}

void remove_distance(size_t n, size_t k, double* dis, bool* sel){
    double* d;
    double* l;

    for (size_t i=0;i<n;i++)
        if (sel[i]){
            d = dis+n*n+i*n+k;
            l = dis+i*n;
            while (cmp_double(l, d)!=0) l++;
            *l = -1;
        }
}
