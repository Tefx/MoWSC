//
// Created by tefx on 9/29/15.
//

#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include "pso.h"

bool initialized = false;

double updateItem(double c1, double c2, int* gbest, int* pbest, int* p, int i, int j) {
    double gb = (gbest[i] == j?1:0);
    double pb = (pbest[i] == j?1:0);
    double pp = (p[i] == j?1:0);
    double r1 = (double)rand() / RAND_MAX;
    double r2 = (double)rand() / RAND_MAX;
    return c1 * r1 * (pb - pp) + c2 * r2 * (gb - pp);
}

void updateVelocity(int n, int m, double w, double c1, double c2, int* gbest, int* pbest, int* p, double* vel){
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = true;
    }

    for (int i=0; i<m*n; i++) vel[i] *= w;

    for (int i=0; i<n; i++) {
        vel[i*m+gbest[i]] += updateItem(c1, c2, gbest, pbest, p, i, gbest[i]);
        vel[i*m+pbest[i]] += updateItem(c1, c2, gbest, pbest, p, i, pbest[i]);
        vel[i*m+p[i]] += updateItem(c1, c2, gbest, pbest, p, i, p[i]);
    }
}