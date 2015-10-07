//
// Created by tefx on 9/29/15.
//

#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <stdio.h>
#include "pso.h"

bool initialized = false;

double updateItem(double c1, double c2, int* gbest, int* pbest, int* p, int i, int j) {
    int gb = (gbest[i] == j?1:0);
    int pb = (pbest[i] == j?1:0);
    double pp = (p[i] == j?1:0);
    double t1 = pb - pp;
    double t2 = gb - pp;
    double res = 0;

    if (t1) res += c1 * t1 / RAND_MAX * rand();
    if (t2) res += c2 * t2 / RAND_MAX * rand();
    return res;
}

void updateVelocity(int n, int m, double w, double c1, double c2, int* gbest, int* pbest, int* p, double* vel){
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = true;
    }

    for (int i=0; i<m*n; i++) vel[i] *= w;

    for (int i=0; i<n; i++) {
        vel[i*m+gbest[i]] += updateItem(c1, c2, gbest, pbest, p, i, gbest[i]);

        if (gbest[i] != pbest[i])
            vel[i*m+pbest[i]] += updateItem(c1, c2, gbest, pbest, p, i, pbest[i]);

        if (p[i] != gbest[i] && p[i] != pbest[i])
            vel[i*m+p[i]] += updateItem(c1, c2, gbest, pbest, p, i, p[i]);
    }
}

void randVelocity(int n, double* vel) {
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = true;
    }

    for (int i=0; i<n; i++)
        vel[i] = (double)rand() / RAND_MAX * 2 - 1;
}

void updatePosition(int n, int m, double* vel, int* pos){
    double max;
    for (int i=0; i<n; i++){
        max = vel[i*m];
        pos[i] = 0;
        for (int j=i*m+1;j<i*m+m; j++){
            if (vel[j] > max) {
                max = vel[j];
                pos[i] = j-i*m;
            }
        }
    }
}