//
// Created by tefx on 9/29/15.
//

#ifndef MOWSC_PSO_H
#define MOWSC_PSO_H

void updateVelocity(int n, int m, double w, double c1, double c2, int* gbest, int* pbest, int* p, double* vel);
void randVelocity(int n, double* vel);
void updatePosition(int n, int m, double* vel, int* pos);

#endif //MOWSC_PSO_H
