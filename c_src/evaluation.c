#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "evaluation.h"

static Problem prob;

void init_problem(size_t n_tasks, size_t n_types) {
    prob.num_task = n_tasks;
    prob.num_type = n_types;
    prob.ref_time = (double*)malloc(sizeof(double)*n_tasks);
    prob.prices = (double*)malloc(sizeof(double)*n_types);
    prob.cus = (double*)malloc(sizeof(double)*n_types);
    prob.preds = (Preds*)malloc(sizeof(Preds)*n_tasks);
}

void finish_problem(){
    free(prob.ref_time);
    free(prob.prices);
    free(prob.cus);
    for (size_t i=0;i<prob.num_task;i++)
        free(prob.preds[i].parents);
    free(prob.preds);
}

void setup_service(double* prices, double* cus){
    memcpy(prob.prices, prices, sizeof(double)*prob.num_type);
    memcpy(prob.cus, cus, sizeof(double)*prob.num_type);
}

void setup_tasks(double* ref_time){
    memcpy(prob.ref_time, ref_time, sizeof(double)*prob.num_task);
}

void setup_preds(size_t no, size_t n, int* parents){
    prob.preds[no].parents = (int*)malloc(sizeof(int)*n);
    prob.preds[no].num = n;
    memcpy(prob.preds[no].parents, parents, sizeof(int)*n);
}

void compute_objs(int* order, int* task2ins, size_t num_ins, int* ins2type,
                  double* makespan, double* cost){
    double* eft;
    double* avail;
    double* boot;

    int task;
    int ins;
    double st;
    int parent;

    eft = (double*)malloc(sizeof(double)*prob.num_task);
    avail = (double*)malloc(sizeof(double)*num_ins);
    boot = (double*)malloc(sizeof(double)*num_ins);

    for (size_t i=0;i<num_ins;i++) {
        avail[i] = 0;
        boot[i] = -1;
    }
    for (size_t i=0;i<prob.num_task;i++) eft[i] = 0;

    for (size_t i=0;i<prob.num_task;i++){
        task = order[i];
        ins = task2ins[task];
        st = avail[ins];
        for (size_t j=0;j<prob.preds[task].num;j++) {
            parent = prob.preds[task].parents[j];
            if (st < eft[parent]) st = eft[parent];
        }
        eft[task] = st + prob.ref_time[task] / prob.cus[ins2type[ins]];
        avail[ins] = eft[task];
        if (boot[ins]<0) boot[ins] = st;
    }

    *makespan = eft[prob.num_task-1];
    *cost = 0;

    for (size_t i=0;i<num_ins;i++)
        if (avail[i] > 0)
            *cost += ceil((avail[i] - boot[i]) / 3600) * prob.prices[ins2type[i]];

    free(boot);
    free(avail);
    free(eft);
}
