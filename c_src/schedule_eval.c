#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "schedule_eval.h"

Problem prob;

void init_problem(int n_tasks, int n_types) {
    printf("Setting problem in C library...\n");

    prob.num_task = n_tasks;
    prob.num_type = n_types;
    prob.ref_time = malloc(sizeof(double)*n_tasks);
    prob.prices = malloc(sizeof(double)*n_types);
    prob.cus = malloc(sizeof(double)*n_types);
    prob.preds = malloc(sizeof(Preds)*n_tasks);

    printf("Num_task: %d\nNum_type: %d\n", prob.num_task, prob.num_type);
}

void finish_problem(){
    printf("Cleaning up problem in C library...\n");

    free(prob.ref_time);
    prob.ref_time=NULL;

    free(prob.prices);
    prob.prices=NULL;

    free(prob.cus);
    prob.cus=NULL;

    for (int i=0;i<prob.num_task;i++)
        free(prob.preds[i].parents);
    free(prob.preds);
    prob.preds=NULL;

    printf("Finished everything in C library.\n");
}


void setup_service(double* prices, double* cus){
    memcpy(prob.prices, prices, sizeof(float)*prob.num_type);
    memcpy(prob.cus, cus, sizeof(double)*prob.num_type);

    for (int i=0;i<prob.num_type;i++)
        printf("No.%d: $%f for %f CU\n", i, prob.prices[i], prob.cus[i]);
}

void setup_tasks(double* ref_time){
    memcpy(prob.ref_time, ref_time, sizeof(double)*prob.num_task);

    for (int i=0;i<prob.num_task;i++)
        printf("Task %d has runtime %f\n", i, prob.ref_time[i]);
}

void setup_preds(int no, int n, int* parents){
    prob.preds[no].parents = malloc(sizeof(int)*n);
    prob.preds[no].num = n;
    memcpy(prob.preds[no].parents, parents, sizeof(int)*n);

    printf("Task %d has parents:", no);
    for (int i=0;i<prob.preds[no].num;i++){
        printf(" %d", prob.preds[no].parents[i]);
    }
    printf("\n");
}
