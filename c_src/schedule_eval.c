#include <stdio.h>
#include "schedule_eval.h"

void debug(Problem* p, Schedule* s) {
    int n = p->n_task;
    int* o = s->order;
    int* t2i = s->t2i;
    int* i2t = s->i2t;

    printf("Received %d tasks and %d types.\n", n, p->n_type);

    printf("Order:\t");
    for (int i=0;i<n;i++) printf(" %d", o[i]);
    printf("\n");

    printf("Task2ins:\t");
    for (int i=0;i<n;i++) printf(" %d", t2i[i]);
    printf("\n");

    printf("Ins2Type:\t");
    for (int i=0;i<s->n_ins;i++) printf(" %d", i2t[i]);
    printf("\n");
}
