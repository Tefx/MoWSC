//
// Created by tefx on 9/24/15.
//
#define _GNU_SOURCE

#include <malloc.h>
#include <stdlib.h>
#include "select.h"
#include "nsga2.h"

typedef struct list_node {
    size_t data;
    struct list_node *next;
} list;

void add_item(list **l, size_t d) {
    list *tmp = malloc(sizeof(list));
    tmp->data = d;
    tmp->next = *l;
    *l = tmp;
}

void free_list(list **l) {
    list *tmp;
    while (*l) {
        tmp = *l;
        *l = (*l)->next;
        free(tmp);
    }
}

int cmp_l(const void* x, const void* y, void* l) {
    return cmp_double((double*)l+*(const size_t*)x, (double*)l+*(const size_t*)y);
}

void crowd_cut(list** l, size_t nn, size_t n, size_t m, double* xs, double* ys) {
    double* dis = (double*) malloc(sizeof(double) * nn);
    size_t* a = (size_t*) malloc(sizeof(size_t) * n);
    list* tmp;
    int k = 0;

    for (size_t i=0; i<nn; i++) dis[i] = 0;

    while (*l) {
        a[k++] = (*l)->data;
        tmp = *l;
        *l = (*l)->next;
        free(tmp);
    }


    qsort_r(a, n, sizeof(size_t), cmp_l, xs);
    dis[a[0]] = dis[a[n-1]] = INF;
    if (xs[a[n-1]] != xs[a[0]])
        for (size_t i=1; i<n-1; i++)
            dis[a[i]] += (xs[a[i+1]] - xs[a[i-1]]) / (xs[a[n-1]] - xs[a[0]]);

    qsort_r(a, n, sizeof(size_t), cmp_l, ys);
    dis[a[0]] = dis[a[n-1]] = INF;
    if (ys[a[n-1]] != ys[a[0]])
        for (size_t i=1; i<n-1; i++)
            dis[a[i]] += (ys[a[i+1]] - ys[a[i-1]]) / (ys[a[n-1]] - ys[a[0]]);

    qsort_r(a, n, sizeof(size_t), cmp_l, dis);
    for (size_t i=1; i<=m; i++)
        add_item(l, a[n-i]);

    free(a);
    free(dis);
}

void nsga2_select_2d(double *xs, double *ys, size_t n, size_t m, size_t *res) {
    size_t *dc = (size_t *) malloc(sizeof(size_t) * n);
    list **s = (list **) malloc(sizeof(list *) * n);
    list *resF = NULL;
    list *curF = NULL;
    list *nextF = NULL;
    list* tmp;
    size_t cur_count = 0;
    size_t selected = 0;

    for (size_t i=0; i<n; i++) {
        dc[i] = 0;
        s[i] = NULL;
    }

    for (size_t i=0; i<n; i++) {
        for (size_t j=0; j<n; j++) {
            if (pareto_dominate_2d(xs[i], ys[i], xs[j], ys[j])) {
                add_item(&s[i], j);
                dc[j]++;
            }
        }
    }

    for (size_t i=0; i<n; i++)
        if (dc[i] == 0) {
            add_item(&curF, i);
            cur_count++;
        }


    while (selected < m) {
        if (cur_count + selected > m)
            crowd_cut(&curF, n, cur_count, m-selected, xs, ys);

        tmp = curF;
        while (tmp) {
            add_item(&resF, tmp->data);
            selected++;
            tmp = tmp->next;
        }

        if (selected == m) {
            free_list(&curF);
            break;
        }

        list *fi = curF;
        cur_count = 0;
        while (fi) {
            tmp = s[fi->data];
            while (tmp) {
                dc[tmp->data]--;
                if (dc[tmp->data] == 0) {
                    add_item(&nextF, tmp->data);
                    cur_count++;
                }
                tmp = tmp->next;
            }
            tmp = fi;
            fi = fi->next;
            free(tmp);
        }

        curF = nextF;
        nextF = NULL;
    }

    size_t k=0;
    while (resF) {
        res[k++] = resF->data;
        tmp = resF;
        resF = resF->next;
        free(tmp);
    }

//    printf("%d %d\n", (int)m, (int)k);
//    for (size_t i=0; i<m; i++) {
//        printf("%lu(%f,%f) ", res[i], xs[i], ys[i]);
//    }
//    printf("\n");

    for (size_t i = 0; i < n; i++) free_list(&s[i]);
    free(s);
    free(dc);
}
