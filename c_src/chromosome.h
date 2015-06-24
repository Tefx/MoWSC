#ifndef CHROMOSOME_H
#define CHROMOSOME_H

#include <stddef.h>

void prob_select(double p, size_t n, int* decisions);

void ol_trans(int n, int* ys, int* xs);

#endif
