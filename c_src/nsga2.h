//
// Created by tefx on 9/24/15.
//

#ifndef MOWSC_NSGA2_H
#define MOWSC_NSGA2_H

#include <stddef.h>

void nsga2_select_2d(double* x_list, double* y_list,
                     size_t num_given,  size_t num_selected,
                     size_t* selected);

#endif //MOWSC_NSGA2_H
