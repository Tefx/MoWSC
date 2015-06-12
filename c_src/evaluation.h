# include <stddef.h>

typedef struct {
    size_t num;
    int *parents;
} Preds;

typedef struct {
    size_t num_task;
    size_t num_type;
    double* ref_time;
    double* prices;
    double* cus;
    Preds* preds;
} Problem;

void init_problem(size_t n_tasks, size_t n_types);
void setup_service(double* prices, double* cus);
void setup_tasks(double* ref_time);
void setup_preds(size_t no, size_t n, int* parents);
void finish_problem(void);

void compute_objs(int* order, int* task2ins, size_t num_ins, int* ins2type,
                  double* makespan, double* cost);
