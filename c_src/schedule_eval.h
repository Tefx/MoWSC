typedef struct {
    int num;
    int *parents;
} Preds;

typedef struct {
    int num_task;
    int num_type;
    double* ref_time;
    double* prices;
    double* cus;
    Preds* preds;
} Problem;

void init_problem(int n_tasks, int n_types);
void setup_service(double* prices, double* cus);
void setup_tasks(double* ref_time);
void setup_preds(int no, int n, int* parents);
void finish_problem();
