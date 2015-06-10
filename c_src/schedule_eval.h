typedef struct {
    int *pds;
    int np;
} Preds;

typedef struct {
    int n_task;
    int n_type;
    float *reft;
    Preds *preds;
    float *prices;
    float *cus;
} Problem;

typedef struct {
    int n_ins;
    int *order;
    int *t2i;
    int *i2t;
} Schedule;

typedef struct {
    float makespan;
    float cost;
} Objs;

void eval(Problem*, Schedule*, Objs*);
void bulk_eval(Problem*, int, Schedule**, Objs**);

void debug(Problem*, Schedule*);
