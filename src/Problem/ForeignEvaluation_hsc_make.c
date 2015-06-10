#include "/usr/lib64/ghc-7.8.4/template-hsc.h"
#line 45 "ForeignEvaluation.hsc"
#include "schedule_eval.h"
#line 46 "ForeignEvaluation.hsc"
#define hsc_alignment(t ) hsc_printf ( "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__));

int main (int argc, char *argv [])
{
    hsc_line (1, "ForeignEvaluation.hsc");
    hsc_fputs ("{-# LANGUAGE ForeignFunctionInterface #-}\n"
           "", hsc_stdout());
    hsc_line (2, "ForeignEvaluation.hsc");
    hsc_fputs ("\n"
           "module Problem.ForeignEvaluation () where\n"
           "\n"
           "import           Data.Vector.Storable as VS\n"
           "import           Foreign\n"
           "\n"
           "import qualified Problem              as P\n"
           "\n"
           "data CProblem = CProblem { nTask   :: Int\n"
           "                         , nType   :: Int\n"
           "                         , refTime :: VS.Vector Double\n"
           "                         , preds   :: VS.Vector (VS.Vector Int)\n"
           "                         , prices  :: VS.Vector Double\n"
           "                         , cus     :: VS.Vector Double}\n"
           "\n"
           "data CSchedule = CSchedule { order    :: VS.Vector Int\n"
           "                           , task2ins :: VS.Vector Int\n"
           "                           , ins2type :: VS.Vector Int\n"
           "                           , nIns     :: Int}\n"
           "\n"
           "data CObjs = CObjs { makespan :: Double\n"
           "                   , cost     :: Double}\n"
           "\n"
           "\n"
           "toCProb::P.Problem->CProblem\n"
           "toCProb p = let n = nTask p\n"
           "                rt = VS.generate n (P.refTime p) p\n"
           "                pds = VS.generate n (VS.fromList . P.preds p) p\n"
           "                prs = VS.generate n (P.insPrice p) p\n"
           "                cus = VS.generate n (P.cu p) p\n"
           "            in CProblem n (P.nType p) rt pds prs cus\n"
           "\n"
           "toSchd::P.Schedule->CSchedule\n"
           "toSchd s = let o = VS.fromList $ P.orderStr s\n"
           "               t2i = VS.convert $ P.task2insStr s\n"
           "               i2t = VS.convert $ P.ins2typeStr s\n"
           "           in CSchedule o t2i i2t $ VS.length i2t\n"
           "\n"
           "fromCObjs::CObjs->(Double, Double)\n"
           "fromCObjs (CObjs m c) = (m, c)\n"
           "\n"
           "-- For C\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (46, "ForeignEvaluation.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (47, "ForeignEvaluation.hsc");
    hsc_fputs ("\n"
           "instance Storable CProblem where\n"
           "  sizeof _ = (", hsc_stdout());
#line 49 "ForeignEvaluation.hsc"
    hsc_size (Problem);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (50, "ForeignEvaluation.hsc");
    hsc_fputs ("  alignment _ = (", hsc_stdout());
#line 50 "ForeignEvaluation.hsc"
    hsc_alignment (Problem);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (51, "ForeignEvaluation.hsc");
    hsc_fputs ("  peek ptr = do\n"
           "    n <- ", hsc_stdout());
#line 52 "ForeignEvaluation.hsc"
    hsc_peek (Problem, n_task);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (53, "ForeignEvaluation.hsc");
    hsc_fputs ("    nt <- ", hsc_stdout());
#line 53 "ForeignEvaluation.hsc"
    hsc_peek (Problem, n_type);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (54, "ForeignEvaluation.hsc");
    hsc_fputs ("    rt <- ", hsc_stdout());
#line 54 "ForeignEvaluation.hsc"
    hsc_peek (Problem, reft);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (55, "ForeignEvaluation.hsc");
    hsc_fputs ("    pd <- ", hsc_stdout());
#line 55 "ForeignEvaluation.hsc"
    hsc_peek (Problem, preds);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (56, "ForeignEvaluation.hsc");
    hsc_fputs ("    pi <- ", hsc_stdout());
#line 56 "ForeignEvaluation.hsc"
    hsc_peek (Problem, prices);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (57, "ForeignEvaluation.hsc");
    hsc_fputs ("    cu <- ", hsc_stdout());
#line 57 "ForeignEvaluation.hsc"
    hsc_peek (Problem, cus);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (58, "ForeignEvaluation.hsc");
    hsc_fputs ("    return $ CProblem n nt rt pd pi cu\n"
           "  poke ptr (CProblem n nt rt pd pi cu) = do\n"
           "    ", hsc_stdout());
#line 60 "ForeignEvaluation.hsc"
    hsc_poke (Problem, n_task);
    hsc_fputs (" ptr n\n"
           "", hsc_stdout());
    hsc_line (61, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 61 "ForeignEvaluation.hsc"
    hsc_poke (Problem, n_type);
    hsc_fputs (" ptr nt\n"
           "", hsc_stdout());
    hsc_line (62, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 62 "ForeignEvaluation.hsc"
    hsc_poke (Problem, reft);
    hsc_fputs (" ptr rt\n"
           "", hsc_stdout());
    hsc_line (63, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 63 "ForeignEvaluation.hsc"
    hsc_poke (Problem, preds);
    hsc_fputs (" ptr pd\n"
           "", hsc_stdout());
    hsc_line (64, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 64 "ForeignEvaluation.hsc"
    hsc_poke (Problem, prices);
    hsc_fputs (" ptr pi\n"
           "", hsc_stdout());
    hsc_line (65, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 65 "ForeignEvaluation.hsc"
    hsc_poke (Problem, cus);
    hsc_fputs (" ptr cu\n"
           "", hsc_stdout());
    hsc_line (66, "ForeignEvaluation.hsc");
    hsc_fputs ("\n"
           "instance Storable CSchedule where\n"
           "  sizeof _ = ", hsc_stdout());
#line 68 "ForeignEvaluation.hsc"
    hsc_size (Schedule);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (69, "ForeignEvaluation.hsc");
    hsc_fputs ("  alignment _ = ", hsc_stdout());
#line 69 "ForeignEvaluation.hsc"
    hsc_alignment (Schedule);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (70, "ForeignEvaluation.hsc");
    hsc_fputs ("  peek ptr = do\n"
           "    o <- ", hsc_stdout());
#line 71 "ForeignEvaluation.hsc"
    hsc_peek (Schedule, order);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (72, "ForeignEvaluation.hsc");
    hsc_fputs ("    t2i <- ", hsc_stdout());
#line 72 "ForeignEvaluation.hsc"
    hsc_peek (Schedule, t2i);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (73, "ForeignEvaluation.hsc");
    hsc_fputs ("    i2t <- ", hsc_stdout());
#line 73 "ForeignEvaluation.hsc"
    hsc_peek (Schedule, i2t);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (74, "ForeignEvaluation.hsc");
    hsc_fputs ("    ni  <- ", hsc_stdout());
#line 74 "ForeignEvaluation.hsc"
    hsc_peek (Schedule, n_ins);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (75, "ForeignEvaluation.hsc");
    hsc_fputs ("    return $ CSchedule o t2i i2t ni\n"
           "  poke ptr (CSchedule o t2i i2t ni) = do\n"
           "    ", hsc_stdout());
#line 77 "ForeignEvaluation.hsc"
    hsc_poke (Schedule, order);
    hsc_fputs (" ptr o\n"
           "", hsc_stdout());
    hsc_line (78, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 78 "ForeignEvaluation.hsc"
    hsc_poke (Schedule, t2i);
    hsc_fputs (" ptr t2i\n"
           "", hsc_stdout());
    hsc_line (79, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 79 "ForeignEvaluation.hsc"
    hsc_poke (Schedule, i2t);
    hsc_fputs (" ptr i2t\n"
           "", hsc_stdout());
    hsc_line (80, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 80 "ForeignEvaluation.hsc"
    hsc_poke (Schedule, n_ins);
    hsc_fputs (" ptr ni\n"
           "", hsc_stdout());
    hsc_line (81, "ForeignEvaluation.hsc");
    hsc_fputs ("\n"
           "instance Storable CObjs where\n"
           "  sizeof _ = ", hsc_stdout());
#line 83 "ForeignEvaluation.hsc"
    hsc_size (Objs);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (84, "ForeignEvaluation.hsc");
    hsc_fputs ("  alignment _ = ", hsc_stdout());
#line 84 "ForeignEvaluation.hsc"
    hsc_alignment (Objs);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (85, "ForeignEvaluation.hsc");
    hsc_fputs ("  peek ptr = do\n"
           "    m <- ", hsc_stdout());
#line 86 "ForeignEvaluation.hsc"
    hsc_peek (Objs, makespan);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (87, "ForeignEvaluation.hsc");
    hsc_fputs ("    c <- ", hsc_stdout());
#line 87 "ForeignEvaluation.hsc"
    hsc_peek (Objs, cost);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (88, "ForeignEvaluation.hsc");
    hsc_fputs ("    return $ CObjs m c\n"
           "  poke ptr (CObjs m c) = do\n"
           "    ", hsc_stdout());
#line 90 "ForeignEvaluation.hsc"
    hsc_poke (Objs, makespan);
    hsc_fputs (" ptr m\n"
           "", hsc_stdout());
    hsc_line (91, "ForeignEvaluation.hsc");
    hsc_fputs ("    ", hsc_stdout());
#line 91 "ForeignEvaluation.hsc"
    hsc_poke (Objs, cost);
    hsc_fputs (" ptr c\n"
           "", hsc_stdout());
    hsc_line (92, "ForeignEvaluation.hsc");
    hsc_fputs ("", hsc_stdout());
    return 0;
}
