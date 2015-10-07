import json as json
import couchdb
from config import db_addr, db_name
from config import query_cond


def build_fun(dag, keys, field):
    fun_code = """
def map_fun(doc):
    conds = %s
    dag = "%s"

    def valid(doc, dag):
        if doc["dag"] != dag:
            return False
        for k, v in conds.iteritems():
            if doc[k] not in v:
                return False
        return True

    if valid(doc, dag):
        yield "-".join(%s), doc["%s"]
"""
    ks =  "[%s]" % ",".join(["str(doc[\"%s\"])" % k for k in keys])

    return fun_code % (json.dumps(query_cond), dag, ks, field)

def query(dag, keys, field):
    db = couchdb.Server(db_addr)[db_name]
    for res in db.query(build_fun(dag, keys, field), language='python'):
        yield res.key, res.value

if __name__ == '__main__':
    for k,v in query("CyberShake_30", ["algorithm", "runtime_scale"], "results"):
        print k