import json as json
import couchdb
from config import db_addr, db_name
from config import query_cond


def build_fun(dag, field):
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
            else:
                return True

    if valid(doc, dag):
        yield doc["algorithm"], doc["%s"]
"""
    return fun_code % (json.dumps(query_cond), dag, field)

def query(dag, field):
    db = couchdb.Server(db_addr)[db_name]
    for res in db.query(build_fun(dag, field), language='python'):
        yield res.key, res.value

if __name__ == '__main__':
    for k,v in query("CyberShake_30", "results"):
        print k