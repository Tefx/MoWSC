#!/usr/bin/env python

import json as json
import couchdb
from config import CONFIG, DBNAME, qCond, qOA, CouchDB_Addr

fun_code = ('\n'
            '\tdef valid(doc):\n'
            '\t\tif doc["ga"]:\n'
            '\t\t\tfor k,v in query.iteritems():\n'
            '\t\t\t\tif doc[k] not in v:\n'
            '\t\t\t\t\treturn False\n'
            '\t\t\treturn True\n'
            '\t\telif doc["alg"] in qOA:\n'
            '\t\t\treturn True\n'
            '\t\telse:\n'
            '\t\t\treturn False\n'
            '\n'
            '\tif doc[\'dag\'] == dag and valid(doc):\n'
            '\t\tyield None, doc')


def build_fun(dag):
    qc = '\tquery=%s' % json.dumps(qCond)
    qoa = '\tqOA=%s' % json.dumps(qOA)
    return 'def map_fun(doc):\n' \
           '\tdag=\"%s\"\n%s\n%s' % (dag, qc, qoa) + fun_code


def query(dag, q):
    db = couchdb.Server(CouchDB_Addr)[DBNAME]
    rs = []
    for res in db.query(build_fun(dag), language='python'):
        d = res.value
        rs.append((format_doc(d, q), d))
    return rs


def format_doc(d, q):
    if not d["ga"]:
        return d["alg"]
    fs = []
    for k, v in CONFIG.iteritems():
        if k not in q and len(v) > 1:
            fs.append(k)
        elif len(q[k]) > 1:
            fs.append(k)
    ks = [d["alg"]] + ["%s-%s" % (k, d[k]) for k in fs]
    return "_".join(ks)


if __name__ == '__main__':
    from config import qCond
    for r in query("Montage_25", qCond):
        print r
