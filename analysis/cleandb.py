#!/usr/bin/env python

import couchdb
from config import DBNAME, CouchDB_Addr


def init_db():
    server = couchdb.Server(CouchDB_Addr)
    try:
        server.create(DBNAME)
    except couchdb.PreconditionFailed:
        del server[DBNAME]
        server.create(DBNAME)

if __name__ == '__main__':
    init_db()