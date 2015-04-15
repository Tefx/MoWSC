class Normaliser(object):
    def __init__(self, rs=[], pf=None):
        self.max = [-float("Inf"), -float("Inf")]
        self.min = [float("Inf"), float("Inf")]
        self.delta = [0, 0]
        if not pf:
            pf = ParetoFilter(rs)
            self.update(pf.front)

    def update(self, ls):
        for x in ls:
            for i in range(2):
                if x[i] > self.max[i]:
                    self.max[i] = x[i]
                if x[i] < self.min[i]:
                    self.min[i] = x[i]
        self.delta = [x - y for x, y in zip(self.max, self.min)]

    def __call__(self, x):
        return [(x[i] - self.min[i]) / self.delta[i] for i in range(2)]


def dominate(a, b):
    if a[0] < b [0]:
        return a[1] <= b[1]
    elif a[0] == b[0]:
        return a[1] == b[1]
    else:
        return False


class ParetoFilter(object):
    def __init__(self, rs=[]):
        self.front = []
        for _, r in rs:
            self.update(r["results"])

    def add_point(self, p):
        r = []
        for x in self.front:
            if dominate(x, p):
                return
            elif not dominate(p, x):
                r.append(x)
        self.front = r
        self.front.append(p)

    def update(self, ps):
        for p in ps:
            self.add_point(p)

    def test(self, p):
        for x in self.front:
            if dominate(x, p):
                return False
        return True

    def __call__(self, ps):
        return filter(self.test, ps)