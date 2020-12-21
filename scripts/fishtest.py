#!/usr/bin/env python3
import re, sqlite3, sys, random, time, argparse, math, itertools
from subprocess import Popen, PIPE, DEVNULL

class Cfg:
    PARAMS = ('ai0', 'ai1', 'ai2', 'ai3', 'ai4')

    def __init__(self, x):
        if isinstance(x, list) or isinstance(x, tuple):
            params = Cfg.PARAMS
            values = x
        elif isinstance(x, str):
            params = Cfg.PARAMS
            values = Cfg._parse(x)
            if values is None:
                raise ValueError('invalid AI configuration: {!r}'.format(x))
        else:
            params = set(x.keys())
            values = (x[k] for k in params)
        self.params = params
        self.dct = {p: v for p, v in zip(params, values)}

    def _parse(s):
        m = re.match(r'^(\d+)/(\d+(,\d+){3})$', s)
        return m and (int(m[1]), *(int(v) for v in m[2].split(',')))

    def to_sql_expr(self):
        return ' AND '.join('{} = :{}'.format(p, p) for p in self.params) or 'TRUE'

    def __getitem__(self, p):
        return self.dct.get(p, None)

    def __repr__(self):
        return '{}/{},{},{},{}'.format(
            self.dct.get('ai0', '*'),
            self.dct.get('ai1', '*'),
            self.dct.get('ai2', '*'),
            self.dct.get('ai3', '*'),
            self.dct.get('ai4', '*'),
        )

    def heap_size(self):
        return self['ai0']

    def _weight(self, w):
        ai2 = self['ai2'] # piece est
        ai_w = self[w]
        if ai2 is not None and ai_w is not None:
            return ai_w / ai2
        else:
            return None

    def row_count_weight(self):
        return self._weight('ai1')

    def i_dep_weight(self):
        return self._weight('ai3')

    def piece_penalty(self):
        return self._weight('ai4')

class Race:
    def __init__(self, **args):
        args, cmd = Race._craft_command(**args)
        self.args = args
        self.cmd = cmd
        self.proc = Popen(cmd, stdout = PIPE, stderr = DEVNULL)
        self.result = None

    def _craft_command(
            goal,
            cfg,
            seed = None,
    ):
        args = { 'goal': goal, 'cfg': cfg }
        cmd = ['blockfish-race', str(goal), '-q', '-A', repr(cfg)]
        if seed is not None:
            cmd.append('-s')
            cmd.append(seed)
        return args, cmd

    def _parse_output(stream):
        data = {'topout': False}
        for line in stream:
            m = re.match(rb'(\d+) pieces\n', line)
            if m:
                data['pc'] = int(m[1])
            m = re.match(rb'(\d+)L downstack\n', line)
            if m:
                data['ds'] = int(m[1])
            m = re.match(rb'total time: ([0-9.]+)s \(.*\)\n', line)
            if m:
                data['time'] = float(m[1])
            m = re.match(rb'PRNG seed: (\d+)\n', line)
            if m:
                data['seed'] = str(m[1], 'utf8')
            m = re.match(rb'topped out early\n', line)
            if m:
                data['topout'] = True
        return data

    def poll(self):
        if self.result is None and self.proc.poll() is not None:
            self.wait()
        return self.result

    def wait(self):
        if self.result is None:
            self.result = Race._parse_output(self.proc.stdout)
        return self.result

    def stop(self):
        self.proc.send_signal(2)
        return self.wait()

class DB:
    def __init__(self, path, auto_create = True):
        self.path = path
        self.conn = sqlite3.connect(path)
        if auto_create:
            self.create()

    def create(self):
        c = self.conn.cursor()
        try:
            c.execute("""
            CREATE TABLE fishtest
                ( seed TEXT
                , time REAL
                , topout INTEGER
                , ds INTEGER
                , pc INTEGER
                , ai0 INTEGER
                , ai1 INTEGER
                , ai2 INTEGER
                , ai3 INTEGER
                , ai4 INTEGER )
            """)
            self.conn.commit()
            return True
        except sqlite3.OperationalError:
            return False

    def save(self, race):
        cfg = race.args['cfg']
        result = race.wait()
        c = self.conn.cursor()
        c.execute("""
        INSERT INTO fishtest
            (seed, time, topout, ds, pc, ai0, ai1, ai2, ai3, ai4)
        VALUES
            (:seed, :time, :topout, :ds, :pc, :ai0, :ai1, :ai2, :ai3, :ai4)
        """, {
            'seed': result.get('seed'),
            'time': result.get('time'),
            'topout': 1 if result.get('topout') else 0,
            'ds': result.get('ds'),
            'pc': result.get('pc'),
            **{p: cfg[p] for p in Cfg.PARAMS}
        })
        self.conn.commit()

    def list_cfgs(self, params = ('ai{}'.format(i) for i in range(5))):
        c = self.conn.cursor()
        cfgs = {}
        for p in params:
            c.execute('SELECT DISTINCT {} FROM fishtest'.format(p))
            cfgs[p] = [val for [val] in c]
        return cfgs

    def calculate_piece_count_dist(self, cfg):
        c = self.conn.cursor()
        c.execute("""
        SELECT
            count(*), sum(pc), sum(pc*pc)
        FROM fishtest
        WHERE
            NOT topout AND {}
        """.format(cfg.to_sql_expr()),
            cfg.dct
        )
        n, sm, smsq = c.fetchone()
        if n == 0:
            return None
        else:
            return Dist(n, sm, smsq)

    def piece_counts(self, ds = None, cfg_filter = None, include_topout = False):
        args = {}
        topout_expr = 'TRUE' if include_topout else 'NOT topout'
        filter_expr = 'TRUE' if cfg_filter is None else cfg_filter.to_sql_expr()
        if ds is None:
            ds_expr = 'TRUE'
        else:
            ds_expr = 'ds = :ds'
            args['ds'] = ds

        c = self.conn.cursor()
        c.execute("""
        SELECT
            ds, pc, ai0, ai1, ai2, ai3, ai4
        FROM fishtest
        WHERE
            {} AND {} AND {}
        """.format(topout_expr, filter_expr, ds_expr), args)
        return (
            (Cfg(cfg_vals), ds, pc)
            for [ds, pc, *cfg_vals] in c
        )

class Dist:
    def __init__(self, n, sm, smsq):
        self.n = n
        self.avg = sm / n
        self.sdsq = smsq / n - sm ** 2 / n ** 2
        self.sd = math.sqrt(self.sdsq)

    def __repr__(self):
        return 'samples: {}, avg: {:.2f}, sd: {:.2f}'.format(self.n, self.avg, self.sd)

def run(db, goal, cfg, jobs):
    db = DB(db)
    cfgs = [Cfg(s) for s in cfg]
    races = [None for i in range(jobs)]

    print('---------------------')
    print('{}L cheese race'.format(goal))
    print('saving to {}'.format(repr(db.path)))
    print('sampling {} different configurations'.format(len(cfg)))
    print('running {} jobs in parallel'.format(jobs))
    print('---------------------')

    n_saved = 0
    start_time = time.time()
    def status():
        return '{} completed in {:.1f}s'.format(n_saved, elapsed)

    try:
        while True:
            for i in range(len(races)):
                if races[i] is None:
                    cfg = random.choice(cfgs)
                    races[i] = Race(goal = goal, cfg = cfg)

            for i in range(len(races)):
                race = races[i]
                if race.poll() is not None:
                    races[i] = None
                    db.save(race)
                    n_saved += 1

            elapsed = time.time() - start_time
            sys.stdout.write('\r{} '.format(status()))
            sys.stdout.flush()
            time.sleep(0.5)
    except KeyboardInterrupt:
        print('\ncaught ctrl-c\n{}'.format(status()))
        for race in races:
            if race is not None:
                race.stop()
                race.wait()

def analyze(db, cfg_name):
    db = DB(db, auto_create = False)
    print('---------------------')
    print('using database {}'.format(repr(db.path)))
    print('---------------------')
    keys = set(cfg_name)
    cfg_value_dist = db.list_cfgs(keys)
    all_cfgs = (
        Cfg({k: v for k, v in zip(cfg_value_dist.keys(), vals)})
        for vals in itertools.product(*cfg_value_dist.values())
    )
    all_dists = []
    for cfg in all_cfgs:
        pc_dist = db.calculate_piece_count_dist(cfg)
        if pc_dist:
            all_dists.append((cfg, pc_dist))
    all_dists.sort(key = lambda x: x[1].avg)
    for cfg, dist in all_dists:
        print('({!r}) {!r}'.format(cfg, dist))

def plot(db, ds):
    db = DB(db, auto_create = False)
    print('---------------------')
    print('using database {}'.format(repr(db.path)))
    print('---------------------')

    PREC = 60
    pace_by_rcw = dict()
    pace_by_idw = dict()
    pace_by_pp = dict()

    def include(w_rc, w_id, pp):
        return pp == PREC

    for cfg, ds, pc in db.piece_counts(ds = ds, include_topout = False):
        pace = pc / ds * 100
        rcw = int(PREC * cfg.row_count_weight())
        idw = int(PREC * cfg.i_dep_weight())
        pp = int(PREC * cfg.piece_penalty())
        if include(rcw, idw, pp):
            pace_by_rcw.setdefault(rcw, []).append(pace)
            pace_by_idw.setdefault(idw, []).append(pace)
            pace_by_pp.setdefault(pp, []).append(pace)

    def show_dists(table, name):
        ws = list(table.keys())
        ws.sort()
        for w in ws:
            paces = table[w]
            dist = Dist(len(paces), sum(paces), sum(x*x for x in paces))
            print('pace({} = {:.2f}): {!r}'.format(name, w / PREC, dist))

    show_dists(pace_by_rcw, 'w_rc')
    print('----------')
    show_dists(pace_by_idw, 'w_id')
    print('----------')
    show_dists(pace_by_pp, 'pp')

def parse_args():
    ap = argparse.ArgumentParser(
        description = 'Run or analyze many Blockfish cheese races'
    )
    root = ap.add_subparsers(title = 'operation to perform')
    root.required = True
    root.dest = 'operation'

    run_ap = root.add_parser('run')
    run_ap.add_argument('--jobs', '-j', help = 'number of parallel processes', type = int)
    run_ap.add_argument('db', help = 'sqlite database file', type = str)
    run_ap.add_argument('goal', help = 'garbage lines to clear', type = int)
    run_ap.add_argument('cfg', help = 'AI configuration', type = str, nargs = '+')
    run_ap.set_defaults(operation = run, jobs = 8)

    an_ap = root.add_parser('analyze')
    an_ap.add_argument('db', help = 'sqlite database file', type = str)
    an_ap.add_argument('cfg_name', help = 'AI parameter to analyze', type = str, nargs = '*')
    an_ap.set_defaults(operation = analyze)

    plt_ap = root.add_parser('plot')
    plt_ap.add_argument('db', help = 'sqlite database file', type = str)
    plt_ap.add_argument('ds', help = 'garbage lines cleared', type = int, nargs = '?')
    plt_ap.set_defaults(operation = plot)

    return vars(ap.parse_args())

if __name__ == '__main__':
    args = parse_args()
    op = args['operation']
    del args['operation']
    op(**args)
