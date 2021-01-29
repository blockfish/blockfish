import asyncio
from collections import namedtuple

from blockfish.ipc import IPC, create_subprocess_ipc
import blockfish.blockfish_pb2 as protos


INPUT_NAMES = ('left', 'right', 'cw', 'ccw', 'hold', 'sd', 'hd')

Snapshot = namedtuple('Snapshot', [
        'queue',
        'hold',
        'matrix',
])

Statistics = namedtuple('Statistics', [
        'nodes',
        'iterations',
        'time_taken',
])

Suggestion = namedtuple('Suggestion', [
        'rating',
        'inputs',
])


class AI:
    def __init__(self):
        self._task = None
        self._next_id = 0
        self._analysis = dict()
        self._deinit()

    async def start(self):
        if self._task is None:
            self._init = asyncio.Event()
            self._ipc = None
            self._task = asyncio.create_task(self._go())
        await self._init.wait()
        return self.version

    def shutdown(self):
        if self._task is not None:
            self._task.cancel()
        self._deinit()

    async def analyze(self, snapshot, **cfg):
        # ensure IPC is connected
        await self.start()
        ipc = self._ipc
        # allocate id, create queue for callback
        id = self._next_id
        self._next_id += 1
        self._analysis[id] = asyncio.Queue(maxsize = 1)
        # build and send request
        if len(cfg) > 0:
            req = to_set_config_proto(cfg)
            await ipc.send(req)
        req = to_analyze_proto(id, snapshot)
        await ipc.send(req)
        # wait for callback
        fin = await self._analysis[id].get()
        del self._analysis[id]
        # parse response data
        stats = from_stats_proto(fin.stats)
        suggs = [from_suggestion_proto(s) for s in fin.suggestions]
        return suggs, stats

    def _deinit(self):
        self.version = None
        self._init = None
        self._ipc = None
        self._task = None

    async def _go(self):
        ipc = await create_subprocess_ipc()
        self._ipc = ipc
        while True:
            try:
                res = await ipc.recv()
            except asyncio.CancelledError:
                break
            tag = res.WhichOneof('res')
            if tag == 'greeting':
                self.version = res.greeting.version
                self._init.set()
            elif tag == 'finished':
                fin = res.finished
                self._analysis[fin.id].put_nowait(fin)
        await ipc.kill()

def to_set_config_proto(cfg):
    req = protos.Request()
    if 'node_limit' in cfg:
        req.set_config.node_limit = cfg['node_limit']
    if 'suggestion_limit' in cfg:
        req.set_config.max_results = cfg['suggestion_limit']
    return req

def to_analyze_proto(id, ss):
    req = protos.Request()
    req.analyze.id = id
    if ss.queue is not None:
        req.analyze.snapshot.queue = ss.queue
    if ss.hold is not None:
        req.analyze.snapshot.hold = ss.hold
    req.analyze.snapshot.rows.extend(ss.matrix)
    return req

def from_stats_proto(proto):
    return Statistics(
        nodes = proto.nodes,
        iterations = proto.iterations,
        time_taken = proto.time_taken_millis * 0.001,
    )

def from_suggestion_proto(proto):
    return Suggestion(
        rating = proto.rating,
        inputs = [INPUT_NAMES[i] for i in proto.inputs],
    )
