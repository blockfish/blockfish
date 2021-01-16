const EventEmitter = require('events');
const protos = require('../generated/blockfish_pb.js');
const IPC = require('./ipc.js');

class AI extends EventEmitter {
    constructor(ipc) {
        super();
        Object.assign(this, {
            ipc: makeIPC(ipc),
            version: null,
            _init: false,
            _id: 0,
            _analysisCallbacks: {},
        });
        this.ipc.on('recv', this._onRecv.bind(this));
        this.ipc.on('error', e => this.emit('error', e));
    }

    kill() {
        this.ipc.kill();
    }

    analyze(snapshot, config, callback) {
        if (config instanceof Function) {
            callback = config;
            config = null;
        }
        this._analyze(snapshot, config, callback);
    }

    _analyze(snapshot, config, callback) {
        if (!this._init) {
            this.on('init', () => this._analyze(snapshot, config, callback));
            return;
        }

        let id = ++this._id;
        let analyzeCallback = () => {
            this._analysisCallbacks[id] = callback;
        };

        let setConfigCallback = () => {
            let ana = new protos.Request.Analyze;
            ana.setId(id);
            ana.setSnapshot(toSnapshotProto(snapshot));
            let analyzeReq = new protos.Request;
            analyzeReq.setAnalyze(ana);
            this.ipc.send(analyzeReq, analyzeCallback);
        };

        if (config === null) {
            setConfigCallback();
        } else {
            let setConfigReq = new protos.Request;
            setConfigReq.setSetConfig(toConfigProto(config));
            this.ipc.send(setConfigReq, setConfigCallback);
        }
    }

    _onRecv(res) {
        if (res.hasGreeting()) {
            let greeting = res.getGreeting();
            this.version = greeting.getVersion();
            if (!this._init) {
                this._init = true;
                this.emit('init');
            }
            this.emit('greet', greeting.getMotd());
        } else if (res.hasFinished()) {
            let analysis = res.getFinished();
            let id = analysis.getId();
            let cb = this._analysisCallbacks[id];
            this._analysisCallbacks[id] = null;
            cb(fromAnalysisProto(analysis));
        }
    }
}

AI.DEFAULT_BLOCKFISH_PATH = 'blockfish';

function makeIPC(arg) {
    if (arg === undefined) {
        arg = AI.DEFAULT_BLOCKFISH_PATH;
    }
    if (typeof arg === 'string') {
        return IPC.childProcess(arg, []);
    } else {
        return arg;
    }
}

function toConfigProto(arg) {
    let cfg = new protos.Request.Config;
    cfg.setNodeLimit(arg.node_limit || 0);
    return cfg;
}

function toSnapshotProto(arg) {
    let ss = new protos.Snapshot;
    ss.setHold(arg.hold || "");
    ss.setQueue(arg.queue);
    let rows = typeof arg.matrix === 'string' ?
        arg.matrix.split('\n') :
        arg.matrix.slice(0);
    ss.setRowsList(rows.reverse());
    return ss;
}

function fromAnalysisProto(arg) {
    let stats = arg.getStats();
    let suggs = arg.getSuggestionsList();
    return {
        suggestions: suggs.map(fromSuggestionProto),
        statistics: {
            nodes: stats.getNodes(),
            iterations: stats.getIters(),
            timeTaken: stats.getTimeMs() / 1000,
        },
    };
}

function fromSuggestionProto(arg) {
    return {
        rating: arg.getRating(),
        inputs: arg.getInputsList().map(fromInputProto).join(' '),
    };
}

function fromInputProto(arg) {
    switch (arg) {
    case protos.Input.LEFT: return 'left';
    case protos.Input.RIGHT: return 'right';
    case protos.Input.CW: return 'cw';
    case protos.Input.CCW: return 'ccw';
    case protos.Input.HOLD: return 'hold';
    case protos.Input.SD: return 'sd';
    case protos.Input.HD: return 'hd';
    }
}

module.exports = AI;
