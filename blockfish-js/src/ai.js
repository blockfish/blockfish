const EventEmitter = require('events');
const protos = require('../generated/blockfish_pb.js');
const IPC = require('./ipc.js');

/**
 * Class representing a handle to a Blockfish AI process.
 */
class AI extends EventEmitter {
    /**
     * Construct a new AI.
     * @param {string} [blockfish] - Blockfish executable path to spawn for IPC.
     */
    constructor(blockfish) {
        super();
        Object.assign(this, {
            ipc: makeIPC(blockfish),
            version: null,
            _init: false,
            _id: 0,
            _analysisCallbacks: {},
        });
        this.ipc.on('recv', this._onRecv.bind(this));
        this.ipc.on('error', e => this.emit('error', e));
    }

    /**
     * Kills the process, preventing any further suggestions or analysis.
     */
    kill() { this.ipc.kill(); }

    /**
     * Starts an analysis. When the analysis finishes, the callback is called
     * with the result of the analysis.
     * @param {Object} snapshot - Snapshot to analyze.
     * @param {string} [snapshot.hold] - Piece in hold.
     * @param {string} snapshot.queue - Upcoming pieces in the queue.
     * @param {string[]} snapshot.matrix - Matrix rows, in order from bottom line to top.
     * @param {Object} [options] - Analysis options.
     * @param {number} [options.nodeLimit] - Max number of nodes to discover before cutting off search.
     * @param {number} [options.suggestionLimit] - Max number of suggestions to return.
     * @param {AI~analyzeCallback} callback - Called when the analysis completes.
     */
    analyze(snapshot, options, callback) {
        if (options instanceof Function) {
            callback = options;
            options = {};
        }
        this._analyze(snapshot, options, callback);
    }

    /**
     * Callback when an analysis completes.
     *
     * @callback AI~analyzeCallback
     * @param {Object} analysis - Describes the analysis results.
     * @param {AI~Suggestion[]} analysis.suggestions - Suggested sequences, in order from best to worst.
     * @param {AI~Statistics} analysis.statistics - Statistics about the analysis.
     */

    _analyze(snapshot, options, callback) {
        if (!this._init) {
            this.on('init', () => this._analyze(snapshot, options, callback));
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
            setAnalyzeProto(ana, options);
            let analyzeReq = new protos.Request;
            analyzeReq.setAnalyze(ana);
            this.ipc.send(analyzeReq, analyzeCallback);
        };

        if (options === null) {
            setConfigCallback();
        } else {
            let setConfigReq = new protos.Request;
            setConfigReq.setSetConfig(toConfigProto(options));
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

/**
 * Default name of the blockfish executable.
 * @constant
 * @type {string}
 */
AI.DEFAULT_BLOCKFISH_PATH = 'blockfish';

/**
 * Initialization event, fired once after the {@link AI} is created.
 *
 * @event AI#init
 */

/**
 * Fired if an error occurs with the blockfish process.
 *
 * @event AI#error
 * @type {Error}
 */

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

function setAnalyzeProto(proto, options) {
    if (options.suggestionLimit !== undefined) {
        proto.setMaxResults(options.suggestionLimit);
    }
}

function toConfigProto(options) {
    let cfg = new protos.Request.Config;
    if (options.nodeLimit !== undefined) {
        cfg.setNodeLimit(options.nodeLimit);
    }
    return cfg;
}

function toSnapshotProto(arg) {
    let ss = new protos.Snapshot;
    ss.setHold(arg.hold || "");
    ss.setQueue(arg.queue);
    ss.setRowsList(arg.matrix);
    return ss;
}

function fromAnalysisProto(arg) {
    return {
        suggestions: arg.getSuggestionsList().map(fromSuggestionProto),
        statistics: fromStatsProto(arg.getStats()),
    };
}

function fromStatsProto(arg) {
    /**
     * Statistics on an analysis.
     *
     * @typedef {Object} AI~Statistics
     * @property {number} nodes - Total number of nodes discovered.
     * @property {number} iterations - Number of iterations (leaf nodes).
     * @property {number} timeTaken - Time taken, in seconds.
     */
    return {
        nodes: arg.getNodes(),
        iterations: arg.getIterations(),
        timeTaken: arg.getTimeTakenMillis() / 1000,
    };
}

function fromSuggestionProto(arg) {
    /**
     * A suggestion from an analysis.
     *
     * @typedef {Object} AI~Suggestion
     * @property {number} rating - Suggestion's rating. Lower is better.
     * @property {string[]} inputs - List of inputs to press. Each string is one of
     * 'left', 'right', 'cw', 'ccw', 'hold', 'sd', or 'hd'.
     */
    return {
        rating: arg.getRating(),
        inputs: arg.getInputsList().map(fromInputProto),
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
