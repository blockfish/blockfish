const protos = require('./generated/blockfish_pb.js');
const IPC = require('./src/ipc.js');
const AI = require('./src/ai.js');

module.exports = { AI };

let ai = new AI;
ai.on('init', motd => {
    console.log("blockfish version: " + ai.version);
});

let ss = {
    queue: "LOJI",
    matrix: [
        'XXXXX_XXXXX',
        'X_XXXXXXXXX'
    ],
};

let cfg = {
    node_limit: 5000,
};

ai.analyze(ss, cfg, result => {
    console.log('result:');
    console.log(result);
    ai.kill();
});
