const protos = require('./generated/blockfish_pb.js');
const IPC = require('./src/ipc.js');
const AI = require('./src/ai.js');

module.exports = {
    protos,
    IPC,
    AI,
};

let ai = new AI;
ai.on('init', motd => {
    console.log("blockfish version: " + ai.version);
});

let ss = {
    queue: "LOJI",
    matrix: "XXXXX_XXXXX\nX_XXXXXXXX",
};
let cfg = {
    node_limit: 30000,
};
ai.analyze(ss, cfg, result => {
    console.log('result:');
    console.log(result);
    ai.kill();
});
