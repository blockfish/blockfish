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
    matrix: "XXXXX XXXXX\nX XXXXXXXX",
};
ai.analyze(ss, result => {
    console.log('result:');
    console.log(result);
});
