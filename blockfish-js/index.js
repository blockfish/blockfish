const protos = require('./generated/blockfish_pb.js');
const IPC = require('./src/ipc.js');

module.exports = {
    protos,
    IPC,
};

let ipc = IPC.childProcess('blockfish', []);
ipc.on('recv', resp => {
    console.log(resp.toObject());
    ipc.kill();
});
