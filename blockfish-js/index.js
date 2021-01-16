const { spawn } = require('child_process');
const EventEmitter = require('events');
const jspb = require('google-protobuf');
const protos = require('./generated/blockfish_pb.js');

class AI extends EventEmitter {
    constructor(subproc) {
        super();
        Object.assign(this, {
            stdin: subproc.stdin,
            stdout: subproc.stdout,
            messageLength: null,
        });
        this.stdout.on('data', this.onRawData.bind(this));
    }

    onRawData(buf) {
        let pos = 0;
        for (;;) {
            if (this.messageLength === null) {
                if (!anyVarints(buf, pos)) {
                    break;
                }
                let reader = new jspb.BinaryReader(buf, pos);
                this.messageLength = reader.decoder_.readUnsignedVarint32();
                pos = reader.getCursor();
            } else {
                if (buf.length - pos < this.messageLength) {
                    break;
                }
                let reader = new jspb.BinaryReader(buf, pos, this.messageLength);
                let resp = new protos.Response;
                protos.Response.deserializeBinaryFromReader(resp, reader);
                this.onResponse(resp);
                this.messageLength = null;
                pos = reader.getCursor();
            }
        }
        this.stdout.unshift(buf.slice(pos));
    }

    onResponse(resp) {
        console.log(resp.toObject());
        setTimeout(() => {
            let req = new protos.Request();
            let ss = new protos.Snapshot();
            ss.setQueue("LOJISZ");
            ss.addRows("XXXXXXXX X");
            ss.addRows("X XXXXXXXX");
            let ana = new protos.Request.Analyze();
            ana.setId(123);
            ana.setSnapshot(ss);
            ana.setMaxResults(1);
            req.setAnalyze(ana);
            this.sendRequest(req);
        }, 1000);
    }

    sendRequest(req) {
        const chunk = req.serializeBinary();
        const lengthWriter = new jspb.BinaryWriter;
        lengthWriter.encoder_.writeUnsignedVarint32(chunk.length);
        const lengthChunk = lengthWriter.getResultBuffer();
        this.stdin.write(lengthChunk);
        this.stdin.write(chunk);
    }
}

function anyVarints(buf, pos) {
    for (let i = pos; i < buf.length; i++) {
        if (!(buf[i] >>> 7)) {
            return true;
        }
    }
    return false;
}

function blockfish() {
    return new AI(spawn('blockfish', [], { stdio: ['pipe', 'pipe', 'inherit'] }));
}

blockfish();
