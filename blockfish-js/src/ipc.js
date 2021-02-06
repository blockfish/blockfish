const EventEmitter = require('events');
const childProcess = require('child_process');
const jspb = require('google-protobuf');
const protos = require('../generated/blockfish_pb.js');

class IPC extends EventEmitter {
    constructor(subprocess) {
        super();
        Object.assign(this, {
            subprocess,
            _buf: Buffer.alloc(0),
            _reader: new jspb.BinaryReader,
            _killed: false,
        });
        subprocess.stdout.on('readable', this._onRead.bind(this));
        subprocess.on('exit', this._onProcessExit.bind(this));
        subprocess.on('error', e => this.emit('error', e));
    }

    _onRead() {
        if (this._killed) {
            // not sure if this can happen but in case it can, just abort
            return;
        }

        // concatenate all incoming buffers
        let bufs = [this._buf];
        let buf;
        while ((buf = this.subprocess.stdout.read()) !== null) {
            bufs.push(buf);
        }
        buf = Buffer.concat(bufs);

        // parse all (complete) responses in the buffer
        let pos = 0;
        while (pos < buf.length) {
            let resp = new protos.Response;
            let end = parse(buf, pos, this._reader, resp);
            if (end === null) {
                break;
            }
            this.emit('recv', resp);
            pos = end;
        }
        this._buf = buf.slice(pos);
    }

    _onProcessExit(sig) {
        this._killed = true;
        if (typeof sig === 'number' && sig !== 0) {
            this.emit('error', new Error("non-zero exit code " + sig));
        } else {
            this.emit('exit');
        }
    }

    send(req, cb) {
        if (this._killed) {
            return;
        }
        const chunk = req.serializeBinary();
        const lengthWriter = new jspb.BinaryWriter;
        lengthWriter.encoder_.writeUnsignedVarint32(chunk.length);
        const lengthChunk = lengthWriter.getResultBuffer();
        this.subprocess.stdin.write(lengthChunk);
        this.subprocess.stdin.write(chunk, cb);
    }

    kill() {
        if (this._killed) {
            return;
        }
        this.subprocess.kill('SIGTERM');
        this._killed = true;
        this._reader.free();
    }
}

/**
 * Returns true iff a (complete) varint was found in `buf` at `pos`.
 */
function anyVarints(buf, pos) {
    while (pos < buf.length && buf[pos] & 0x80) {
        pos++;
    }
    return pos < buf.length;
}

/**
 * Parse a response from `buf` starting at position `pos`, deserializing into `resp`. If
 * the response data is incomplete, returns `null`, otherwise returns the next position to
 * read from.
 */
function parse(buf, pos, reader, resp) {
    if (!anyVarints(buf, pos)) {
        return null;
    }

    reader.setBlock(buf, pos);
    let len = reader.decoder_.readUnsignedVarint32();
    pos = reader.getCursor();
    if (pos + len > buf.length) {
        return null;
    }

    reader.setBlock(buf, pos, len);
    protos.Response.deserializeBinaryFromReader(resp, reader);
    pos = reader.getCursor();
    return pos;
}

IPC.childProcess = (executable, args) => new IPC(
    childProcess.spawn(
        executable,
        args,
        { stdio: ['pipe', 'pipe', 'inherit'] },
    )
);

module.exports = IPC;
