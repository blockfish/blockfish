import asyncio, subprocess
from google.protobuf.internal.decoder import _DecodeVarint32
from google.protobuf.internal.encoder import _EncodeVarint

import blockfish.blockfish_pb2 as protos


class IPC:

    DEFAULT_BLOCKFISH_PATH = 'blockfish'

    def __init__(self, process):
        self._sp = process

    async def send(self, *reqs):
        for req in reqs:
            write_request(self._sp.stdin, req)

    async def recv(self):
        return await read_response(self._sp.stdout)

    async def kill(self):
        if self._sp.returncode is None:
            self._sp.terminate()
            await self._sp.wait()


async def create_subprocess_ipc(program = IPC.DEFAULT_BLOCKFISH_PATH):
    sp = await asyncio.create_subprocess_exec(
        program = program,
        stdout = subprocess.PIPE,
        stdin = subprocess.PIPE,
    )
    return IPC(sp)


async def read_varint(stream):
    buf = bytearray()
    bs = b''
    while not any(x & 0x80 == 0 for x in bs):
        bs = await stream.read(n = 1)
        if len(bs) == 0:
            raise EOFError
        buf += bs
    varint, pos = _DecodeVarint32(buf, 0)
    assert(pos == len(buf))
    return varint


async def read_response(stream):
    length = await read_varint(stream)
    buf = await stream.read(length)
    if len(buf) < length:
        raise EOFError
    res = protos.Response()
    res.ParseFromString(buf)
    return res


def write_request(stream, req):
    data = req.SerializeToString()
    length_buf = bytearray()
    _EncodeVarint(length_buf.extend, len(data))
    stream.write(length_buf)
    stream.write(data)
