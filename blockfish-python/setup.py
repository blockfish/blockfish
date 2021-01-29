#!/usr/bin/env python
import setuptools, subprocess

PROTOC = ['protoc', '--python_out=blockfish', '-I..', '../blockfish.proto']
subprocess.run(PROTOC, check = True)

setuptools.setup(
    name = 'blockfish',
    version = '0.1',
    description = 'Python interface to Blockfish AI',
    author = 'iitalics',
    url = 'https://github.com/iitalics/blockfish',
    packages = ['blockfish'],
)
