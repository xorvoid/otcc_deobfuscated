#!/bin/sh
set -e

## Only tested on alpine-standard-3.17.3-x86 in qemu-system-i386
## Try to at least do basic sanity though

MACHINE=$(uname -m)
if [ "$MACHINE" != "i686" ]; then
    echo "ERROR: Not running on an intel 32-bit linux distro, cannot compile!"
    exit 1
fi

gcc -shared -fPIC -o malloc_shim.so malloc_shim.c
gcc -std=c89 -m32 -g -Wl,-z,execstack -no-pie -Wno-int-to-pointer-cast -Wno-int-conversion -Wno-builtin-declaration-mismatch -o otcc otcc.c
gcc -std=c89 -m32 -g -Wl,-z,execstack -no-pie -Wno-int-to-pointer-cast -Wno-int-conversion -Wno-builtin-declaration-mismatch -o otcc_commented otcc_commented.c
