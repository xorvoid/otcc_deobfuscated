# OTCC Deobfuscated

The Obfuscated Tiny C Compiler: Deobfuscated and Explained

## Building

A 32-bit i686 or greater machine running Linux (and possibly BSD) is required. I tested on `qemu-system-i386`.

Command: `./build.sh`

This will build:

- `otcc.c => otcc`
- `otcc_commented.c => otcc_commented`

## Running

`./otcc otccex.c 5`

`./otcc_commented otccex.c 5`

## Issues

OTCC assumes all heap memory is <0x80000000 but this is not a gaurentee on modern x86-32 distros. In testing, I used Alpine Linux 3.17.3 which does a lot of hardening for security reasons. This makes it painful for getting OTCC working:

- Musl calls `mmap()` were possible, returning addresses >=0x80000000
- Sbrk() is rejected for non-zero increments
- `LD_PRELOAD` was attempted but loaded shared-libs were being mapped to high-memory also

I ended up using a shim `malloc` implementation linked into OTCC. This works fine for the stage1 compiler, but the problems arise again for the stage2 compile-the-compiler-with-the-compiler phase. I didn't bother to solve this problem, so the binaries are not able to compile themselves on many systems.

Better results can likely be obtained via either a different distro or by using a machine and distro from the same era as this code. But, I'm not interested enough to do that work.

