# OTCC Deobfuscated

The Obfuscated Tiny C Compiler: Deobfuscated and Explained

## Building

A 32-bit i686 or greater machine running Linux (and possibly BSD) is required. I tested on `qemu-system-i386`.

Command: `./build.sh`

This will build:

- `otcc_fix.c => otcc`
- `otcc_commented_fix.c => otcc_commented`

## Running

`./otcc otccex.c 5`

`./otcc_commented otccex.c 5`

## Self-compiling

`./otcc otcc_fix.c otccex.c 5`

`./otcc_commented otcc_commented_fix.c otccex.c 5`


## Fixes

The original OTCC assumes all heap memory and global vars are <0x80000000 but this is not a gaurentee on modern x86-32 distros. In testing, I used Alpine Linux 3.17.3 which does a lot of hardening for security reasons.

To resolve the issue, OTCC was patched to only compile small offsets in `(-512, 512)` as stack offsets

```
$ diff otcc.c otcc_fix.c
168c168
< s((e<512)<<7|5,n;
---
> s((e<512&-e<512)<<7|5,n;
```
