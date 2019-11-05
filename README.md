# Elm Project Survey

How do build times and asset sizes change as Elm projects grow larger?

Step one is to gather data, which is what this code is for.


## Generated Data

This code generates a local file called `summary.dat` which contains info on:

- OS
- RAM
- CPU
- Direct and indirect dependencies (with exact versions)
- Sizes of generated JS files
- From-scratch build times
- Per-file data:
    - Incremental build time
    - Number of bytes
    - Number of lines

It does not save any names though. No file names. No directory names. Etc.

The `summary.dat` file is just written to disk, so no one has access to this data unless you send it to them yourself.
