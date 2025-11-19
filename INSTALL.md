# Installation Guide

The project is an OCaml dune workspace with a GUI written on top of the
[Bogue](https://github.com/sanette/bogue) toolkit. The steps below install the
compiler toolchain, pull every OCaml dependency (including Bogue), and explain
how to build/run the executables. If you use macOS and don't have Homebrew installed, please install it now via the instructions on https://brew.sh/. 

## 1. Prerequisites

1. **OPAM 2.1+** – the OCaml package manager (macOS via `brew install opam`,
   Linux via distribution packages).
2. **Dune 3.20+** – installed automatically by OPAM, but listed explicitly.
3. **SDL2 native libraries** – needed for Bogue. Install once per machine:
   - macOS: `brew install sdl2 sdl2_image sdl2_ttf sdl2_mixer`
   - Ubuntu/Debian: `sudo apt-get install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev`

## 2. Install OCaml dependencies (including Bogue)

`opam install . --deps-only` will pull everything listed by dune. But also run the following commands.

Install Bogue (the GUI package):
```bash
opam install bogue
```

Also make sure you have ounit2 installed:
```bash
opam install ounit2
```

This command downloads the Bogue bindings and builds them against the SDL2 libs
installed in step 1.

## 3. Build the program

```bash
dune build
```

## 4. Run the executables

```bash
dune exec bin/gui.exe   # launches the Bogue GUI
```
