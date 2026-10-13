# Nand2Tetris Toolchain in OCaml

A complete software toolchain for the **Hack** computer, built in OCaml as part of **CS2111 - Foundations of Computing Systems**, following the **Nand2Tetris** course (projects 6 to 11).

It takes a program written in the high level **Jack** language all the way down to **Hack** binary machine code:

```
Foo.jack ──► Jack Compiler ──► Foo.vm ──► VM Translator ──► Foo.asm ──► Assembler ──► Foo.hack
```

One command runs the whole pipeline, and each stage can also be used on its own.

---

## Table of Contents

- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Limitations](#limitations)
- [Acknowledgements](#acknowledgements)

---

## Features

### Hack Assembler (project 6)

Translates Hack assembly (`.asm`) into Hack binary machine code (`.hack`).

- Two pass assembly: the first pass records labels in the symbol table, the second encodes every instruction.
- Full support for A-instructions, C-instructions, labels, predefined symbols (`R0`-`R15`, `SP`, `LCL`, `SCREEN`, `KBD`, ...) and variables, which are allocated from RAM address 16.
- Comments and whitespace are stripped before parsing.
- The output is written next to the input (`Max.asm` gives `Max.hack`).

### VM Translator (projects 7 and 8)

Translates Jack VM code (`.vm`) into Hack assembly (`.asm`), then runs the assembler to produce the `.hack` file.

- All arithmetic and logical commands: `add`, `sub`, `neg`, `eq`, `gt`, `lt`, `and`, `or`, `not`.
- All memory segments: `constant`, `local`, `argument`, `this`, `that`, `pointer`, `temp` and `static` (statics are named per file, so `static 3` in `Foo.vm` becomes `Foo.3`).
- Program flow: `label`, `goto`, `if-goto`, with unique labels for every comparison.
- Function calls: `function`, `call`, `return`, with the full stack frame of the standard calling convention.
- Accepts a single `.vm` file or a directory of `.vm` files, translated into one program.
- Adds the bootstrap code (`SP = 256`, `call Sys.init 0`) whenever the program defines `Sys.init`.

### Jack Compiler (projects 10 and 11)

Compiles Jack classes (`.jack`) into VM code (`.vm`).

- **Tokenizer:** reads the source character by character into keywords, symbols, integer and string constants and identifiers, skipping `//`, `/* */` and `/** */` comments, including comments that span lines.
- **Parser:** a recursive descent parser with one function per grammar rule, producing a typed AST of the class.
- **Syntax analyzer (project 10):** with the `-xml` flag, writes the token stream (`XxxT.xml`) and the parse tree (`Xxx.xml`) in the official course format.
- **Code generator (project 11):** builds class and subroutine symbol tables and generates VM code for:
  - static, field, argument and local variables
  - constructors (memory allocation with `Memory.alloc`), methods (with `this` bound to argument 0) and functions
  - method calls on objects and on the current object, and function calls on classes
  - arrays, for both reading `a[i]` and assigning `let a[i] = e`
  - string constants, built with `String.new` and `String.appendChar`
  - `if`/`else` and `while`, with unique labels inside each subroutine
- **Full pipeline:** after writing the `.vm` files, the compiler runs the VM translator, which runs the assembler, so one command produces `.vm`, `.asm` and `.hack`.
- Clear error messages that name the file and the problem, for example `Bad.jack: parse error: expected a term but got ';'`.

---

## Requirements

| Requirement | Version |
|---|---|
| OCaml | 4.14 or newer (the code uses the `In_channel` and `Out_channel` modules) |
| dune | 3.16 or newer |
| opam | 2.x (recommended, to install OCaml and dune) |

There are no other library dependencies; only the OCaml standard library is used. The same list is in [requirements.txt](requirements.txt).

To run the generated files, the **Nand2Tetris software suite** (CPU emulator and VM emulator), from  the online IDE of [nand2tetris.org](https://www.nand2tetris.org/software) is needed.

---

## Installation

1. Install OCaml and dune with opam:

   ```bash
   opam init
   opam switch create 4.14.2
   eval $(opam env)
   opam install dune
   ```

2. Clone the repository and check if the link to assembler exist in jackvm directory and link to jackvm exist in jackcompiler directory otherwise run these commands.

   ```bash
   cd jackvm       && ln -s ../assembler assembler && cd ..
   cd jackcompiler && ln -s ../jackvm jackvm       && cd ..
   ```

   They let the VM translator use the assembler and the compiler use the VM translator.


3. Build each tool from its own directory:

   ```bash
   cd assembler    && dune build && cd ..
   cd jackvm       && dune build && cd ..
   cd jackcompiler && dune build && cd ..
   ```

---

## Usage

Each tool must be run from **its own directory**, because each one starts the next stage with `dune exec`. Outputs are always written next to the input.

### Compile a Jack program (full pipeline)

```bash
cd jackcompiler
dune exec jackcompiler path/to/Foo.jack     # Foo.vm, Foo.asm, Foo.hack
dune exec jackcompiler path/to/Program      # one .vm per .jack, plus Program/Program.asm and Program/Program.hack
```

### Generate the parse tree XML (project 10)

```bash
cd jackcompiler
dune exec jackcompiler -xml path/to/Program
```

The XML is written to an `out/` directory next to the input (`out/FooT.xml` and `out/Foo.xml`), so the official compare files are never overwritten.

### Translate VM code

```bash
cd jackvm
dune exec jackvm path/to/Foo.vm       # Foo.asm and Foo.hack
dune exec jackvm path/to/Program      # Program/Program.asm and Program/Program.hack
```

### Assemble Hack assembly

```bash
cd assembler
dune exec assembler path/to/Foo.asm   # Foo.hack
```

### Run the output

- Load `.vm` files (or the directory) in the **VM emulator**.
- Load `.asm` or `.hack` files in the **CPU emulator**.

---

## Limitations

- Errors are basic: the first error is reported with the file name, and the compiler does no type checking.
- Each tool has to be run from its own directory, since the next stage is started with `dune exec`.
- The projects are connected through symbolic links, which must be created once after cloning.
- The Jack OS is not included; it comes from the official software suite.
- Other limitations which may not be known yet.

---

## Acknowledgements

- [Nand2Tetris](https://www.nand2tetris.org/) by Noam Nisan and Shimon Schocken, for the course, the Hack platform, the specifications and the test programs.
- **CS2111 - Foundations of Computing Systems**, for which this project was developed.

