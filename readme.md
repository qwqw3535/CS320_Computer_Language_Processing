# EPFL CS320 Amy Compiler - group 13

---

This project is a compiler for Amy language.
A final report of the project is [here](https://gitlab.epfl.ch/lara/student-repos-cs320-2022/cs320-group13/-/blob/clplab6/report/5.6%20Multiple%20Syntax%20Styles.pdf).
A presentation slide of the project is [here](https://gitlab.epfl.ch/lara/student-repos-cs320-2022/cs320-group13/-/blob/clplab6/slides/Amy_extension_multiple_syntax.pptx).

## Getting Started

---

### Prerequisites

- This project needs Scala 3. You can download it [here](https://www.scala-lang.org/download/)
- This project needs Node.js version 12 or later. For checking your computer's Node.js version, put `nodejs --version` in your terminal.
- This project also needs `wat2wasm`. Install the package with your system's package manager like `apt install wabt`
- After installing `wat2wasm`, make sure that `wat2wasm` is either in a system path or in the \<root of the project>/bin folder (that you may have to create).

### How to execute Amy program

1. Clone this repository to your computer.
2. Open a terminal, change current address to this folder.
3. Type `sbt`
4. To generate WebAssembly code, type `run <your dependencies> <your Amy program name.amy>`
5. To execute the result JavaScript code, type `node wasmout/<your Amy program name.js>`

## example

- Our own made examples to test our extension `5.6 multiple syntax` is in the folder `/extension-examples`. Just run as other examples.

---

## Contributors

This project was made for EPFL CS-320 class by Donggyu Lee `donggyu.lee@epfl.ch` and Jisu Seo `jisu.seo@epfl.ch`
