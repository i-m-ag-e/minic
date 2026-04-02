# minic: A C Compiler in Rust

`minic` is a subset-C compiler written in Rust. It parses C source code, performs semantic analysis, and generates AT&T syntax assembly for the x64 architecture. 

## Features & Supported C Subset

The compiler currently supports the following language features:
* **Data Types:** 64-bit `int` variables (all integers are currently mapped to 64-bit to simplify architectural constraints), variable declarations, and assignments.
* **Control Flow:** `if-else` statements, `for`, `while`, and `do-while` loops, `break`, `continue`, and `switch` statements.
* **Operators:** All standard binary, unary, and ternary operators, including short-circuiting conditional operators.
* **Functions:** Declarations and definitions supporting any number of 64-bit integer arguments and recursive calls.
* **Semantic Analysis:** Basic type checking and scope resolution to distinguish between functions and integers.

## Architecture & Code Generation

`minic` handles the lexical analysis, parsing, and assembly generation phases. It relies on `gcc` for preprocessing, assembling the generated `.s` files, and performing all linking (including linking with GNU libc).

* Generates x64 assembly in AT&T syntax.
* Automatically includes detailed, line-by-line assembly comments for debugging and educational purposes (can be disabled via `--no-comments`).
* Multiple `.c` files can be compiled and linked into a single executable.

## Installation & Usage

Clone the repository and build using `cargo`:

```shell
git clone https://github.com/i-m-ag-e/minic.git
cd minic
cargo build --release
```

**Compilation Examples:**

```shell
# Compile to an executable
cargo run -- files/hello.c -o hello 

# Compile to an object file
cargo run -- files/hello.o -c -o hello.o 

# Compile and link multiple files
cargo run -- files/bye.c files/hello.c -o combined 
```

*Note on linking and libc integration: `minic` relies on GCC for all linking. Because GCC automatically links the generated object files with GNU libc, standard C functions (e.g., `putchar`) can be utilized provided they are declared with the correct signature. `minic` passes the full 64-bit integer via standard registers (e.g., `%rdi`, `%rax`), but standard libc functions expecting a 32-bit `int` will only read the lower 32 bits (e.g., `%edi`, `%eax`).*

## Testing & Roadmap

The compiler utilizes a snapshot testing strategy to verify abstract syntax tree (AST) generation and assembly output consistency against known source files. 

Run the current test suite via:
```shell
cargo test
```

**Current Status:** The test suite is currently in active development and is not yet comprehensive. Expanding test coverage across edge cases and complex control flow is the primary focus of ongoing work.
