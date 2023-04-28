# basic-lang
## Compile
1. Install Rust
2. Run: `cargo build -r`
3. The executable is at: `./target/release/basic-lang.exe`
## Run the example:
`basic-lang.exe -i brainfuck.txt`

## JIT (Just In Time compilation)

JIT improvides a very large speed increase as it compiles on the fly to native machine code for your architecture.

To enable JIT, add the `--jit` flag to the command.

You can also add the `--verbose` flag to see what it's doing a little better :)

**Just In Time compiling currently only compiles a small subset of what the interpreter can run!**

It currently only supports:
- Function definitions
- Variable definitions & assignments
- Control flow (if/while/return)
- Calling user defined functions (no standard library)
- Math, unary, comparison, and boolean operations

A key limiting factor is the weakly typed nature of this language. Currently all data types are float64's behind the hood. The interpreter can handle dynamic data types, though.
