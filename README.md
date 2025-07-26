# Whisper
[![CI Status](https://github.com/WhyNeet/Whisper/actions/workflows/test.yml/badge.svg)](https://github.com/WhyNeet/Whisper/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
> A functional, statically typed language that transpiles to JavaScript - inspired by Rust's syntax.

**Whisper** is a modern programming language that brings functional programming, type safety, and expressive syntax to the JavaScript ecosystem. It compiles directly to minified, unreadable JavaScript, making it perfect for frontend, backend, or fullstack development — all while enforcing purity, immutability, and correctness by design (most of the time).

```whisper
// Effectful function (performs I/O)
fn{io} println(s: string) = {
  namespace console {
    // JavaScript interop
    extern fn{io} log(args: string);
  }

  console.log(s);
}

@main fn{io} main() = println("Hello, world!");
```

## ✨ Features

- ✅ Functional First: Pure functions, flexible expressions.
- 🛡️ Statically Typed: Catch errors at compile time with type inference.
- 🧼 Pure by Default: Only effect functions can perform side effects.
- 🔧 Rust-like Syntax: Familiar to Rust developers — clean, expressive, and safe. No borrow checker.
- ⚡ Transpiles to JavaScript: Runs anywhere JS does — browsers, Node.js, Deno, etc.
- 📦 Zero Runtime: Generated code has no Whisper-specific runtime dependencies.
- 🧪 Pattern Matching, Immutability, Algebraic Types — all the FP goodness (probably in the future).

## 🚀 Getting Started

1. Install the compiler
For now, build from source:
```bash
git clone https://github.com/WhyNeet/Whisper.git
cd Whisper
cargo build --release
./target/release/wrc
```
2. Write your first program
```whisper
// hello.wr
@main fn{io} main() = println("Hello from Whisper!");
```
3. Compile to JavaScript
```bash
wrc hello.wr
node hello.js
# Output: Hello from Whisper!
```
## 📜 License
MIT - see [LICENSE](https://github.com/WhyNeet/Whisper/blob/main/LICENSE) for details.
