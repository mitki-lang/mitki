# Mitki Contribution Guide

This document explains how to contribute changes to the Mitki repository.

## Project Structure for OpenAI Codex Navigation

- `crates/` - workspace crates for the language and tools. Each subdirectory
  contains a Rust crate implementing part of the compiler or its tooling, such
  as the parser, analysis engine, or language server. Add new crates here when
  extending Mitki's functionality.
- `crates/mitki` - command line interface providing `run` and `lsp` subcommands.
- `crates/mitki-analysis` - semantic analysis including type inference and HIR.
- `crates/mitki-benchmark` - micro-benchmarks for parser and IDE features.
- `crates/mitki-db` - salsa database wiring all compiler components together.
- `crates/mitki-errors` - utilities for emitting diagnostics and errors.
- `crates/mitki-ide` - higher level APIs used by the LSP and tools.
- `crates/mitki-inputs` - representation of input files and line indexing.
- `crates/mitki-lsp-server` - Language Server Protocol implementation.
- `crates/mitki-parse` - grammar parser building syntax trees.
- `crates/mitki-span` - interned symbols used throughout the compiler.
- `crates/mitki-tokenizer` - lexical tokenizer producing syntax tokens.
- `crates/mitki-yellow` - green/red tree representation for syntax nodes.
- `target/` - build artifacts (not committed)
- `Cargo.toml` and `Cargo.lock` - workspace manifests
- `README.md` - overview of the project


## Required checks

Before committing, ensure the following commands succeed:

```bash
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
```

If formatting or linting fail because the tools are missing, install them with:

```bash
rustup component add rustfmt clippy
```

Commit code only after these checks pass.

## Pull request guidelines

- Provide a clear description of your changes.
- Reference relevant issues when applicable.
- Keep each pull request focused on a single topic.

