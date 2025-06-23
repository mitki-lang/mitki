## Style and Cleanup

* We encourage small cleanup PRs (e.g., renaming a local variable) for style nits; reviewers may flag only some nits and then send a follow-up cleanup PR.
* We continuously refactor code; don’t block PRs on style changes, but maintain “clean code” over time.

## Pull Request Boundaries

* Send small, focused PRs whenever possible; if an API change is needed, split it into a separate PR and minimize changed lines.
* Distinguish between internal component changes, API expansions, and new inter-crate dependencies when reviewing PRs.

## Dependencies and Performance

* Be conservative with crates.io dependencies; avoid small helper crates (except `itertools` and `either`);
* Avoid unnecessary allocations; prefer iterators over `Vec` when possible and push allocations to the call site.
* Use `rustc_hash::FxHashMap` and `FxHashSet` for collections.

## Commit and Changelog

* Use a rebase workflow; squash fixup commits with interactive rebase before merging.
* Write PR titles and commit messages from the user’s perspective; avoid `@`-mentioning people in commits.
* Mark PRs for release notes with `feat:`, `fix:`, `internal:`, or `minor:` prefixes or `changelog [feature|fix|internal|skip]` comments.

## Testing and Lints

* Write minimal, compact Rust code snippets in tests; use raw string literals without indentation for multiline fixtures.

* Avoid `#[should_panic]` and `#[ignore]`; explicitly test for `Err` or `None` and add `// FIXME` if behavior is wrong.

* If Clippy lints annoy, allow them in the `[workspace.lints.clippy]` section of `Cargo.toml`.

## API Design and Control Flow

* Express preconditions in types (e.g., accept `Walrus`, not `Option<Walrus>`); prefer early returns and push control flow to callers.
* Prefer standalone functions over “doer” objects for outward-facing APIs.
* For functions with many optional or boolean parameters, introduce a `Config` struct or split into separate functions.

## Type and Pattern Choices

* Prefer general types (`&[T]` over `&Vec<T>`, `&str` over `&String`).
* Prefer `Default` and `Vec::new()` over zero-arg `new` and `vec![]`.
* Use type ascription (e.g., `let v: Vec<T> = ...`) instead of turbofish when specifying collection types.

## Match and Control-Flow Ergnomics

* Use `match` instead of `if let ... else ...`.
* Avoid `ref` in patterns; use `=> (),` for empty match arms.
* Favor imperative control flow (`if`, `for`) over combinators when clarity is paramount.
