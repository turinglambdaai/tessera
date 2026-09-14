# Contributing to Tessera

Thanks for helping. The bar for a contribution is simple:

1. **It must run.** A widget or feature ships with a runnable example or
   test that exercises it.
2. **It must be verified.** Layout or drawing changes update a snapshot
   PNG; logic changes add `rackunit` checks.
3. **It must be documented.** A row in `AGENTS.md`'s intent table and, for
   public API, a section in the manual.

## Development setup

```bash
git clone https://github.com/turinglambdaai/tessera.git
cd tessera
raco pkg install --name tessera --link .
raco test test/
```

`raco test test/` runs the full suite: unit tests plus smoke tests that
open hidden windows and write snapshot PNGs into `test/snapshots/`.

## Code conventions

- `#lang racket/base` everywhere; add explicit `require`s.
- Module doc comments explain *why*, especially platform quirks — several
  are load-bearing (see the notes on the legacy GL pipeline in
  `render.rkt` and `platform.rkt`).
- Public API goes through `main.rkt`; internal modules may change.
- Tests live in `test/`; every bug fix adds a regression test.

## Adding a widget

1. Add the constructor to `view.rkt` (props hash, defaults).
2. Add measure + draw cases to `layout.rkt`.
3. Add a row to the intent table in `AGENTS.md`.
4. Add a snapshot test: render it, look at the PNG, keep the assertion.

## Reporting bugs

Include: OS + version, Racket version, a minimal runnable example, and a
screenshot if it renders at all. GPU driver details help for rendering
bugs.
