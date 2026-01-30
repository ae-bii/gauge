gauge — a tiny OCaml static complexity estimator

What it is

Gauge is a prototype static analyzer that inspects OCaml source and produces conservative
asymptotic complexity estimates for top-level code and functions.

Key components

- Frontend: ppxlib-based parsing of OCaml source (uses the same Parsetree types).
- Inference: a small AST walker with a cost algebra (polynomial degree + log-power).
- Contracts: simple inline annotations in comments `(* @complexity O(n) *)`.
- Reporter: compares declared vs inferred complexities and prints OK/MISMATCH.

Cost model

- Represented as `type cost = { degree : int; log : int }`.
- Constants: `o1`, `on`, `on2`, `olog`, `ounk`.
- Operations: `max_cost`, `mul_cost`, `seq_cost`.

Inference strategy

- Walk the AST and combine child costs with `seq_cost` (max).
- Treat loops and `List.*` traversals as multiplicative by `O(n)`.
- Detect `let rec` groups (top-level and local) and conservatively map their names to `O(n)` when they may be called recursively.
- Propagate callee costs to callers via a simple fixed-point iteration across top-level functions.
- Heuristics: AST-based collection of local `let rec` names, with a printed-expression fallback when needed.

Why conservative

Gauge prefers sound, conservative over-approximation. Where the analyzer cannot be sure, it reports a higher cost.

How to build & run tests

Requirements: dune, OCaml toolchain (same version used by the project).

Build and run all expect tests:

```bash
dune build
dune runtest -p gauge
```

Developer notes

- The main inference logic is in `lib/infer.ml`.
- Tests are under `test/` and use `ppx_expect` style expect tests.
- To add more patterns, update `expr_cost_with_env_with` and the local-rec name collection.

Next steps / Improvements

- Implement a basic name-resolution pass to resolve call-sites to their bindings (will improve precision).
- Handle higher-order patterns and known library traversals more precisely.
- Add more tests (tail recursion, higher-order use, nested modules).

License: MIT
