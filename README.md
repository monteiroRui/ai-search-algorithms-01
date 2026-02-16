# Peg Solitaire Solver — AI Search Algorithms

An AI-based solver for the classic **Peg Solitaire** board game, implementing and comparing multiple search algorithms with different heuristic strategies.

Built as part of the Artificial Intelligence course (2025/2026) using **Common Lisp**.

## Algorithms Implemented

- **BFS** — Breadth-First Search (optimal, high memory usage)
- **DFS** — Depth-Limited Search (fast, no optimality guarantee)
- **A\*** — Best-first search with heuristic evaluation
- **IDA\*** — Iterative Deepening A* (memory-efficient alternative to A*)

## Heuristics

| Heuristic | Formula | Admissible | Notes |
|-----------|---------|------------|-------|
| h1 | `1 / (mobile pieces + 1)` | No | Favors mobility |
| h2 | `pieces - 1` | Yes | Estimates minimum moves remaining |

## Key Results (Problem 5 — depth 10 solution)

| Algorithm | Nodes Generated | Time (s) | Optimal |
|-----------|----------------|----------|---------|
| BFS | 1696 | 0.078 | Yes |
| DFS (limit=10) | 445 | 0.031 | No |
| A* (h2) | 1038 | **0.030** | Yes |
| IDA* (h2) | 5081 | 0.047 | Yes |

**A\* with h2** achieves the best balance of speed and optimality. **IDA\* with h2** is the best choice under memory constraints.

## Project Structure

```
├── projeto.lisp    # User interface, menus, file I/O
├── procura.lisp    # Generic search algorithms (BFS, DFS, A*, IDA*)
├── puzzle.lisp     # Game domain: board representation, operators, heuristics
└── docs/           # Full technical manual
```

## How to Run

1. Load the files in a Common Lisp environment (e.g., LispWorks, SBCL):
   ```lisp
   (load "puzzle.lisp")
   (load "procura.lisp")
   (load "projeto.lisp")
   ```
2. Start the program:
   ```lisp
   (start)
   ```
3. Follow the menu to select a problem and algorithm.

## Authors

- Dinis Xavier
- Rui Monteiro
- Daniel Pais

> For a detailed breakdown of the architecture, heuristic analysis, and performance benchmarks, see the [Technical Manual](https://github.com/monteiroRui/ai-search-algorithms-01/blob/main/Manuals/TechnicalManual/TechnicalManual.md).
