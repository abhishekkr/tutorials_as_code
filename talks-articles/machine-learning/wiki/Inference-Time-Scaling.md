
> concept wiki

## Inference Time Scaling

> [arxiv paper](https://arxiv.org/html/2408.00724v2)
> To improve upon the accuracy, though it wouldn't be exponential enough to substitue higher parameter models.

* Instruct or tune a Model to generate more tokens before giving a final result (based on reasoning generated before). Often a longer process.

**There are difference approached to it, like..**

* ITS - Chain-of-Thought: As per discussions, OpenAI o1 model was trained on private `CoT` (a long, slow, resource heavy inference workflow).

* ITS - Best of N: Generate `n` results, for each result.. generate a critique. Based on `n1, n2, n3,..` and respective critiques, generate final result.

* ITS - MCTS (Monte Carlo Tree Search): MCTS to find different `n` results. Building a tree of generations, expanding nodes with best results from evaluator.

* ITS - Iterative Refinement: Using revision model to revise generated answer; until evaluation is satisfactory.

---
