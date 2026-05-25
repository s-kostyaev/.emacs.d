---
name: agent-harness-engineering
description: Build, audit, or improve agentic coding/research systems and their harnesses. Use when designing agent workflows, small-model scaffolds, tool schemas, progressive-disclosure context, repository knowledge maps, guardrails, validation loops, subagents, evals, recovery mechanisms, or autonomous-agent safety policies.
---

# Agent Harness Engineering

Use this skill to design the environment around an agent: context surfaces,
tools, guardrails, validation, recovery, and feedback loops. The default move is
not "write a bigger prompt"; it is "make the right action legible and
mechanically checkable."

## Workflow

1. Define the job and risk.
   - Goal: coding, research, operations, UI validation, data work, review, or
     cleanup.
   - Model class: small/local, frontier, reasoning, tool-weak, or no-tool.
   - Autonomy: advisory, edit-with-confirmation, autonomous in sandbox, or PR
     producer.
   - Failure cost: low, medium, high, irreversible, or data-exfiltration risk.

2. Build a context map before loading details.
   - Keep the always-loaded entry point short: a table of contents, not a
     manual.
   - Point to repo-local sources of truth: `README.org`, `docs/`, `AGENTS.md`,
     skills, blueprints, eval fixtures, generated schemas, and runbooks.
   - Preserve an existing canonical source of truth. Do not create mirrored docs
     unless there is an enforcement/update path.
   - Load details only when the current step needs them.

3. Shape the tool surface.
   - Expose the fewest tools that can complete the current step.
   - Prefer deterministic tools and structured outputs.
   - Use compound tools when a small model repeatedly needs the same chain
     (`search_and_read`, `read_and_edit`, `write_and_test`).
   - Add repair hints to tool failures: why it failed, what to try next, and an
     exact minimal example.

4. Add mechanical feedback loops.
   - Run tests, linters, type checks, UI probes, log queries, or metrics checks
     as tools or hooks.
   - Treat failures as prompt surface: concise, actionable, and scoped.
   - Encode recurring review comments as docs, lints, checks, or skills.

5. Add safety and state controls.
   - Use read-before-write, sandboxed filesystem policy, DLP, irreversible
     action checks, and confirmation only for high-signal cases.
   - Keep session-local state for plans, files read, cached pure tool results,
     evidence, and model/tool failures.
   - For multi-file edits, prefer checkpoints or rollbackable patches.

6. Evaluate and iterate.
   - Add a small eval before claiming a harness improvement works.
   - Compare profiles such as baseline, limited-tools, compound-tools,
     strict-write, selective-skills, and quality-monitor.
   - Track pass rate, tool-call count, recovery count, validation failures, and
     context size.

## Small-Model Biases

For small or local models, optimize for fewer choices and shorter turns:

- Inject one relevant instruction block, not all instructions.
- Show fewer tool schemas; route by task phase when possible.
- Keep the active plan visible and current.
- Prefer exact file ranges and diffs over full files.
- Cache repeated read/search results.
- Detect loops: repeated tool call, repeated edit miss, empty answer, malformed
  tool args, or repeated shell failure.
- On repair turns, give the model the failing command, the smallest relevant
  output tail, and the next action.

## Repository Harness Surfaces

Use this minimal checklist when auditing a project:

- Entry map: short `AGENTS.md` or agent index with links to deeper docs.
- Knowledge: one canonical source or well-indexed docs; large Org sources
  should have a structure-first query tool such as `oq`.
- Tools: read/search/edit/shell split by role; MCP tools classified by risk.
- Validation: fast test command, lint/type command, formatting command.
- Guardrails: sandbox, secret handling, read-before-write, destructive checks.
- Feedback: evals, traces, review agents, quality sweeps, stale-doc cleanup.
- Legibility: logs, UI/browser state, screenshots, metrics, traces, schemas.

Run the bundled audit for a quick first pass:

```bash
python3 <skill-dir>/scripts/harness_audit.py /path/to/repo
```

## Progressive References

Read only the reference needed for the current task:

- `references/principles.md`: principles and tradeoffs for agent harness design.
- `references/patterns.md`: reusable implementation patterns and when to use
  them.
- `references/examples.md`: concrete examples for coding agents, research
  agents, UI agents, and small-model profiles.

## Output Shape

When designing or auditing a harness, return:

```text
Goal:
Model and autonomy assumptions:
Current harness inventory:
Recommended changes:
1. Highest-leverage change
2. Next change
3. Later change
Validation plan:
Risks and guardrails:
```

Keep recommendations mechanical. Prefer "add this check/tool/state machine" over
"tell the model to be careful."
