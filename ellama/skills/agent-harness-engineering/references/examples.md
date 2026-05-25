# Examples

Use these examples as templates. Adapt names and tools to the project.

## Small Local Coding Model

Problem: a 7B or 14B coder model repeats search calls and loses the plan.

Harness:
- Always-loaded context: short `AGENTS.md`, current task, active plan.
- Tools: route by phase; hide MCP and shell until needed.
- Context: `grep` then `lines_range`, not whole-file reads by default.
- State: cache pure tool calls; track read-before-write; track repeated edit
  misses.
- Repair: after two `oldcontent not found` failures, force `lines_range` around
  the target before another edit.
- Validation: run formatter and focused tests after each edit.

Prompt fragment:

```text
Use the current active plan. Work on the current step only. Prefer grep then
lines_range. If an edit fails, read the exact target range before retrying.
```

## Repository Knowledge System

Problem: a large repo has stale and duplicated instructions.

Harness:
- `AGENTS.md` is only a map.
- `docs/agent-index.md` lists task-to-doc routes.
- `docs/ARCHITECTURE.md`, `docs/TESTING.md`, `docs/QUALITY.md`, and
  `docs/SECURITY.md` are sources of truth.
- CI checks that docs referenced from `AGENTS.md` exist.
- A cleanup agent scans stale docs weekly and opens small PRs.

Bad:

```text
AGENTS.md contains 900 lines of every rule.
```

Good:

```text
AGENTS.md contains 80 lines and points to versioned docs.
```

## UI Agent

Problem: the agent changes frontend code but cannot verify user-visible
behavior.

Harness:
- Tool to start app per worktree.
- Browser tool or MCP with DOM snapshot, screenshot, console logs, and network
  events.
- Named journeys such as `signup`, `settings-save`, and `empty-state`.
- Before/after screenshot capture on bug fixes.
- Failing journey output includes route, action, observed DOM, console tail, and
  next action.

Validation loop:

```text
start app -> run journey -> observe failure -> patch -> restart -> rerun journey
```

## Research Agent

Problem: an agent produces plausible synthesis without stable evidence.

Harness:
- Search tools return bounded results with source URLs.
- Evidence store records source, claim, date, and confidence.
- Writer agent can only use evidence IDs, not raw memory.
- Final report includes citations.
- Reviewer agent checks unsupported claims.

Context rule:

```text
Load the source only when validating a claim. Keep evidence digests in the
active context, not full pages.
```

## Review Subagent

Problem: one coding pass misses risk and tests.

Harness:
- Coder subagent edits.
- Reviewer subagent receives diff, test output, and task.
- Reviewer reports findings first, then missing tests, then residual risk.
- Coder fixes only concrete findings.

Review prompt skeleton:

```text
Review this diff for bugs, regressions, missing tests, unsafe operations, and
architecture drift. Focus on actionable findings with file/line references.
```
