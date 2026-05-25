# Principles

Use these principles when a task needs design judgment beyond the core workflow.

## 1. Harness Beats Prompt Size

A prompt asks. A harness makes the desired behavior easy to do, observable, and
checkable. If an agent repeats a mistake, add a tool, check, state variable, or
documentation map before adding more prose.

Example:
- Weak: "Be careful not to overwrite files."
- Strong: `write_file` refuses existing files until `read_file` or
  `lines_range` has seen them.

## 2. Maps Beat Manuals

Keep always-loaded instructions short. A good `AGENTS.md` points to authoritative
repo-local docs. It does not duplicate them.

Good map:

```text
For architecture, read docs/ARCHITECTURE.md.
For tests, read docs/TESTING.md.
For release notes, use skills/changelog/SKILL.md.
For UI rules, read docs/FRONTEND.md.
```

## 3. One Source Of Truth Beats Mirrored Docs

Prefer one canonical knowledge source when the project already has one. Do not
create parallel docs that restate the same rules unless the repo also has an
enforcement or generation path that keeps them synchronized.

Valid shapes:

- A short `AGENTS.md` points to `docs/agent-index.md`, which links to focused
  docs.
- A large `README.org` is the canonical manual, and agents use a structure-first
  query tool such as `oq` to inspect headings and targeted sections.
- Generated schema or API docs are rebuilt from source definitions in CI.

Invalid shape:

```text
README.org says one thing.
docs/ARCHITECTURE.md copies part of it by hand.
AGENTS.md copies both.
```

That creates drift. Add an index, query tool, or generated view instead.

## 4. Progressive Disclosure Is a Runtime Contract

The agent should load details at the moment of need:

1. Metadata or index in default context.
2. Relevant guide for the current phase.
3. Exact source file, section, line range, or test output.

Avoid loading "just in case" context.

## 5. Tool Schemas Are Context

Every tool schema competes with task context. Small models especially benefit
from phase-specific tool sets.

Examples:
- Explore phase: read, grep, tree, lines.
- Edit phase: read, edit, write, test.
- Review phase: diff, tests, lint, static checks.
- Operations phase: logs, metrics, traces, rollback.

## 6. Make Failure Actionable

Tool failure output should include:

- what failed,
- why it failed,
- smallest useful evidence,
- exact next action.

Example:

```text
No replacement made: oldcontent was not found in src/api.ts.
Use lines_range(file_name="src/api.ts", from=120, to=170) and retry with exact
text from the file, including real newline characters.
```

## 7. Store State Outside the Model

Do not rely on the model remembering:

- active plan and current step,
- files read before write,
- known file hashes or diffs,
- cached read-only tool results,
- failed tools and repeated calls,
- evidence from prior attempts.

Put these in session state, repo artifacts, or durable memory.

## 8. Validation Is the Work Loop

The harness should expose validation directly:

- tests and linters,
- build/startup checks,
- browser or UI workflows,
- log and metric queries,
- security and DLP checks,
- structural architecture rules.

Write check failures as instructions the agent can use.

## 9. Safety Should Be Low-Friction And High-Signal

Autonomous agents should not ask for confirmation on every ordinary action.
That trains blind approval. Instead:

- allow clean reads and scoped writes in a sandbox,
- block high-confidence destructive actions,
- require typed confirmation for ambiguous irreversible actions,
- redact or block secrets,
- classify MCP tools by stable identity and risk.

## 10. Reviews Should Become Rules

If the same human review comment appears twice, encode it:

- as a doc in the system of record,
- as a lint or structural test,
- as a tool failure hint,
- as a skill or blueprint.

## 11. Garbage Collection Is Continuous

Agent-generated systems drift by copying local patterns. Schedule small cleanup
passes: stale docs, oversized files, duplicated helpers, missing tests, weak
errors, and architectural boundary violations.
