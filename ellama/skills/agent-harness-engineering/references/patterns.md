# Patterns

Use this catalog to select concrete harness mechanisms.

## Context Patterns

### Agent Index

Use when a repo has many docs or skills.

Shape:

```text
AGENTS.md
docs/agent-index.md
docs/ARCHITECTURE.md
docs/TESTING.md
docs/QUALITY.md
skills/<task>/SKILL.md
```

Keep `AGENTS.md` under roughly 100 lines. Link to deeper docs by task.

Do not add an agent index just to satisfy a checklist when the repo has one
canonical, structured source such as `README.org` and a query tool for targeted
section retrieval.

### Canonical Org Manual

Use when a project intentionally keeps knowledge in one large Org file.

Shape:

```text
AGENTS.md                # tells agents to use oq for Org files
README.org               # canonical source of truth
skills/oq/SKILL.md       # structure-first queries for large Org files
```

Rules:
- Start with headings/search, not full text extraction.
- Extract only the relevant section.
- Do not create mirrored Markdown docs unless they are generated or enforced.
- Treat missing `docs/ARCHITECTURE.md` as acceptable when `README.org` is
  explicitly canonical.

### Selective Skill Injection

Use when many skills exist or the model is small.

Selection signals:
- current user prompt,
- recent tool error,
- active plan step,
- file type,
- task phase.

Inject only skill name, short description, and path unless the skill is clearly
needed.

### Diff-Based Reads

Use when agents repeatedly inspect edited files.

Behavior:
- first read returns full content,
- repeated unchanged read returns "unchanged",
- changed read returns compact unified diff if smaller than full file,
- full read remains available by explicit request.

## Tool Patterns

### Phase Tool Routing

Use when models choose wrong tools or schemas are large.

Phases:
- discover: `project_root`, `directory_tree`, `grep`, `read_file`,
  `lines_range`
- edit: `edit_file`, `write_file`, `append_file`, formatter/test command
- review: `git diff`, tests, lint, static checks
- operate: logs, metrics, traces, browser/UI tools

### Compound Tools

Use when repeated chains waste turns.

Examples:
- `search_and_read(pattern, dir, context_lines)`
- `read_and_edit(file, selector, replacement)`
- `write_and_test(file, content, test_command)`
- `run_ui_path(path_name)` for browser validation

Compound tools should return structured, bounded evidence.

### Tool Dedup

Use when small models repeat reads/searches.

Cache pure tools by normalized arguments:
- reads,
- line ranges,
- grep,
- directory tree,
- project root.

Return `[cached]` plus the result. Do not cache mutating tools.

### Trust Decay

Use when a tool fails repeatedly.

Example policy:
- 1 failure: return normal failure.
- 2 failures: add repair hint.
- 3 failures: hide or de-prioritize the tool for this session.
- recovery: restore after successful adjacent tool or new phase.

## Planning Patterns

### Active Plan Anchor

Use for multi-step tasks.

Store plan outside the prompt, then inject a compact state:

```text
ACTIVE PLAN (step 2 of 5)
[x] Locate target code
[-] Implement change
[ ] Run tests
[ ] Review diff
[ ] Report result
```

For complex work, persist the plan in `docs/exec-plans/active/`.

### Evidence Store

Use when long tasks need durable facts.

Store:
- commands tried,
- tests that passed/failed,
- files changed,
- decisions made,
- links or citations,
- validation outcomes.

Avoid storing full traces. Store compact, searchable digests.

## Guardrail Patterns

### Strict Write Mode

Use for small models and autonomous edits.

Policy:
- `write_file` creates new files only.
- existing files require `edit_file` or patch-style tools.
- mutating tools require prior read in the same session.

### Checkpoint Before Multi-File Edit

Use when editing more than one file or running autonomous loops.

Options:
- git worktree branch,
- patch checkpoint,
- file snapshot map,
- test-created temp workspace.

Rollback on hard validation failure or user cancellation.

### Agent-Legible Hook Output

Use for formatting, lint, tests, codegen, or docs freshness.

Error format:

```text
CHECK FAILED: make test
Relevant output:
...
Next action:
1. Read tests/foo_test.py lines 40-80.
2. Fix assertion shape.
3. Re-run make test.
```

## Eval Patterns

### Scaffold Profile Eval

Use before adopting a new harness feature.

Compare:
- baseline,
- limited tools,
- compound tools,
- strict write,
- selective skills,
- quality monitor,
- all changes.

Track:
- pass/fail,
- tool-call count,
- repeated calls,
- edit rejection recovery,
- final syntax validity,
- wall-clock time,
- context size.
