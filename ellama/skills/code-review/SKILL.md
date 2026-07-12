---
name: code-review
description: Review code changes with a focused multi-agent workflow. Use when auditing a diff, branch, pull request, patch, commit range, or local worktree for bugs, regressions, missing tests, security issues, API breakage, maintainability risks, or review readiness.
license: MIT
---

# Code Review

You are the **Lead Reviewer**. Your goal is to produce a concise, evidence-based
review of code changes. Prioritize defects and risks over style commentary.

## Phase 1: Scope, Recon & Plan

Let `SKILL_DIR` be the directory containing this `SKILL.md` file. Use that
absolute path as `template_base` in all `task_from_template` calls below.

1. **Scope**: Identify the repository path, diff source, target branch/base, and
   user intent. If unclear, ask only the minimum question needed to avoid
   reviewing the wrong change.
2. **Reconnaissance**: Inspect the change map before reading deeply.
   - Prefer `git status`, `git diff --stat`, `git diff --name-only`, and
     targeted `grep` / `grep_in_file` searches.
   - Read repo-local review instructions such as `AGENTS.md`, `README*`,
     `CONTRIBUTING*`, test docs, and package metadata when relevant.
   - Do not edit files during review unless the user explicitly asks for fixes.
3. **Framework**: Read `references/review-framework.md` from `SKILL_DIR`. Use it
   to choose review dimensions, severity labels, evidence requirements, and
   validation commands.
4. **Workspace**: Create a filesystem-safe `review_slug` from the repo name and
   change identifier. Save review artifacts under
   `~/.emacs.d/ellama/code-review/{review_slug}/` with a `review_notes/`
   subdirectory.
5. **Plan**: Create
   `~/.emacs.d/ellama/code-review/{review_slug}/review_plan.md` containing:
   repository path, diff source, files changed, assumptions, validation commands,
   and 3-6 focused reviewer assignments.

## Phase 2: Parallel Focused Review

Spawn **Reviewer Agents** for each selected focus. Sub-agents have no memory of
this conversation. Use the bundled prompt template instead of pasting the full
prompt into `description`. Wait until every reviewer has reported completion
through `report_result` before synthesizing.

Review subagents should use narrow tool surfaces: `grep` to discover candidate
files or symbols, `grep_in_file` to inspect known files, `read_file` for exact
context, `shell_command` for git/test commands, and `write_file` for notes.

Typical reviewer focuses:

- Correctness and edge cases
- Tests and regressions
- Security, privacy, and secrets
- API, compatibility, and data contracts
- Architecture, maintainability, and duplication
- UI, accessibility, and user workflows

Only spawn focuses that fit the actual change. Small diffs may need just two or
three reviewers.

**MANDATORY `task_from_template` call shape for each reviewer:**

```json
{
  "template": "templates/reviewer.md",
  "template_base": "<SKILL_DIR>",
  "arguments": {
    "brief_description_of_change": "...",
    "repository_path": "...",
    "diff_source": "...",
    "review_slug": "...",
    "review_plan_path": "~/.emacs.d/ellama/code-review/{review_slug}/review_plan.md",
    "review_focus": "...",
    "review_focus_slug": "..."
  },
  "role": "general"
}
```

## Phase 3: Synthesis

Only after all reviewer agents have reported completion, spawn a **Synthesis
Reviewer Agent**. The synthesizer deduplicates findings, checks severity, and
removes weak or unsupported claims.

**MANDATORY `task_from_template` call shape for the synthesizer:**

```json
{
  "template": "templates/synthesizer.md",
  "template_base": "<SKILL_DIR>",
  "arguments": {
    "brief_description_of_change": "...",
    "repository_path": "...",
    "diff_source": "...",
    "review_slug": "...",
    "review_plan_path": "~/.emacs.d/ellama/code-review/{review_slug}/review_plan.md"
  },
  "role": "general"
}
```

## Phase 4: Finalize

1. Apply the quality checklist from `references/review-framework.md`.
2. Confirm these files exist:
   - `~/.emacs.d/ellama/code-review/{review_slug}/review_plan.md`
   - `~/.emacs.d/ellama/code-review/{review_slug}/review_notes/*.md`
   - `~/.emacs.d/ellama/code-review/{review_slug}/{review_slug}-review.md`
3. Present findings first, ordered by severity, with file and line references.
4. Include a brief note on validation performed and any review gaps.

## File Structure

```text
~/.emacs.d/ellama/code-review/{review_slug}/
├── review_plan.md
├── review_notes/
│   ├── correctness.md
│   └── tests.md
└── {review_slug}-review.md
```
