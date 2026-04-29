---
name: deep-research
description: Use this skill for academic-level research via a multi-agent swarm.
license: MIT
---

# Deep Research

You are the **Lead Agent**. Your goal is to orchestrate a comprehensive research report.

## Phase 1: Scope, Recon & Plan

Let `SKILL_DIR` be the directory containing this `SKILL.md` file. Use
that actual directory path as the `template_base` value in all `task` calls
below. Replace `<SKILL_DIR>` with that absolute directory path before calling
`task`.

1.  **Interview**: Use `ask_user` to clarify Objective, Audience, and Scope (max
    3-4 questions). Ask questions one by one (one at a time), providing possible
    answer variants array for every question.
2.  **Reconnaissance**: Use `ddgr-web-search` SKILL to perform 3-5 broad searches.
    Identify key themes, vocabulary, and authoritative domains.
3.  **Methodology**: Read
    `references/research-frameworks.md` from `SKILL_DIR`. Use it to choose the
    research depth, planning framework, source-quality criteria, and synthesis
    strategy. Record those choices in the research plan.
4.  **Workspace**: Create a filesystem-safe `main_topic_slug` from the main
    topic: lowercase it, replace every run of non-alphanumeric characters with
    `-`, and trim leading/trailing `-`. Save all files under
    `~/.emacs.d/ellama/deep-research/{main_topic_slug}/`. Create that directory
    and its `research_notes/` subdirectory before spawning sub-agents.
5.  **Plan**: Create
    `~/.emacs.d/ellama/deep-research/{main_topic_slug}/research_plan.md`.
    Decompose the topic into 6-10 distinct sub-threads based on your recon.

## Phase 2: Parallel Research (The Swarm)

Spawn **Researcher Agents** for *each* sub-topic. **CRITICAL**: Sub-agents have
NO memory of this conversation. Use the `task` tool with the bundled prompt
template instead of pasting the full prompt into `description`.
If the tool returns a template validation error, retry with the exact argument
names shown in its hint.

Do **not** spawn the writer agent in this phase. Wait until every researcher
agent has reported completion through `report_result`. Treat missing,
incomplete, or still-running researcher results as a hard blocker for Phase 3.

**MANDATORY `task` call shape for each researcher:**

```json
{
  "template": "templates/researcher.md",
  "template_base": "<SKILL_DIR>",
  "arguments": {
    "brief_description_of_overall_project": "...",
    "main_topic": "...",
    "main_topic_slug": "...",
    "research_plan_path": "~/.emacs.d/ellama/deep-research/{main_topic_slug}/research_plan.md",
    "subtopic_name": "...",
    "subtopic_slug": "..."
  },
  "role": "general"
}
```

## Phase 3: Synthesis & Report

Only after **all** researcher agents have reported completion through
`report_result`, spawn a **Report-Writer Agent**. Do not call the writer task
early, even if some researcher outputs already look sufficient.

**MANDATORY `task` call shape for the writer:**

```json
{
  "template": "templates/writer.md",
  "template_base": "<SKILL_DIR>",
  "arguments": {
    "brief_description_of_overall_project": "...",
    "main_topic": "...",
    "main_topic_slug": "...",
    "research_plan_path": "~/.emacs.d/ellama/deep-research/{main_topic_slug}/research_plan.md"
  },
  "role": "general"
}
```

## Phase 4: Finalize

1.  Apply the report quality checklist from
    `references/research-frameworks.md`.
2.  Confirm
    `~/.emacs.d/ellama/deep-research/{main_topic_slug}/{main_topic_slug}-report.md`
    and
    `~/.emacs.d/ellama/deep-research/{main_topic_slug}/{main_topic_slug}-sources.md`
    exist.
3.  Present a brief verbal summary to the user.
4.  Ask if they want to expand on any specific section.

## File Structure

```text
~/.emacs.d/ellama/deep-research/{main_topic_slug}/
├── research_plan.md
├── research_notes/      # Raw agent outputs
│   ├── subtopic-a.md
│   └── subtopic-b.md
├── {main_topic_slug}-report.md    # Final Output
└── {main_topic_slug}-sources.md   # Final Output
```
