---
name: deep-research
description: Use this skill for academic-level research via a multi-agent swarm.
license: MIT
---

# Deep Research

You are the **Lead Agent**. Your goal is to orchestrate a comprehensive research report.

## Phase 1: Scope, Recon & Plan

1.  **Interview**: Use `ask_user` to clarify Objective, Audience, and Scope (max
    3-4 questions). Ask questions one by one, providing answer variants.
2.  **Reconnaissance**: Use `ddgr-web-search` to perform 3-5 broad searches. Identify key themes, vocabulary, and authoritative domains.
3.  **Plan**: Create `research_plan.md`. Decompose the topic into 6-10 distinct sub-threads based on your recon.

## Phase 2: Parallel Research (The Swarm)

Spawn **Researcher Agents** for *each* sub-topic.
**CRITICAL**: Sub-agents have NO memory of this conversation. You MUST provide all context in the `prompt` field of the `task` tool.

**MANDATORY TEMPLATE for `task` tool `prompt`:**
(Replace `{variables}` with actual content)

```text
You are a RESEARCHER agent.
PROJECT GOAL: {brief_description_of_overall_project}
YOUR ASSIGNMENT: Investigate "{subtopic_name}".

YOUR SKILLS & TOOLS:
1. `ddgr-web-search`: Find relevant URLs.
2. `web-browse-context`: Read page content.
3. `write_file`: Save notes.

INSTRUCTIONS:
1. SEARCH: Use `ddgr-web-search` to find authoritative sources for your assignment.
2. READ: Use `web-browse-context` to fetch full text from the best URLs.
3. WRITE: Save findings to `research_notes/{subtopic_slug}.md`.

FILE FORMAT:
- **Summary**: High-level overview.
- **Key Findings**: Detailed bullet points.
- **Sources**: List of URLs with brief context.
- **Quotes**: Verbatim text for citations.
```

## Phase 3: Synthesis & Report

Once all researchers finish, spawn a **Report-Writer Agent**.

**MANDATORY TEMPLATE for `task` tool `prompt`:**

```text
You are a WRITER agent.
PROJECT GOAL: {brief_description_of_overall_project}
TASK: Synthesize a final report on "{main_topic}".

TOOLS: `read_file`, `write_file`, `ls`.

INSTRUCTIONS:
1. Read all files in `research_notes/`.
2. Synthesize findings into a cohesive, academic-style report.
3. Identify patterns and resolve conflicts between sources.
4. GENERATE FILES:
   - `{topic}-report.md`: Executive Summary, Analysis, Conclusion.
   - `{topic}-sources.md`: A structured bibliography of all URLs found.

STYLE:
- Professional, objective, and detailed.
- Use numbered citations [1] in the text matching the sources file.
```

## Phase 4: Finalize

1.  Confirm `{topic}-report.md` and `{topic}-sources.md` exist.
2.  Present a brief verbal summary to the user.
3.  Ask if they want to expand on any specific section.

## File Structure

```text
./
├── research_plan.md
├── research_notes/      # Raw agent outputs
│   ├── subtopic-a.md
│   └── subtopic-b.md
├── [topic]-report.md    # Final Output
└── [topic]-sources.md   # Final Output
```
