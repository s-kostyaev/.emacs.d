---
name: web-browse-context
description: Efficiently capture web pages as clean markdown and query them with `mq` to avoid loading full content into context. Use when browsing web pages, extracting specific sections, or summarizing sources with minimal context usage.
---

# Web Browsing With Context Economy

## Overview

Capture web pages into local markdown, inspect structure with `mq`, and extract only the relevant sections to keep context small and focused.

## Workflow

0. Use shell_command tool for every command from this skill.

1. Save the page as markdown (cleaned).

```bash
npx -y url-to-markdown-cli-tool 'https://example.com' --clean-content > /tmp/result.md
```

2. Inspect structure before reading content.

```bash
mq /tmp/result.md '.tree'
```

3. Extract only the needed sections.

```bash
mq /tmp/result.md '.section("Using Blueprints") | .text'
```

4. Search when you are not sure where the content lives.

```bash
mq /tmp/result.md '.search("auth")'
```

5. If the search returns a large subtree, narrow it by re-running `.tree` on the specific section(s), then extract `.text`.

## Query Patterns

- View a fuller outline: `mq /tmp/result.md '.tree("preview")'`
- Get all headings: `mq /tmp/result.md '.headings'`
- Extract code blocks by language: `mq /tmp/result.md '.code("bash")'`
- List links: `mq /tmp/result.md '.links'`
- Extract a section: `mq /tmp/result.md '.section("Name") | .text'`

## Context Hygiene

- Never `cat` the whole markdown file into the conversation.
- Prefer `.tree` and `.search` to locate content, then extract only the minimal `.text` needed.
- Use one file per page (e.g., `result-pricing.md`, `result-api.md`) to keep sources separated.
- If a page changes, re-run the capture command and overwrite the corresponding file.
- Use `mq --help` whenever you need more selectors or flags.
