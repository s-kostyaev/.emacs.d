---
name: ddgr-web-search
description: Run DuckDuckGo web searches from the terminal using `ddgr` with JSON output. Use when the task calls for lightweight web search, quick result triage, or filtering by region/time/site.
---

# DDGR Web Search (JSON)

## Overview

Use `ddgr "query" --json` to fetch search results as structured JSON, then extract only the minimal fields needed for the task.

## Workflow

1. Run a JSON search (non-interactive).

```bash
ddgr "cli websearch" --json
```

2. If you need fewer/more results or filters, add options:

```bash
ddgr "cli websearch" --json -n 5
ddgr "cli websearch" --json -t w
ddgr "cli websearch" --json -w "example.com"
ddgr "cli websearch" --json -r "us-en"
```

3. Summarize or cite results using only a small subset of entries. Avoid copying full JSON into context.

## Useful Options (Non-Exhaustive)

- `--json`: output results as JSON
- `-n N`: results per page (0-25)
- `-t d|w|m|y`: time filter (day/week/month/year)
- `-w SITE`: restrict to a site/domain
- `-r REG`: region code (e.g., `us-en`)
- `--unsafe`: disable safe search if needed
- `-x`: expand full URLs
- `--proxy URI`: use HTTPS proxy
