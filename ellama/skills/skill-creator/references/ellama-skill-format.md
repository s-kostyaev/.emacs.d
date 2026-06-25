# Ellama Skill Format

## Minimal Structure

```text
skill-name/
├── SKILL.md
├── references/
├── scripts/
├── templates/
└── assets/
```

Only `SKILL.md` is required. Create optional directories only when they serve
the skill.

Ellama discovers skills in:

- `~/.emacs.d/ellama/skills/` by default through `ellama-skills-global-path`.
- `skills/` under the project root through `ellama-skills-local-path`.

Ellama injects only skill metadata into the system prompt: `name`,
`description`, and the `SKILL.md` path. The model must use `read_file` on that
path to load the full instructions.

## Frontmatter

Use YAML frontmatter at the top of `SKILL.md`:

```markdown
---
name: pdf-processing
description: Extract, inspect, transform, and validate PDF files. Use when the user asks to read, rotate, split, merge, OCR, or summarize PDFs.
---
```

Rules:

- `name` is required.
- `description` is required and is the main trigger surface.
- Prefer folder name == `name`.
- Use lowercase letters, digits, and hyphens for portable names.
- Extra metadata can exist, but do not add it unless another tool consumes it.

## Body

Make the body a short workflow plus routing table:

- What to do first.
- Which tool to use for each phase.
- Which bundled reference to read for each variant.
- What to validate before final output.

Do not put trigger criteria only in the body. A model cannot use body text to
select the skill before the skill is loaded.

## Bundled Resources

Use `references/` for longer instructions that should be loaded selectively.
Every reference must be linked from `SKILL.md` with a concrete "read when"
condition.

Use `scripts/` for repeatable operations, validators, file converters, parsers,
or scaffolders. Scripts should have a `--help` path and avoid project-specific
defaults unless the skill is project-specific.

Use `templates/` for prompts passed to the `task` tool. Templates are plain text
with placeholders such as `{topic}`.

Use `assets/` for files copied or transformed into user outputs. Do not load
assets into context unless inspection is necessary.

## Task Templates

Ellama's `task` tool accepts either a free-form `description` or:

```json
{
  "template": "templates/reviewer.md",
  "template_base": "/absolute/path/to/skill",
  "arguments": {
    "topic": "Example"
  },
  "role": "general"
}
```

Template rules:

- Use a relative `template` path.
- Set `template_base` to the actual skill directory.
- Make `arguments` keys exactly match placeholders in the template.
- If validation fails, retry with the shape shown in the tool error.
- Tell subagents to call `report_result` exactly once when complete.

## Validation Checklist

Before considering a skill done:

- `SKILL.md` exists and starts with valid frontmatter.
- `name` and `description` are present and non-empty.
- Folder name matches `name` unless intentionally namespaced.
- Body references only files that exist.
- Every large or variant-specific detail is behind a reference.
- Scripts are executable or have clear `python3 path/script.py` usage.
- Task templates have placeholders documented in the expected `arguments`.
- The final answer says what validation was run.
