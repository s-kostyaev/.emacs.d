# Porting Codex/OpenAI Skills to Ellama

Port behavior, not platform assumptions.

## Replace Codex-Specific Assumptions

Remove or rewrite:

- `$CODEX_HOME` discovery defaults.
- `agents/openai.yaml` generation requirements.
- Codex-only MCP/resource loading instructions.
- Codex-specific tool names such as `apply_patch` unless the target Ellama
  environment exposes them.
- Instructions that assume a fixed web browser, image generator, or hosted
  API.

Add Ellama-specific guidance:

- Global skills live under `~/.emacs.d/ellama/skills/` by default.
- Project skills live under `skills/` in the project root.
- Ellama loads only metadata initially; `read_file` loads the body.
- Subagents use `task` and must finish through `report_result`.
- Task prompt templates should live in `templates/` and be resolved with
  `template_base`.
- Tool availability may vary by role, so skill instructions should name the
  minimum tools needed.

## Frontmatter

Keep at least:

```yaml
---
name: skill-name
description: What the skill does. Use when ...
---
```

Do not rely on extra fields for triggering. Ellama's skill prompt uses `name`,
`description`, and location.

## Resource Mapping

Codex `references/` usually maps directly to Ellama `references/`.

Codex `scripts/` maps directly when scripts are portable. Remove assumptions
about unavailable packages and prefer Python standard library for bootstrap
scripts.

Codex `assets/` maps directly when files are output resources.

Codex `agents/openai.yaml` usually has no Ellama equivalent. Omit it unless a
separate UI or marketplace workflow consumes it.

Codex subagent instructions should become Ellama `task` templates under
`templates/`. Include explicit placeholder names and a required
`report_result` instruction inside each template.

## Porting Steps

1. Read the original skill and list user-facing behaviors.
2. Delete platform-specific setup and metadata requirements.
3. Rewrite the trigger-rich `description` for Ellama.
4. Convert the main body into a short Ellama tool workflow.
5. Move long examples and variant details into `references/`.
6. Convert repeated deterministic operations into scripts.
7. Convert reusable delegated prompts into `templates/`.
8. Validate with `scripts/validate_ellama_skill.py`.
9. Smoke-test on one realistic prompt if a suitable agent runner is available.

## Common Failure Modes

- The port keeps "Codex" in instructions where the target agent should read
  "Ellama".
- Trigger examples stay in the body instead of `description`.
- The skill says to use a script that was not bundled.
- A template references placeholders not supplied in the documented task call.
- The port creates extra docs such as README files that are not used by the
  agent.
- The skill asks too many clarifying questions before doing useful work.
