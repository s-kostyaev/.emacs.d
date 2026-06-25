---
name: skill-creator
description: Create or update Ellama Agent Skills. Use when designing new skills, porting skills from other agent systems, improving SKILL.md instructions, adding bundled resources, validating metadata, or adapting skills for small/local models.
---

# Skill Creator

Create Ellama skills that are small enough to trigger reliably, explicit
enough for local models, and backed by deterministic tools when work should not
depend on model memory.

## Workflow

1. Identify concrete trigger examples.
   - Ask at most one or two questions if the user has not provided examples.
   - Capture what a user would say, what files/tools are involved, and what a
     correct output looks like.
   - Reuse the location the user gave. If none is given, default to
     `~/.emacs.d/ellama/skills/` for global skills or `skills/` for
     project-local skills.

2. Choose the skill shape.
   - Use only `SKILL.md` for a simple procedural skill.
   - Add `references/` for detailed guidance that should be loaded only when
     needed.
   - Add `scripts/` for fragile, repetitive, or mechanically checkable work.
   - Add `templates/` for reusable `task` subagent prompts.
   - Add `assets/` only for files copied or transformed into final outputs.

3. Initialize the folder.
   - Prefer the bundled initializer:
     `python3 <skill-dir>/scripts/init_ellama_skill.py <name> --path <dir>`.
   - Use lowercase letters, digits, and hyphens for names.
   - Make the folder name match the `name` field.

4. Write `SKILL.md`.
   - Use YAML frontmatter with at least `name` and `description`.
   - Put all trigger conditions in `description`; the body is loaded only after
     the model has already selected the skill.
   - Keep the body as a routing and workflow guide, not a manual.
   - Link every reference file from `SKILL.md` and say when to read it.

5. Adapt for Ellama tools.
   - Use `read_file` to load skill instructions and references.
   - Use `grep`, `grep_in_file`, `lines_range`, and `directory_tree` before
     broad reads.
   - Use `write_file` for new files and `edit_file` for precise edits.
   - Use `shell_command` only when deterministic validation or generation is
     worth the extra tool risk.
   - Use `ask_user` only when a missing answer would make the skill wrong.
   - Use `task` only for clearly separable work; require subagents to finish
     with `report_result`.

6. Validate.
   - Run `python3 <skill-dir>/scripts/validate_ellama_skill.py <target-skill>`.
   - Run or smoke-test any added scripts.
   - For skills that use `task`, check template placeholders against the
     documented `arguments` object.

7. Iterate from real failures.
   - Prefer changing a script, template, reference, or validation check over
     adding vague warnings to `SKILL.md`.
   - When a small model repeats the same mistake, add a mechanical example or a
     narrower instruction near the step where the mistake happens.

## Ellama References

Read only the file needed for the current decision:

- `references/ellama-skill-format.md`: Ellama skill structure, frontmatter,
  discovery paths, task templates, and validation rules.
- `references/small-model-skill-design.md`: How to make skills robust for
  small/local models.
- `references/porting-codex-skills.md`: How to port Codex/OpenAI skills to
  Ellama without copying incompatible assumptions.

## Output Shape

When creating or updating a skill, report:

```text
Skill path:
Files created or changed:
Validation:
Notes:
```

Keep the final answer short. Mention only meaningful tradeoffs, missing inputs,
or validation that could not be run.
