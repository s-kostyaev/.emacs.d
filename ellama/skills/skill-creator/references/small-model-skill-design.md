# Small-Model Skill Design

Small/local models need fewer choices, more explicit phase boundaries, and
mechanical feedback. Optimize the skill so the next action is obvious.

## Design Rules

1. Keep `SKILL.md` short.
   Put only the default workflow, routing, and validation in the main file.
   Move variants, examples, and background into `references/`.

2. Use numbered phases.
   Small models follow ordered steps better than long prose. Each phase should
   have one purpose and a clear exit condition.

3. Prefer exact tool names.
   Say `grep_in_file`, `lines_range`, `edit_file`, or `write_file` instead of
   "inspect" or "modify" when the right operation is predictable.

4. Minimize branching.
   If there are many variants, use a routing table:

   ```text
   If task mentions X, read references/x.md.
   If task mentions Y, read references/y.md.
   Otherwise continue with the default workflow.
   ```

5. Make validation executable.
   Add a script, command, checklist, or file existence check. Avoid relying on
   "review carefully" as the only validation.

6. Use templates for subagents.
   Do not paste long reviewer or researcher prompts into `SKILL.md`. Put them
   in `templates/` and pass them through the `task` tool.

7. Prevent loop-prone behavior.
   Tell the agent what to do when a tool result is empty, truncated, blocked, or
   repeated. Example: "If `grep` returns no matches, run `directory_tree` once,
   then inspect likely files; do not repeat the same grep."

## Good Instruction Shape

```text
1. Run `directory_tree` on the target directory.
2. Read only `SKILL.md` first.
3. If the skill has `references/`, read only the reference named by the current
   task.
4. Edit with `edit_file` for existing files or `write_file` for new files.
5. Run the validator.
6. Call `report_result` with the path and validation result.
```

## Avoid

- Long motivational introductions.
- Multiple names for the same action.
- Unbounded "research all relevant files" instructions.
- Hidden trigger logic in the body.
- References that are not linked from `SKILL.md`.
- Optional directories created "just in case".
- Large examples in the main skill file.

## Degrees of Freedom

Use high freedom for writing, strategy, and review tasks. Give principles and a
short output shape.

Use medium freedom for tasks with a preferred workflow but context-dependent
decisions. Give numbered phases and routing to references.

Use low freedom for fragile tasks. Provide scripts, exact commands, examples,
and validation checks.
