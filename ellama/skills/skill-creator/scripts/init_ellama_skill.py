#!/usr/bin/env python3
"""Initialize an Ellama Agent Skill folder."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


NAME_RE = re.compile(r"^[a-z0-9][a-z0-9-]{0,62}[a-z0-9]$|^[a-z0-9]$")


def normalize_name(value: str) -> str:
    name = re.sub(r"[^a-z0-9]+", "-", value.lower()).strip("-")
    name = re.sub(r"-+", "-", name)
    if not name:
        raise SystemExit("Skill name must contain at least one letter or digit")
    if len(name) > 64:
        name = name[:64].rstrip("-")
    if not NAME_RE.match(name):
        raise SystemExit(f"Invalid normalized skill name: {name}")
    return name


def write_new(path: Path, content: str) -> None:
    if path.exists():
        return
    path.write_text(content, encoding="utf-8")


def skill_template(name: str, description: str) -> str:
    return f"""---
name: {name}
description: {description}
---

# {name}

Describe the workflow this skill should perform.

## Workflow

1. Inspect the user's request and identify the target files or artifacts.
2. Load only the bundled reference needed for the current variant.
3. Use deterministic tools or scripts for fragile operations.
4. Validate the result before final output.

## References

Add references here only after creating files under `references/`.

## Output Shape

```text
Result:
Files:
Validation:
```
"""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("name", help="Skill name or title")
    parser.add_argument(
        "--path",
        default=str(Path.home() / ".emacs.d" / "ellama" / "skills"),
        help="Directory that will contain the skill folder",
    )
    parser.add_argument(
        "--description",
        default=None,
        help="Frontmatter description. A TODO description is used when omitted.",
    )
    parser.add_argument(
        "--resources",
        default="",
        help="Comma-separated optional directories: references,scripts,templates,assets",
    )
    args = parser.parse_args()

    name = normalize_name(args.name)
    root = Path(args.path).expanduser()
    skill_dir = root / name
    skill_dir.mkdir(parents=True, exist_ok=True)

    allowed = {"references", "scripts", "templates", "assets"}
    resources = {item.strip() for item in args.resources.split(",") if item.strip()}
    unknown = resources - allowed
    if unknown:
        raise SystemExit(f"Unknown resource directories: {', '.join(sorted(unknown))}")

    for resource in sorted(resources):
        (skill_dir / resource).mkdir(exist_ok=True)

    description = args.description or (
        f"TODO: Describe what {name} does and when Ellama should use it."
    )
    write_new(skill_dir / "SKILL.md", skill_template(name, description))
    print(skill_dir)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
