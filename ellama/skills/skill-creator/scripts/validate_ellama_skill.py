#!/usr/bin/env python3
"""Validate basic Ellama Agent Skill structure."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


NAME_RE = re.compile(r"^[a-z0-9][a-z0-9-]{0,62}[a-z0-9]$|^[a-z0-9]$")
LINK_RE = re.compile(r"`((?:references|scripts|templates|assets)/[^`]+)`")
PLACEHOLDER_RE = re.compile(r"{([A-Za-z0-9_-]+)}")


def parse_frontmatter(text: str) -> tuple[dict[str, str], int]:
    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        raise ValueError("SKILL.md must start with YAML frontmatter")
    try:
        end = next(i for i, line in enumerate(lines[1:], start=1) if line.strip() == "---")
    except StopIteration as exc:
        raise ValueError("YAML frontmatter must end with ---") from exc

    data: dict[str, str] = {}
    for lineno, line in enumerate(lines[1:end], start=2):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if ":" not in stripped:
            raise ValueError(f"Invalid frontmatter line {lineno}: {line}")
        key, value = stripped.split(":", 1)
        key = key.strip()
        value = value.strip().strip("'\"")
        if not key:
            raise ValueError(f"Empty frontmatter key on line {lineno}")
        data[key] = value
    return data, end + 1


def validate_skill(path: Path) -> tuple[list[str], list[str]]:
    errors: list[str] = []
    warnings: list[str] = []

    skill_dir = path.expanduser().resolve()
    skill_file = skill_dir / "SKILL.md"
    if not skill_dir.is_dir():
        return [f"Not a directory: {skill_dir}"], warnings
    if not skill_file.is_file():
        return [f"Missing SKILL.md: {skill_file}"], warnings

    text = skill_file.read_text(encoding="utf-8")
    try:
        meta, body_start = parse_frontmatter(text)
    except ValueError as exc:
        return [str(exc)], warnings

    name = meta.get("name", "")
    description = meta.get("description", "")
    if not name:
        errors.append("Missing frontmatter field: name")
    elif not NAME_RE.match(name):
        errors.append("name must use lowercase letters, digits, and hyphens")
    elif skill_dir.name != name:
        warnings.append(f"Folder name '{skill_dir.name}' differs from skill name '{name}'")

    if not description:
        errors.append("Missing frontmatter field: description")
    elif len(description) < 40:
        warnings.append("description is short; include specific trigger contexts")

    body = "\n".join(text.splitlines()[body_start:])
    if len(body.strip()) < 40:
        warnings.append("SKILL.md body is very short")

    for match in LINK_RE.finditer(text):
        relative = match.group(1)
        if not (skill_dir / relative).exists():
            errors.append(f"Referenced bundled file does not exist: {relative}")

    for template in (skill_dir / "templates").glob("**/*") if (skill_dir / "templates").exists() else []:
        if template.is_file():
            template_text = template.read_text(encoding="utf-8")
            placeholders = sorted(set(PLACEHOLDER_RE.findall(template_text)))
            if placeholders and "report_result" not in template_text:
                warnings.append(
                    f"Template {template.relative_to(skill_dir)} has placeholders "
                    "but does not mention report_result"
                )

    return errors, warnings


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("skill_dir", help="Path to a skill directory")
    args = parser.parse_args()

    errors, warnings = validate_skill(Path(args.skill_dir))
    for warning in warnings:
        print(f"WARN: {warning}", file=sys.stderr)
    for error in errors:
        print(f"ERROR: {error}", file=sys.stderr)
    if errors:
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
