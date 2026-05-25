#!/usr/bin/env python3
"""Read-only harness inventory for an agentic repository.

The script scans a project for agent-facing context, validation commands,
guardrails, skills, docs, and CI hints. It prints a compact Markdown report.
It does not modify files.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
from typing import Iterable


DOC_NAMES = [
    "AGENTS.md",
    "docs/agent-index.md",
    "docs/ARCHITECTURE.md",
    "docs/TESTING.md",
    "docs/QUALITY.md",
    "docs/SECURITY.md",
    "docs/RELIABILITY.md",
    "README.md",
    "README.org",
]

TEST_HINTS = [
    "Makefile",
    "package.json",
    "pyproject.toml",
    "pytest.ini",
    "Cargo.toml",
    "go.mod",
    "pom.xml",
    "build.gradle",
    "Gemfile",
]

GUARDRAIL_HINTS = [
    ".pre-commit-config.yaml",
    ".github/workflows",
    "scripts/git-hooks",
    ".dir-locals.el",
    ".srt-settings.json",
    "srt-settings.json",
]


def exists(root: Path, rel: str) -> bool:
    return (root / rel).exists()


def find_files(root: Path, names: Iterable[str], limit: int = 50) -> list[str]:
    wanted = set(names)
    found: list[str] = []
    skip_dirs = {".git", "node_modules", ".venv", "venv", "target", "dist", "build"}
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in skip_dirs]
        base = Path(dirpath)
        for filename in filenames:
            if filename in wanted:
                found.append(str((base / filename).relative_to(root)))
                if len(found) >= limit:
                    return sorted(found)
    return sorted(found)


def skill_dirs(root: Path) -> list[str]:
    out: list[str] = []
    for parent in ["skills", ".codex/skills", ".emacs.d/ellama/skills"]:
        path = root / parent
        if not path.exists():
            continue
        for skill in path.glob("*/SKILL.md"):
            out.append(str(skill.parent.relative_to(root)))
    return sorted(out)


def has_global_skill(name: str) -> bool:
    home = Path.home()
    candidates = [
        home / ".codex" / "skills" / name / "SKILL.md",
        home / ".emacs.d" / "ellama" / "skills" / name / "SKILL.md",
    ]
    return any(path.exists() for path in candidates)


def file_mentions(path: Path, needles: Iterable[str]) -> bool:
    if not path.exists() or not path.is_file():
        return False
    text = path.read_text(errors="ignore").lower()
    return all(needle.lower() in text for needle in needles)


def canonical_org_knowledge(root: Path, docs: list[str], skills: list[str]) -> list[str]:
    if "README.org" not in docs:
        return []
    has_oq = any(Path(skill).name == "oq" for skill in skills) or has_global_skill("oq")
    agents_mentions_oq = file_mentions(root / "AGENTS.md", ["oq", ".org"])
    if has_oq and agents_mentions_oq:
        return ["README.org + oq"]
    return []


def make_targets(root: Path) -> list[str]:
    makefile = root / "Makefile"
    if not makefile.exists():
        return []
    targets: list[str] = []
    for line in makefile.read_text(errors="ignore").splitlines():
        if line.startswith("\t") or ":" not in line:
            continue
        name = line.split(":", 1)[0].strip()
        if name.startswith("."):
            continue
        if name and all(c.isalnum() or c in "._-" for c in name):
            targets.append(name)
    return targets[:40]


def package_scripts(root: Path) -> dict[str, str]:
    package = root / "package.json"
    if not package.exists():
        return {}
    try:
        data = json.loads(package.read_text(errors="ignore"))
    except Exception:
        return {}
    scripts = data.get("scripts")
    if isinstance(scripts, dict):
        return {str(k): str(v) for k, v in scripts.items()}
    return {}


def score(section: list[str]) -> int:
    return 1 if section else 0


def print_list(title: str, items: list[str]) -> None:
    print(f"## {title}")
    if not items:
        print("- none found")
    else:
        for item in items:
            print(f"- {item}")
    print()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("root", nargs="?", default=".", help="Repository root")
    args = parser.parse_args()

    root = Path(args.root).expanduser().resolve()
    if not root.exists() or not root.is_dir():
        raise SystemExit(f"Not a directory: {root}")

    docs = [name for name in DOC_NAMES if exists(root, name)]
    test_files = [name for name in TEST_HINTS if exists(root, name)]
    guardrails = [name for name in GUARDRAIL_HINTS if exists(root, name)]
    ci = []
    workflows = root / ".github" / "workflows"
    if workflows.exists():
        ci = [str(p.relative_to(root)) for p in sorted(workflows.glob("*"))[:30]]
    skills = skill_dirs(root)
    make = make_targets(root)
    scripts = package_scripts(root)
    local_docs = find_files(root, ["ARCHITECTURE.md", "TESTING.md", "QUALITY.md", "SECURITY.md"], 30)
    canonical_org = canonical_org_knowledge(root, docs, skills)
    knowledge = local_docs + canonical_org

    categories = {
        "entry map": score([d for d in docs if d == "AGENTS.md" or d.endswith("agent-index.md")]),
        "knowledge": score(knowledge),
        "skills": score(skills),
        "validation": score(test_files) + score(make or list(scripts)),
        "ci": score(ci),
        "guardrails": score(guardrails),
    }
    possible = 7
    total = sum(categories.values())

    print(f"# Agent Harness Audit: {root}")
    print()
    print(f"Score: {total}/{possible}")
    print()

    print("## Category Scores")
    for key, value in categories.items():
        print(f"- {key}: {value}")
    print()

    print_list("Agent Entry And Docs", docs)
    print_list("Local Knowledge Docs", local_docs)
    print_list("Canonical Knowledge", canonical_org)
    print_list("Skills", skills)
    print_list("Validation Files", test_files)
    print_list("Make Targets", make)

    print("## Package Scripts")
    if scripts:
        for name, command in sorted(scripts.items()):
            print(f"- {name}: `{command}`")
    else:
        print("- none found")
    print()

    print_list("CI", ci)
    print_list("Guardrail Hints", guardrails)

    print("## Recommendations")
    if not any(d == "AGENTS.md" for d in docs):
        print("- Add a short AGENTS.md as a map to deeper repo docs.")
    if not any(d.endswith("agent-index.md") for d in docs) and not canonical_org:
        print("- Add docs/agent-index.md when the repo has several docs or skills.")
    if not knowledge:
        print("- Add architecture/testing/quality/security docs as repo-local sources of truth.")
    if "README.org" in docs and not canonical_org:
        print("- If README.org is canonical, ensure AGENTS.md points agents to a structure-first Org query skill such as oq.")
    if not skills:
        print("- Add task-specific skills for repeated workflows.")
    if not (make or scripts):
        print("- Add explicit validation commands, for example make test or package scripts.")
    if not guardrails:
        print("- Add mechanical guardrails: pre-commit, lint hooks, sandbox policy, or edit hooks.")
    if not ci:
        print("- Add CI workflows for validation outside the agent session.")
    if total == possible:
        print("- Harness surface looks complete at this scan depth. Audit error messages and evals next.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
