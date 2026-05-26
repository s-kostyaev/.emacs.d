# Review Framework

Use this reference to keep review agents mechanical, evidence-based, and
consistent.

## Severity Labels

- **Critical**: Security exposure, data loss, irreversible destructive behavior,
  production outage, or broken release path.
- **High**: User-visible regression, incorrect core behavior, compatibility
  break, race condition, authorization bug, or missing migration/contract update.
- **Medium**: Edge-case bug, incomplete error handling, flaky behavior, notable
  performance risk, or risky code without tests.
- **Low**: Maintainability, clarity, or local robustness issue with limited
  blast radius.
- **Question**: A material uncertainty that cannot be resolved from the repo and
  changes review confidence.

## Review Dimensions

Choose only dimensions relevant to the diff.

### Correctness and Edge Cases

- Changed control flow, data transformations, state transitions, retries,
  concurrency, time zones, nullability, parsing, limits, and failure paths.
- Caller/callee assumptions and lifecycle ordering.
- Backward compatibility with existing stored data and user workflows.

### Tests and Regressions

- Tests cover the behavior changed, not just implementation details.
- Risky paths have negative, boundary, and regression cases.
- Fixtures and snapshots changed intentionally.
- Existing test commands are known, runnable, and scoped.

### Security, Privacy, and Secrets

- Authentication, authorization, tenant isolation, injection, SSRF, XSS, CSRF,
  unsafe deserialization, path traversal, command execution, and secret handling.
- Logs, analytics, errors, and traces do not expose sensitive data.
- New dependencies, scripts, and network calls are justified.

### API, Compatibility, and Data Contracts

- Public API signatures, wire formats, database schemas, migrations, feature
  flags, generated clients, and docs remain aligned.
- Defaults, deprecations, version negotiation, and rollback behavior are clear.
- Consumers and integration points are updated.

### Architecture and Maintainability

- The change fits existing module boundaries and abstractions.
- Shared behavior is not duplicated in a way that will drift.
- Error messages and recovery paths are useful to callers.
- Complexity is proportional to the problem.

### UI, Accessibility, and User Workflows

- User-visible text, loading/error/empty states, focus management, keyboard
  access, responsive layout, and contrast.
- State changes do not produce visual jumps, stale data, or broken navigation.
- Forms and destructive actions are reversible or confirmed where appropriate.

## Evidence Rules

Every finding should answer:

1. What changed?
2. What breaks or becomes risky?
3. Where is the exact evidence?
4. How can the author fix or validate it?

Use exact file and line references when possible. Prefer reasoning from code,
tests, schemas, command output, and docs over intuition. Do not include a finding
only because a pattern looks unfamiliar.

## Validation Loop

Use validation to reduce uncertainty:

- Run narrow tests for changed packages before broad suites.
- Use static checks, type checks, linters, schema generators, or build commands
  when they are the repository's source of truth.
- If validation is too expensive or unavailable, say so and explain the risk.
- Failed validation is not automatically a finding; connect it to the change.

## Subagent Harness Principles

- Keep assignments narrow and non-overlapping.
- Give each reviewer the review plan path and exact diff source.
- Store notes in files so synthesis does not depend on memory.
- Treat repeated review comments as candidates for future tests, linters, or
  repo-local instructions.
- If reviewers disagree, prefer the claim with stronger evidence or downgrade to
  an open question.

## Final Review Checklist

- Findings are first and sorted by severity.
- Each finding has a concrete impact and precise location.
- Duplicate symptoms are merged under the root cause.
- Speculation is removed or marked as a question.
- Validation commands and gaps are stated.
- The summary does not bury critical issues.
