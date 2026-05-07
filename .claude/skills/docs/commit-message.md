# Commit Message Format

## What this message is for

Pitch the change to another engineer: *what it does at a high level, and why*. The diff is the
source of truth for what changed - don't duplicate it.

- Explain intent and motivation, not mechanics.
- Do not enumerate renamed types, moved files, touched functions, or updated imports.
- A reader should grasp the change from the message alone, without opening the diff.
- Name a specific symbol, file, or type only when it's load-bearing for understanding.

## Length

Always include a body. Match its weight to the change.

- Trivial change → subject + one-sentence body.
- Normal change → subject + one or two sentences, plus a short motivation line if the why isn't
  obvious from the subject.
- Genuinely complex change → subject + summary sentence + short motivation paragraph. Sections /
  bullets only if there are truly distinct areas.
- Never pad to look thorough. When in doubt, cut.

## Structure

- **Subject**: title-style headline, ≤ 68 chars, capitalized, no trailing punctuation. Reads as a
  title, not a sentence (`Add git commit and PR workflow skills`, not `This commit adds...`).
- Blank line, then body (if any).
- Body opens with `This commit [does X].` then a short motivation paragraph if warranted.
- Hard-wrap body lines at 100 chars (raw text, not visual).
- Bullets only when they carry meaning the diff can't: a behavior change, a deliberate
  non-change, a now-unblocked follow-up, a subtle invariant. A bullet that restates the diff
  ("Renames X to Y", "Moves A to B") gets cut.

## Style

- Conversational, complete sentences, proper punctuation.
- No `#` or `##` headings. `###` for sections, `####` only if absolutely necessary.
- ` - ` not ` — `.
- `x / y` not `x/y` for inline lists.
- Inline backticks for types, identifiers, and paths.
