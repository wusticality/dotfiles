# Commit Message Format

These rules apply to commit messages and equally to MR / PR descriptions.

## What it's for

Say *what the change does and why*, plainly and briefly - never *how* it's implemented. The diff
is the source of truth for the how; don't narrate or duplicate it.

- Document the behavior or outcome, not the implementation.
- Do NOT mention code, internals, or specific mechanics unless they're genuinely needed to
  understand the change.
- Do NOT list tests added, files touched, functions or types changed, or imports updated.
- A reader should grasp what the change accomplishes from the message alone.
- Name a specific symbol, file, or flag only when it's load-bearing for understanding.

## Length

Be terse. Write the shortest message that conveys what changed and why, then stop. The default is
short; length must be earned.

- Most changes: subject alone, or subject + one sentence.
- Add a sentence only for the *why*, and only when it isn't obvious from the subject.
- A body is optional - omit it when the subject already says everything.
- Reserve a short paragraph (or bullets) for genuinely involved changes with distinct parts.
- Never explain mechanics, restate the subject, or pad to look thorough. When in doubt, cut.

## Structure

- **Subject**: title-style headline, ≤ 68 chars, capitalized, no trailing punctuation. Reads as a
  title, not a sentence (`Add git commit and PR workflow skills`, not `This commit adds...`).
- **Body** (only if it adds something the subject can't): blank line, then state what it does -
  `This commit [does X].` for a commit, `This MR [does X].` for an MR - plus the *why* if it isn't
  obvious. Keep it tight.
- Hard-wrap body lines at 100 chars (raw text, not visual).
- Bullets only for genuinely distinct parts that prose can't carry cleanly - never to enumerate
  the diff. A bullet that restates a code change ("Renames X to Y", "Moves A to B") gets cut.

## Style

- Conversational, complete sentences, proper punctuation.
- No `#` or `##` headings. `###` for sections, `####` only if absolutely necessary.
- ` - ` not ` — `.
- `x / y` not `x/y` for inline lists.
- Inline backticks for types, identifiers, and paths.
