# Commit Message Format

## Tone

- Conversational, not robotic. Write like you're explaining the change to a colleague.
- Lead with *why* the change exists, not *what* files were touched.
- Only include details when they add context that isn't obvious from the diff.

## Structure

- The first line is the **subject** — a short, title-style headline that appears in `git log
  --oneline`, GitHub's commit list, and similar tools. Keep it to 68 characters or fewer, start
  with a capital letter, and use no trailing punctuation. Read it as a title, not a sentence
  (e.g., `Add git commit and PR workflow skills`, not `This commit adds git commit and PR workflow
  skills.`).
- Follow the subject with a blank line, then the body.
- Hard-wrap every line of the body at 100 characters. Long sentences and bullets wrap onto
  continuation lines; the wrap is at the raw-text level, not just visual.
- Open the body with a one-sentence summary: "This commit [does X]." This sentence is the first
  line of the *body*, not the subject, so it can run as long as it needs to — it just wraps at 100
  characters like everything else in the body.
- After the summary sentence, add a short paragraph explaining the motivation or problem being
  solved.
- Use `###` headings to group by area only when there are genuinely distinct areas — don't force
  sections.
- Bullet points with imperative verbs (Adds, Removes, Fixes, etc.) for specifics.
- Keep it proportional to the change: a one-line fix gets a one-line message. A large refactor gets
  sections.

## Style

- Use complete sentences with proper punctuation.
- No `#` or `##` headings. `###` for sections, `####` if absolutely necessary.
- " - " not " — ".
- `x / y` not `x/y` for inline lists.
- Inline backticks for types, identifiers, and paths when referencing them.
- Bullets should convey meaning clearly — let them run to a natural length rather than forcing
  terseness.
- Dense, not verbose. Don't pad small changes to look bigger.
