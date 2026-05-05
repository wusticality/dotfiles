# Commit Message Format

## Tone

- Conversational, not robotic. Write like you're explaining the change to a colleague.
- Lead with *why* the change exists, not *what* files were touched.
- Only include details when they add context that isn't obvious from the diff.
- The message conveys *meaning*, not a manifest. Don't itemize what happened ("moved this file",
  "renamed this type", "updated this import") - the diff already says that. Step back and describe
  the overall change in the words you'd use to a teammate over coffee.
- Name specific symbols, files, or types only when they're load-bearing for understanding. If the
  reader could follow the message without that name, leave it out.

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
- Bullets are fine when they group genuinely distinct points that prose would muddle. They are
  not fine as a checklist of what the diff did. A bullet that reads "Renames X to Y" or "Moves
  file A to B" or "Updates import of Z" is restating the diff - cut it.
- Good bullets carry meaning the diff can't: a behavior change, a deliberate non-change ("we
  intentionally left X alone because..."), a follow-up that's now unblocked, a subtle invariant.
- Keep it proportional to the change: a one-line fix gets a one-line message. A large refactor
  with truly distinct areas gets sections. Most commits land in between - subject, one summary
  sentence, a short motivation paragraph, done.

## Style

- Use complete sentences with proper punctuation.
- No `#` or `##` headings. `###` for sections, `####` if absolutely necessary.
- " - " not " — ".
- `x / y` not `x/y` for inline lists.
- Inline backticks for types, identifiers, and paths when referencing them.
- Bullets should convey meaning clearly — let them run to a natural length rather than forcing
  terseness.
- Dense, not verbose. Don't pad small changes to look bigger.
