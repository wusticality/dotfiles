# Commit Message Format

## Structure

- Open with a one-sentence summary: "This commit [does X]."
- Group changes by area using `###` headings (e.g., `### Input system`, `### Camera`).
- Under each heading, bullet points starting with imperative verbs (Adds, Removes, Moves, Fixes, Replaces, etc.).
- Use `####` sparingly for sub-headings if an area is large.
- Use inline backticks for types, identifiers, and crate names. Pluralize as `Foo`'s not `Foo`s.
- Optionally add 1-2 plain text paragraphs at the end for architecture context or rationale.

## Style

- No `#` or `##` headings. Use `###` for sections, `####` if absolutely necessary.
- Use " - " not " — ".
- Use `x / y` not `x/y` for inline lists.
- Keep bullets to one line each when possible.
- Focus on what the changes mean and do, not mechanical details like creating modules, moving files, or adding submodules.
- Naming key types is fine, but don't exhaustively list every type.
- Don't name crates or modules in prose unless it adds clarity.
- Dense, not verbose. Aim for 5-15 bullets total.
