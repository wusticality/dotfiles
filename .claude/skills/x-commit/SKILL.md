---
name: x-commit
description: Commit staged changes with a generated commit message
disable-model-invocation: true
allowed-tools: Bash Read
---

## Staging policy (hard rule)

Only commit what is **already staged**. Specifically, do not:

- run `git add`
- stage tracked modifications
- include untracked files

Unstaged and untracked changes must stay untouched.

The one exception: if the user's invoking message explicitly tells you to stage something
(e.g. "stage everything and commit", "add foo.py and commit"), follow that instruction and then
commit. A bare `/x-commit` is **not** such an instruction.

## Steps

1. Run `git diff --cached` to read the staged changes. If nothing is staged, stop and tell the
   user. Do not stage anything on their behalf.
2. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
3. Write the commit message per those rules.
4. Commit with `git commit -m`. For multi-line messages, use a quoted heredoc of the form
   `git commit -m "$(cat <<'EOF' ... EOF)"`. The heredoc is single-quoted, so backticks and `$`
   inside the message do **not** need escaping; backslashes come through literally.
5. Run `git log -1` to confirm the commit landed.
