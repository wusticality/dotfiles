---
name: x-commit-message
description: Generate a commit message for staged changes and copy to clipboard
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Run `git diff --cached` to understand the staged changes.
2. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
3. Write the commit message following those rules.
4. Output the message and also pipe it to `pbcopy` so it's on the clipboard.
