---
name: x-commit
description: Commit staged changes with a generated commit message
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Run `git diff --cached` to understand the staged changes. If there is nothing staged, stop and
   tell the user.
2. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
3. Write a commit message following those rules.
4. Create the commit using `git commit -m` with the generated message. For multi-line messages use
   a quoted heredoc: `git commit -m "$(cat <<'EOF' ... EOF)"`. Because the heredoc is quoted, do
   not escape backticks or `$` inside the message — backslashes come through literally.
5. Run `git log -1` to confirm the commit was created.
