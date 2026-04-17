---
name: x-amend
description: Amend HEAD with staged changes and a regenerated commit message
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Run `git log -1` to see the current HEAD commit.
2. Run `git diff --cached` to see any staged changes that will be folded in, and `git show HEAD`
   to see what is already in the commit.
3. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
4. Write a new commit message that describes the combined contents (HEAD + any staged changes)
   following those rules.
5. Amend the commit using `git commit --amend -m` with the new message. For multi-line messages
   use a quoted heredoc: `git commit --amend -m "$(cat <<'EOF' ... EOF)"`. Because the heredoc is
   quoted, do not escape backticks or `$` inside the message — backslashes come through literally.
   If nothing is staged, still amend to update the message.
6. Run `git log -1` to confirm the amend succeeded.
