---
name: x-squash
description: Squash all commits on the current branch into one and regenerate the commit message
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Check the current branch with `git branch --show-current`. If the branch is `main` or `master`,
   stop and tell the user — this skill refuses to run on main branches.
2. Determine the base branch (usually `main`, fall back to `master` if `main` does not exist) and
   find the merge base: `git merge-base HEAD <base>`.
3. Count the commits to be squashed: `git rev-list --count <merge-base>..HEAD`. If the count is 0
   or 1, stop and tell the user there is nothing to squash.
4. Refuse if there are staged changes. Run `git diff --cached --quiet`; if it exits non-zero, stop
   and tell the user to commit, unstage, or stash the staged changes first. This is a safety guard
   — soft-resetting with staged changes would silently fold them into the squash commit. Unstaged
   changes are left alone by the squash and are fine.
5. Squash by soft-resetting to the merge base and creating a single new commit:
   - Save the diff summary first: `git diff <merge-base>..HEAD` so you can write a good message.
   - Run `git reset --soft <merge-base>` to collapse all commits while keeping the changes staged.
6. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
7. Write a commit message that describes the combined squashed changes following those rules.
8. Create the squashed commit with `git commit -m`. For multi-line messages use a quoted heredoc:
   `git commit -m "$(cat <<'EOF' ... EOF)"`. Because the heredoc is quoted, do not escape backticks
   or `$` inside the message — backslashes come through literally.
9. Run `git log --oneline <merge-base>..HEAD` to confirm exactly one commit exists on the branch.
