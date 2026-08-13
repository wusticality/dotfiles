---
name: x-github-pr
description: Push current branch and create or update its GitHub PR
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Confirm `gh` is authenticated by running `gh auth status`. If it reports that the user is not
   logged in, stop and tell the user to run `gh auth login` before retrying.
2. Read `${CLAUDE_SKILL_DIR}/../docs/branch-prep.md` and follow every step in it. That guards
   against running on a main branch, builds the title and body, and pushes the branch only if
   the remote is not already up to date. Stop here if it says to stop.
3. Check for an existing PR:
   `gh pr list --head "$(git branch --show-current)" --state open --json number,url --jq '.[0]'`.
4. If a PR exists, update it with `gh pr edit <number> --title <title> --body-file -` piping the
   body in. Otherwise create one with `gh pr create --title <title> --body-file -`. For passing
   multi-line titles or bodies inline, use a quoted heredoc: `"$(cat <<'EOF' ... EOF)"`. Because
   the heredoc is quoted, do not escape backticks or `$` inside — backslashes come through
   literally.
5. Print the PR URL as the final line so the terminal renders it as a clickable link.
