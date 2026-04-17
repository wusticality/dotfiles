---
name: x-github-pr
description: Force-push current branch and create or update its GitHub PR
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Confirm `gh` is authenticated by running `gh auth status`. If it reports that the user is not
   logged in, stop and tell the user to run `gh auth login` before retrying.
2. Check the current branch with `git branch --show-current`. If the branch is `main` or `master`,
   stop and tell the user — this skill refuses to run on main branches.
3. Determine the base branch (prefer `main`, fall back to `master` if `main` does not exist) and
   find the merge base: `git merge-base HEAD <base>`.
4. Count the commits on this branch: `git rev-list --count <merge-base>..HEAD`. If 0, stop and
   tell the user there is nothing to push.
5. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
6. Build the commit-message-style text the PR will mirror:
   - If exactly 1 commit on the branch: use `git log -1 --format=%B HEAD` verbatim.
   - If multiple commits: run `git diff <merge-base>..HEAD` and write a fresh message from
     scratch, following the rules from step 5 (subject line on the first line, blank line, then
     body starting with "This commit [does X].").
7. The PR title is the subject line of that message (the first line). The PR body is everything
   after the blank line that follows the subject. Both come straight from the message — do not
   rewrite them.
8. Force-push the branch: `git push --force-with-lease origin HEAD`. If the push is rejected, stop
   and report the error — do not retry with `--force`.
9. Check for an existing PR:
   `gh pr list --head "$(git branch --show-current)" --state open --json number,url --jq '.[0]'`.
10. If a PR exists, update it with `gh pr edit <number> --title <title> --body-file -` piping the
    body in. Otherwise create one with `gh pr create --title <title> --body-file -`. For passing
    multi-line titles or bodies inline, use a quoted heredoc: `"$(cat <<'EOF' ... EOF)"`. Because
    the heredoc is quoted, do not escape backticks or `$` inside — backslashes come through
    literally.
11. Print the PR URL as the final line so the terminal renders it as a clickable link.
