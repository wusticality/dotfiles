---
name: x-gitlab-mr
description: Push current branch and create a GitLab merge request assigned to me
disable-model-invocation: true
allowed-tools: Bash Read
---

1. Confirm `glab` is authenticated by running `glab auth status`. If it reports that the user is
   not logged in, stop and tell the user to run `glab auth login` before retrying. Do not pass
   `--hostname` — the host is pinned via the `GITLAB_HOST` environment variable in the user's
   shell config.
2. Check the current branch with `git branch --show-current`. If the branch is `main` or `master`,
   stop and tell the user — this skill refuses to run on main branches.
3. Determine the base branch (prefer `main`, fall back to `master` if `main` does not exist) and
   find the merge base: `git merge-base HEAD <base>`.
4. Count the commits on this branch: `git rev-list --count <merge-base>..HEAD`. If 0, stop and
   tell the user there is nothing to push.
5. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
6. Build the commit-message-style text the MR will mirror:
   - If exactly 1 commit on the branch: use `git log -1 --format=%B HEAD` verbatim.
   - If multiple commits: run `git diff <merge-base>..HEAD` and write a fresh message from
     scratch, following the rules from step 5 (subject line on the first line, blank line, then
     body starting with "This commit [does X].").
7. The MR title is the subject line of that message (the first line). The MR description is
   everything after the blank line that follows the subject. Both come straight from the message
   — do not rewrite them.
8. Push the branch: `git push --force-with-lease -u origin HEAD`. If the push is rejected, stop
   and report the error — do not retry with `--force`.
9. Check for an existing MR for the current branch:
   `glab mr list --source-branch "$(git branch --show-current)" --opened --output json`.
   If one already exists, stop and tell the user — this skill only creates new MRs. Direct them
   to update the existing one manually or via the GitLab UI.
10. Create the MR with `glab mr create --assignee @me --title <title> --description <body> --yes`.
    Pass the title and description via quoted heredocs:
    `--title "$(cat <<'EOF' ... EOF)"` and `--description "$(cat <<'EOF' ... EOF)"`. Because the
    heredocs are quoted, do not escape backticks or `$` inside — backslashes come through
    literally. The `--yes` flag skips the interactive confirmation.
11. Print the MR URL as the final line so the terminal renders it as a clickable link.
