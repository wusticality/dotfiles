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
2. Read `${CLAUDE_SKILL_DIR}/../docs/branch-prep.md` and follow every step in it. That guards
   against running on a main branch, builds the title and body, and pushes the branch only if
   the remote is not already up to date. Stop here if it says to stop.
3. Check for an existing MR for the current branch:
   `glab mr list --source-branch "$(git branch --show-current)" --opened --output json`.
   If one already exists, stop and tell the user — this skill only creates new MRs. Direct them
   to update the existing one manually or via the GitLab UI.
4. Create the MR with `glab mr create --assignee @me --title <title> --description <body> --yes`.
   Pass the title and description via quoted heredocs:
   `--title "$(cat <<'EOF' ... EOF)"` and `--description "$(cat <<'EOF' ... EOF)"`. Because the
   heredocs are quoted, do not escape backticks or `$` inside — backslashes come through
   literally. The `--yes` flag skips the interactive confirmation.
5. Print the MR URL as the final line so the terminal renders it as a clickable link.
