# Branch prep

Shared steps for the `x-github-pr` and `x-gitlab-mr` skills. Everything here is pure git —
nothing in this file touches `gh` or `glab`. The calling skill handles forge authentication
before running these steps, and handles creating or updating the PR/MR after them.

Throughout, "change description" means the commit-message-style text built in step 5 below.
The calling skill turns that text into a title and a body.

1. Check the current branch with `git branch --show-current`. If the branch is `main` or
   `master`, stop and tell the user — these skills refuse to run on main branches.
2. Determine the base branch (prefer `main`, fall back to `master` if `main` does not exist)
   and find the merge base: `git merge-base HEAD <base>`.
3. Count the commits on this branch: `git rev-list --count <merge-base>..HEAD`. If 0, stop and
   tell the user there is nothing to push.
4. Read `${CLAUDE_SKILL_DIR}/../docs/commit-message.md` for formatting rules.
5. Build the change description:
   - If exactly 1 commit on the branch: use `git log -1 --format=%B HEAD` verbatim.
   - If multiple commits: run `git diff <merge-base>..HEAD` and write a fresh message from
     scratch, following the rules from step 4 (subject line on the first line, blank line, then
     body starting with "This commit [does X].").
   The title is the subject line (the first line). The body is everything after the blank line
   that follows the subject. Both come straight from the message — do not rewrite them.
6. Push the branch, but only if it is actually needed. First check whether the remote already
   matches HEAD: `git rev-parse HEAD` and `git rev-parse "origin/$(git branch --show-current)"`.
   - If the remote ref exists and matches HEAD, the branch is already pushed — skip the push
     entirely. Do not force-push a branch that is already up to date. If the branch has no
     upstream set but the ref matches, set it without pushing:
     `git branch --set-upstream-to "origin/$(git branch --show-current)"`.
   - Otherwise push: `git push --force-with-lease -u origin HEAD`. If the push is rejected,
     stop and report the error — do not retry with `--force`.
