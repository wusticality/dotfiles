# Git Safety

- NEVER commit (`git commit`, including `--amend`) without asking first. Leave changes
  uncommitted so the user can review them, then ask before committing. Approval is
  per-commit, not standing - "go ahead" or "give it a go" on a task is not approval to commit.
  Exception: committing is fine when running a commit skill (e.g. `/x-commit`) that the user
  invoked or explicitly told me to run. Never start such a skill on my own initiative.

- NEVER force push (`git push --force`, `--force-with-lease`, `-f`) without asking first.
  This applies to every branch, including personal / feature branches. Always confirm with
  the user before running any force-push variant, even if a previous force-push was approved
  earlier in the session - approval is per-action, not standing.
