# Audit current branch

## Overview

Review all changes on the current branch and any uncommitted staged changes to verify everything looks correct.

## Steps

1. Run `git diff` to check for unstaged changes. If any exist, flag them and ask the user whether to proceed with the audit or stop so they can deal with them first.
2. Run `git log --oneline @{u}..HEAD` to list all commits on the current branch since it diverged from its upstream tracking branch.
3. For each commit, run `git show {sha}` to inspect the full diff.
4. Run `git diff --cached` to check for any staged but uncommitted changes.
5. Review all changes against the context of the current conversation — verify that the changes are correct, complete, and consistent with what was discussed.
6. Report a summary: what looks good, and flag anything that seems off, missing, or unintended.
