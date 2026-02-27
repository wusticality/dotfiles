# Manage worktrees

## Overview

Create or remove multi-repo worktrees. All commands are in the user's PATH.

## Available commands

- `wt-loyalty <folder> <branch>` — create a loyalty worktree
- `wt-rewards <folder> <branch>` — create a rewards worktree
- `wt add <folder> <branch> <repo1> [repo2 ...]` — create a custom worktree with specific repos
- `wt remove <folder>` — remove a worktree and all its branches

## Behavior

When this command is invoked, walk the user through the following prompts in order:

1. Ask: **add or remove?**
2. If **remove**: ask for the folder name, then run `wt remove <folder>`.
3. If **add**:
   - Ask: **loyalty, rewards, or custom?**
   - Ask for the **folder name**
   - Ask for the **branch name**
   - If loyalty: run `wt-loyalty <folder> <branch>`
   - If rewards: run `wt-rewards <folder> <branch>`
   - If custom: ask for the **list of repos**, then run `wt add <folder> <branch> <repos...>`

Don't list the repos included in the worktree unless the user asks.
