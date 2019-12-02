# Reddup

A program to help keep things tidy.

This should be considered alpha quality. If you have any questions, feel free to
use issues or send an email.

## Overview

I like to keep my computer neat and organized: code changes committed, branches
pushed, downloads in the correct place, etc.

The command `reddup` helps me make this cleanup process easy to do, so that I do
it more often, and also know when I need to do it.

The idea is, you define locations/specifications for what you want to keep tidy
in `~/.reddup.yml`, and then run `reddup`. The command then reads the system and
reports anything that is out of place.

Reddup also includes an interactive mode (`-i`, inspired by `git add -p`)
which can be used to "clean up" the untidy things that have been found.

## Support

For a few reasons (which may be overcomable), `reddup` requires that it be on a
unix-like environment. Sorry, Windows users.

# Capabilities

What can `reddup` do specifically? Right now, it helps you keep directories and
git repositories nice and tidy.

## Directories (that should be empty)

Often, we have directories that are temporary storage places. For example, on my
mac, I consider `~/Desktop` and `~/Downloads` to be two such directories.

When configured with a location of type `inbox`, `reddup` looks for any files
inside the specified directory. In non-interactive mode, it will
will print these files for your inspection.

In interactive mode (with `-i`), `reddup` will interactively help you to handle
each file in a handled directory. Options include:

- Deleting
- Renaming
- Moving to a predefined list of destinations
- Running custom commands defined in the configuration file (WIP)
- Opening a shell at location to run ad-hoc commands

## Git Repositories

When configured with a location of type `git`, `reddup` checks the git
repository to ensure that it is "tidy". It checks that there are:

- no unstaged changes
- no untracked files
- no staged, uncommitted changes
- no unpushed branches

In non-interactive mode, it will will print these files for your inspection.

In interactive mode (with `-i`), `reddup` will interactively help you "reddup"
the repository.

Options include:

- Committing all changes (deleted, staged, unstaged, untracked).
- Push any branch that are unpushed to upstream.
- Run any ad-hoc commands entered.
- Running custom commands defined in the configuration file.

# Configuration

Reddup looks for its configuration file at `~/.reddup.yml`.

## Example

Here is the one I am using right now:

```
locations:
  - type: git
    location: ~/EF
  - type: git
    location: ~/Reference
  - type: git
    location: ~/Projects/*
  - type: inbox
    location: ~/Desktop
    ignored_files:
      - .DS_Store
  - type: inbox
    location: ~/Inbox
    ignored_files:
      - .DS_Store
handlers:
  inbox:
    commands:
      - name: (o)pen
        cmd: open "$FILE"
        key: o
      - name: open (e)nclosing dir
        cmd: open .
        key: e
    refile_dests:
      - name: (b)ooks
        char: b
        dir: ~/Nextcloud/books
      - name: (p)apers
        char: p
        dir: ~/Nextcloud/papers
      - name: p(r)ivate
        char: r
        dir: ~/Nextcloud/private
      - name: (f)unny
        char: f
        dir: ~/Nextcloud/funny
```

# Installation

Currently, there is no binary installation available.

- Install Stack: https://docs.haskellstack.org/en/stable/README/

- Clone or download repo.

- `stack build` to build the project.

- `stack exec reddup` to run.

- To use it from anywhere on the system:

  - Install it to `~/.local/bin` by running `stack install`.

  - Add that that to your path, if it is not already added, (e.g. add
    `export PATH="~/.local/bin:$PATH"` to the end of your `~/.bashrc` and/or
    `~/.bash_profile` files).

# Enhancements

Have any requests for functionality? Please file an issue. PRs welcome.
