# Reddup

A program to help keep things tidy. It is currently in active development. Some
of the claims made in this README may not yet be quite accurate, but many are.
If you'd like to use it, contact me via issues or email
(mccracken.joel@gmail.com) and I will let you know if it should work for your
use case.

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
- Running ad-hoc commands entered (TODO)
- Running custom commands defined in the configuration file (WIP)

## Git Repositories

When configured with a location of type `git`, `reddup` checks the git
repository to ensure that it is "tidy". It checks that there are:

- no unstaged changes
- no untracked files
- no staged, uncommitted changes
- no unpushed branches (TODO)
- no stashes (TODO, not even sure if I want this)

In non-interactive mode, it will will print these files for your inspection.

In interactive mode (with `-i`), `reddup` will interactively help you reddup
the repository.

Options include (all of these are TODO):

- Committing all changes (deleted, staged, unstaged, untracked) as a WIP commit (TODO)
- Push any branch that are unpushed.
- Push any branch that is unpushed to a new branch.
- Run any ad-hoc commands entered (TODO)
- Running custom commands defined in the configuration file (WIP)


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
  - type: inbox
    location: ~/Inbox
handlers:
  inbox:
    commands:
      - name: (o)pen
        string: o
        cmd: "open $FILENAME"
      - name: open (e)nclosing directory
        string: e
        cmd: open
    mv_destinations:
      - name: (b)ooks
        string: b
        directory: ~/Nextcloud/books
      -
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

# Enhancments

Have any requests for functionality? Please file an issue. PRs welcome.

# todo

- not/verbose
- handler, run predef command on file (e.g. open, etc)
- inbox dir
  - mv file to predefined location(s)
  - rename
  - delete
- git
  - unstaged changes
    - commit everything with text WIP
  - untracked files
    - add and commit as WIP
    - add to gitignore
  - push any branch
- also an exit for each of these
