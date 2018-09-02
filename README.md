# Reddup

A program to help keep things tidy.

I like to keep my computer neat and organized: code changes committed, branches pushed, downloads in the correct place, etc.

Reddup helps me make this cleanup process easy to do, so that I do it more often, and also know when I need to do it.

# example config
in file: `~/.reddup.yml`

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
```

# commands

stack build

stack exec reddup

stack install

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
