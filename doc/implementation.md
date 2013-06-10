---
layout: default
title: Director Musices - Implementation details
---

# Implementation Details

The source can be found in *src/director_musices* in the root
directory. 

The function *director-musices* inside *core.clj* is the entry for the program. Arguments to this function is simply a list of command line arguments that should be sent to the program.

Below is a description of the other files in this directory.

**cli.clj** parses the command line arguments sent to the *director-musices* function. After *cli/parse-args* is called with the arguments, the options can be accessed through the *global/get-arg* function.


**global.clj** contains global variables.

**logging.clj** sets up the logging library as well as the log window. Logging in the program should not be done using this namespace in any way - it's done by importing *taoensso.core* and calling *warn*, *error* etc from that namespace.

**main.clj** is the main entry when director musices is bundled in a jar. It's not really important to know what exactly *main.clj* does, see *core/director-musices* as the main entry instead.

**menu.clj** defines all menus.

**player.clj** for playing midi files.

**util.clj** utility functions usable in more than one namespace.

## common_lisp

The directory *src/director_musices/common_lisp* contains files that interact with common lisp.

**command_line.clj** creates the repl found in *Help->Command Line*.

**glue.clj** contains director-musices specific abcl interactions. The function *init-dm* is responsible for loading all dm files.

**interpreter.clj** contains the base for interacting with abcl.

## rulepalette

The directory *src/director_musices/rulepalette* contains everything that is necessary for the handling of rulepalettes.

## score

The directory *src/director_musices/score* contains everything that is necessary for the handling of scores. This includes the mixer.
