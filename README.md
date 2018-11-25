# What

posplyu is a sleep tracker for X11, using XScreenSaver extension and manual input, motivated by http://super-memory.com/articles/sleep.htm and designed to facilitate transitioning to the free running sleep regiment while living mostly with a GNU/Linux system handy.

# Installation

To install, ask your package manager to bring you `cabal-install`, clone this repo and call `cabal update && cabal install` in it.

# Usage

Put `posplyu &` in your `~/.xsession` or something to start it with your X session. Use the various command line args to query and edit the collected data. The argument style was inspired by `crontab` program.
