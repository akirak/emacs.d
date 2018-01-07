# emacs.d

[![Build Status](https://travis-ci.org/akirak/emacs.d.svg?branch=master)](https://travis-ci.org/akirak/emacs.d)

This is an attempt to write an Emacs initialization file from scratch. I had been a long-time user of Vim, then switched to Spacemacs last year, and now I want to use Emacs without extra layers of Spacemacs. 

This configuration is not "production-ready" for now. That is, I am not using it for any serious purpose. I am currently developing this configuration using Spacemacs and trying it on [play](https://github.com/akirak/play.el). 

## Installation

Back up your ~/.emacs.d to somewhere:

    mv ~/.emacs.d ~/.emacs.d.bak

and clone the repository:

    git clone https://github.com/akirak/emacs.d.git ~/.emacs.d

You can also use [play.el](https://github.com/akirak/play.el) for trying this configuration. Install play and run `play-checkout` command in Emacs. Enter the repository URL [https://github.com/akirak/emacs.d](https://github.com/akirak/emacs.d.git) into the minibuffer. 

## Features

This list is going to expand soon. 

- use-package and straight.el
- general.el and which-key for keybinding
