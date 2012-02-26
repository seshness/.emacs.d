Here lies my Emacs configuration: settings, helpers, fancy functions, etc.

To be able to manage them sanely, I long ago deleted my .emacs file and chose to use a .emacs.d folder instead.
Emacs looks inside ~/.emacs.d for init.el, which I use somewhat like a header file.

Warning: I think I ought to use `provide`/`require` instead of `load`. This rewrite has been pending for a while..

## How to use

```sh
$ git clone git://github.com/seshness/.emacs.d.git
$ cd .emacs.d
$ git submodule update --init
```

I've used git submodules in this repository. They're like pointers to other git repos, allowing you to nest one repository inside another.

## Credits

This emacs customization set is sourced from many locations,
including but not limited to:

   * The UC Berkeley CS 61A default .emacs file

   * https://github.com/purcell/emacs.d/

   * Ahmed Owainati's .emacs file

   * http://www.cs.berkeley.edu/~prmohan/emacs/

   * CS 164 .emacs class master

   * The wealth of Emacs users at Facebook

   * https://github.com/overtone/live-coding-emacs
