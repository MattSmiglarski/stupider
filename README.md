stupider.el
=============
Create speed reading frames in Emacs.

Inspired by spray-mode.el.

Usage
-----

Add stupider.el to your `load path`, `require` the library and then execute `stupider-speed-read` on your favourite buffer.

    (add-to-list 'load-path "/home/zz/src/elisp/stupider/")
    (require 'stupider)
    (stupider-speed-read (current-buffer))

It can also be called interactively, with `M-x stupider-speed-read`.

Keys
-----
During execution, the following keys have effect.

Key | Effect
--- | --- 
q | quit
SPC | Pause / Unpause
up | Speed Up
down | Slow Down
n | Nudge, next word

Notes
------
See https://github.com/ian-kelling/spray for a mode based implemention.
