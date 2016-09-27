
## Let VIM do the Typing
> at Boston VIM Typing, thoughtbot
> by George Brocklehurst @georgebrock

* directly to paste from a buffer, `CTRL + r` then buffer-name

* get last i-mode inserted text in new insert mode `CTRL + a`

* completion with previous occuring `CTRL + p`

* completion with next occuring `CTRL + n`

* CTRL+x puts in a sub-mode completion, `CTRL + x + p`
> repeating this w/o any interrupt will keep completing following elements of same edit.

* ctags completion with `CTRL + x ; CTRL + [`

* completion for file-path `CTRL + x ; CTRL + f`

* completion mode for a line than word, `CTRL + x + l`; context-aware line completion

* if syntax is on, omni-completion can be used `CTRL + x ; CTRL + o`

* jump around tag, `CTRL + t`

* `:set complete`, shows order for auto-completion
> . ~ current buffer
> w ~ windows
> b ~ buffer
> u ~ all active buffers
> t ~ text file
> i ~ included files
> kspell ~ spell-checker only when spelling is enabled

---
---
