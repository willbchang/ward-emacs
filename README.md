# Teem Emacs

**T**ext **E**diting within **E**macs for **M**acOS.



## Introduction

- **Shortcuts**: MacOS Native keybindings for **Text Editing** and **Window Management** that you already learned, or you can learn now.
- **Vim Emulator**: [evil-mode](https://github.com/emacs-evil/evil)is an expected vim emulator, with a bunch of useful vim plugins.
- **Markup Language**: Providing a WYSIWYG like experience for [org-mode](https://orgmode.org/)(Building), [markdown](https://daringfireball.net/projects/markdown/)(Coming soon) and [Latex](https://www.latex-project.org/)(Coming soon).
- **Git Interface**: [magit](https://magit.vc/) is a complete text-based user interface to Git.
- **Self Documentation**: Press <kbd>alt + x</kbd> and type `describe-` then press <kbd>TAB</kbd>, you can find almost anything in Emacs by yourself.
- **Emacs is highly customizable.**

**Notes**:
- The configuration is in [config.org](config.org).
- I only use Emacs as a Text Editor. [JetBrains](https://www.jetbrains.com/products/) provides the best IDE for most programming languages, if you are seeking. EAP versions are free to use.
- I mainly support the version that I'm using(currently is [emacs-plus@28 native comp](https://github.com/d12frosted/homebrew-emacs-plus)), other versions should work but I'm lack to make it compatible, you can get help from search engine and emacs communities ([reddit/emacs](https://www.reddit.com/r/emacs/), [Emacs StackExchange](https://emacs.stackexchange.com/), [emacs-china](https://emacs-china.org/)).
- Do not expect too much, and you'll be happy.


## Installation

Install to `XDG_CONFIG_HOME` (default to `~/.config`).

```bash
git clone --depth=1 https://github.com/willbchang/teem-emacs.git ~/.config/emacs
```


## Shortcuts
### Text Editing

| Editing Text                        | Features                            | Status   |
|:------------------------------------|:------------------------------------|:---------|
| <kbd>Command + C</kbd>              | Copy selected text                  | MacOS    |
| <kbd>Command + V</kbd>              | Paste text from clipboard           | MacOS    |
| <kbd>Command + X</kbd>              | Cut selected text                   | MacOS    |
| <kbd>Command + Z</kbd>              | Undo text change                    | MacOS    |
| <kbd>Command + Shift + Z</kbd>      | Redo text change                    | MacOS    |
| <kbd>Command + A</kbd>              | Select all text                     | MacOS    |
| <kbd>Command + F</kbd>              | Search text                         | MacOS    |
| <kbd>Option  + Delete</kbd>         | Delete a word back                  | MacOS    |
| <kbd>Command + Delete</kbd>         | Delete to line start                | MacOS    |
| <kbd>Command + Shift + Delete</kbd> | Delete entire line                  | Personal |
| <kbd>Command + /</kbd>              | Comment or uncomment line(s)        | MacOS    |
| <kbd>Shift + Alt + ↑</kbd>          | Swap current line and previous line | Emacs    |
| <kbd>Shift + Alt + ↓</kbd>          | Swap current line and next line     | Emacs    |


| Moving Cursor          | Features                          | Status |
|:-----------------------|:----------------------------------|:-------|
| <kbd>Command + ↑</kbd> | Move to the top of the file.      | MacOS  |
| <kbd>Command + ↓</kbd> | Move to the bottom of the file    | MacOS  |
| <kbd>Command + ←</kbd> | Move to the beginning of the line | MacOS  |
| <kbd>Command + →</kbd> | Move to the end of the line       | MacOS  |
| <kbd>Command + L</kbd> | Go to line                        | MacOS  |

| Selecting Text                 | Features                            | Status |
|:-------------------------------|:------------------------------------|:-------|
| <kbd>Shift + ↑</kbd>           | Select one line up                  | MacOS  |
| <kbd>Shift + ↓</kbd>           | Select one line down                | MacOS  |
| <kbd>Shift + ←</kbd>           | Select one character left           | MacOS  |
| <kbd>Shift + →</kbd>           | Select one character right          | MacOS  |
| <kbd>Shift + Alt + ←</kbd>     | Select one word left                | MacOS  |
| <kbd>Shift + Alt + →</kbd>     | Select one word right               | MacOS  |
| <kbd>Shift + Command + ↑</kbd> | Select to ttop of the file          | MacOS  |
| <kbd>Shift + Command + ↓</kbd> | Select to bottom of the file        | MacOS  |
| <kbd>Shift + Command + ←</kbd> | Select to the beginning of the line | MacOS  |
| <kbd>Shift + Command + →</kbd> | Select to the end of the line       | MacOS  |

### Window Management
TODO...

## Credit

It learns from:
- [redguardtoo/emacs.d](https://github.com/redguardtoo/emacs.d)
- [hrs/dotfiles](https://github.com/hrs/dotfiles)
- [hlissner/doom-emacs](https://github.com/hlissner/doom-emacs)


## LICENSE

[GPL-3.0 License](./LICENSE)
