<div align="center">
    <img width="200" alt="Star Guardian Ward" src="https://user-images.githubusercontent.com/14329786/110733831-9cdb7a00-8261-11eb-9f15-041b70be54b1.png">
</div>


# Ward Emacs

Make Emacs the Plain Text Editor as a great Word Processor in MacOS.



## Introduction

- **Package Management**
- **User Interface**: Distraction Free.
- **Window Management**: MacOS keybindings.
- **Text Editing**: MacOS keybindings, Vim emulator([evil-mode](https://github.com/emacs-evil/evil)), multiple cursor,
- **Markup Language**: WYSIWYG like for [org-mode](https://orgmode.org/), gold experience with plain text table. Synatx highlight for [Markdown](https://daringfireball.net/projects/markdown/).
- **Terminal Emulator**: Like a native Terminal.
- **Version Control**: [magit](https://magit.vc/) is a complete text-based user interface to Git.
- **Chinese Optimization**: Correctly word wrap for long lines, auto switch input method for Evil mode, search text with first letter of Pinyin.

The configuration is in [config.org](config.org).

**Notes**:
- **Self Documentation**: Press <kbd>alt + x</kbd> and type `describe-` then press <kbd>TAB</kbd>, you can find almost anything in Emacs by yourself.
- **Emacs is more highly customizable than you think.**
- I only use Emacs as a Text Editor, no coding features will be added expect elisp. [JetBrains](https://www.jetbrains.com/products/) provides the best IDE for most programming languages, [EAP](https://www.jetbrains.com/resources/eap/) versions are free to use.
- I mainly support the version that I'm using(currently is [emacs-plus@28 native comp](https://github.com/d12frosted/homebrew-emacs-plus)), most of the code should work for other versions, but I don't have time to make them compatible. You can get help from search engine and the great emacs communities ([reddit/emacs](https://www.reddit.com/r/emacs/), [Emacs StackExchange](https://emacs.stackexchange.com/), [emacs-china](https://emacs-china.org/)).
- Do not expect too much, and you'll be happy.


## Installation

Install to `XDG_CONFIG_HOME` (default to `~/.config`).

```bash
git clone --depth=1 https://github.com/willbchang/teem-emacs.git ~/.config/emacs
```


## Shortcuts
### Text Editing

| Moving Cursor          | Features                          | Convention |
|:-----------------------|:----------------------------------|:-----------|
| <kbd>Command + ↑</kbd> | Move to the top of the file.      | MacOS      |
| <kbd>Command + ↓</kbd> | Move to the bottom of the file    | MacOS      |
| <kbd>Command + ←</kbd> | Move to the beginning of the line | MacOS      |
| <kbd>Command + →</kbd> | Move to the end of the line       | MacOS      |
| <kbd>Command + L</kbd> | Go to line                        | MacOS      |
| <kbd>Option + ←</kbd>  | Move to the previous word         | MacOS      |
| <kbd>Option + →</kbd>  | Move to the next word             | MacOS      |

| Selecting Text                 | Features                            | Convention |
|:-------------------------------|:------------------------------------|:-----------|
| <kbd>Command + A</kbd>         | Select all text                     | MacOS      |
| <kbd>Shift + ↑</kbd>           | Select one line up                  | MacOS      |
| <kbd>Shift + ↓</kbd>           | Select one line down                | MacOS      |
| <kbd>Shift + ←</kbd>           | Select one character left           | MacOS      |
| <kbd>Shift + →</kbd>           | Select one character right          | MacOS      |
| <kbd>Shift + Option + ←</kbd>  | Select one word left                | MacOS      |
| <kbd>Shift + Option + →</kbd>  | Select one word right               | MacOS      |
| <kbd>Shift + Command + ↑</kbd> | Select to ttop of the file          | MacOS      |
| <kbd>Shift + Command + ↓</kbd> | Select to bottom of the file        | MacOS      |
| <kbd>Shift + Command + ←</kbd> | Select to the beginning of the line | MacOS      |
| <kbd>Shift + Command + →</kbd> | Select to the end of the line       | MacOS      |



| Editing Text                        | Features                            | Convention |
|:------------------------------------|:------------------------------------|:-----------|
| <kbd>Command + C</kbd>              | Copy selected text                  | MacOS      |
| <kbd>Command + V</kbd>              | Paste text from clipboard           | MacOS      |
| <kbd>Command + X</kbd>              | Cut selected text                   | MacOS      |
| <kbd>Command + Z</kbd>              | Undo text change                    | MacOS      |
| <kbd>Command + Shift + Z</kbd>      | Redo text change                    | MacOS      |
| <kbd>Command + F</kbd>              | Search text                         | MacOS      |
| <kbd>Option  + Delete</kbd>         | Delete a word back                  | MacOS      |
| <kbd>Command + Delete</kbd>         | Delete to line start                | MacOS      |
| <kbd>Command + Shift + Delete</kbd> | Delete entire line                  | Personal   |
| <kbd>Command + /</kbd>              | Comment or uncomment line(s)        | MacOS      |
| <kbd>Shift + Option + ↑</kbd>       | Swap current line and previous line | Emacs      |
| <kbd>Shift + Option + ↓</kbd>       | Swap current line and next line     | Emacs      |





### Window Management
| Window                | Features             | Convention |
|-----------------------|----------------------|------------|
| ~Command + Shift + W~ | Close Current Window | macOS      |
| ~Command + Shift + =~ | Zoom in Window       | Personal   |
| ~Command + Shit + -~  | Zoom out Window      | Personal   |
| ~Command + Shit + 0~  | Reset Zoom Window    | Personal   |
| ~Command + N~         | Create New Window    | macOS      |
| ~Command + `~         | Move to next Window  | macOS      |
| ~Command + Q~         | Quit Emacs           | macOS      |



| Buffer        | Features              | Convention |
|---------------|-----------------------|------------|
| ~Command + W~ | Close Current Buffer  | macOS      |
| ~Command + [~ | Go to previous Buffer | macOS      |
| ~Command + ]~ | Go to next Buffer     | macOS      |
| ~Command + =~ | Zoom in Buffer        | macOS      |
| ~Command + -~ | Zoom out Buffer       | macOS      |
| ~Command + 0~ | Reset Zoom Buffer     | macOS      |
| ~Command + T~ | Create New Buffer     | macOS      |
| ~Command + S~ | Save Buffer           | macOS      |
| ~Command + R~ | Revert Buffer         | macOS      |
| ~Command + '~ | Move to next Buffer   | Emacs      |
| ~Command + ,~ | Open Preferences      | macOS      |



## Credit

It learns from:
- [redguardtoo/emacs.d](https://github.com/redguardtoo/emacs.d)
- [hrs/dotfiles](https://github.com/hrs/dotfiles)
- [hlissner/doom-emacs](https://github.com/hlissner/doom-emacs)
- [Practical Emacs Tutorial](http://ergoemacs.org/emacs/emacs.html)

## LICENSE

[GPL-3.0 License](./LICENSE)
