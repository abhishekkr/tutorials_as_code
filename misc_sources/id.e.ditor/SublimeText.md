# Sublime Text 2
*linux specific*

### Quick Tips Revision

> quick code tips
> * Ctrl + Alt + 'p'        : quickly switch project
> * Ctrl + 'p'              : quickly browse a file
> * Ctrl + <SPACE>          : auto-complete suggestion
> * Ctrl + Shift + '/'      : (un)comment current line
> * Ctrl + 'g'              : Goto a line by number
> * Ctrl + 'r'              : Goto a method/symbol by name
> * Ctrl + 'm'              : Goto next block

> word/text selection tips
> * Ctrl + 'd'              : Repeat quick-select next
> * Ctrl + 'u'              : Undo previous quick-select
> * Ctrl + 'k','d'          : Skip next quick-select and quick-select next

> block selection tips
> * Ctrl + 'l'              : Expand selection to Line
> * Ctrl + Shift + <SPACE>  : Expand selection to Scope
> * Ctrl + Shift + 'm'      : Expand selection to Brackets
> * Ctrl + Shift + 'j'      : Expand selection to Indentation
> * Ctrl + Shift + 'a'      : Expand selection to Tag
> * Ctrl + Shift + 'l'      : Add cursor to each line of selected text block

> text/block move/replace tips
> * Ctrl + 'j'                : join lines
> * Ctrl + Shift + 'd'        : duplicate lines
> * Ctrl + Shift + 'v'        : paste + indent
> * Ctrl + 'k','u'            : uppercase selected/cursor-ed text
> * Ctrl + 'k','l'            : lowercase selected/cursor-ed text
> * Ctrl + Shift + <UP|DOWN>  : Move selected text block up/down the other lines

> sublime behavior tips
> * Ctrl + Shift + 'p'      : sublime prompt
> * Ctrl + 'k','b'          : toggle side-bar

> globally common/basic stuff for just-in-case days
> * Ctrl + 'n'              : open new tab
> * Ctrl + Shift + 'n'      : open new tab in new instance
> * Ctrl + 'w'              : exit current tab
> * Ctrl + Shift + 'w'      : exit current instance
> * Ctrl + 'o'              : browse-open file
> * Ctrl + 's'              : save opened file
> * Ctrl + Shift + 's'      : save-as opened file


### Power from Packages

* Package Control
[https://packagecontrol.io/installation](https://packagecontrol.io/installation)
> * Click the Preferences > Browse Packages… menu
> * Browse up a folder and then into the Installed Packages/ folder
> * Download Package Control.sublime-package and copy it into the Installed Packages/ directory
> * Restart Sublime Text
>
> * Can be used now as 'Ctrl + Shift + P' >> 'Package Control:{{required-task}}'
```
cd "${HOME}/.config/sublime-text-2/Installed Packages"
curl -LkO https://packagecontrol.io/Package%20Control.sublime-package
```

---

* Fetch Text from Internet
[https://github.com/weslly/Nettuts-Fetch](https://github.com/weslly/Nettuts-Fetch)
> * To Install: 'Ctrl + Shift + P' >> 'Package Control: Install Package' >> 'Nettuts+ Fetch'

> * To Use
> ```
> To Fetch a file from www:               'Shift + Ctrl + P' >> 'Fetch: File'
> To manage names=url to be fetched:      'Shift + Ctrl + P' >> 'Fetch: Manage'
> Fetch entire package mapped to a name:  'Shift + Ctrl + P' >> 'Fetch: Package'
> ```

---

*  TidyTabs not worked on for some time
[https://github.com/bradleyboy/TidyTabs-Sublime](https://github.com/bradleyboy/TidyTabs-Sublime)
> * To Install: 'Ctrl + Shift + P' >> 'Package Control: Install Package' >> 'TidyTabs'

> * removes all older tabs not worked upon by 'Ctrl + Alt + w', default is inactive for 30min I guess

---

* Emmet
[https://github.com/sergeche/emmet-sublime](https://github.com/sergeche/emmet-sublime)

> * snippet shortcut feature by completion from mapped symbolic-text

---

