# runemacs
A vanilla Emacs 27 configuration following https://github.com/daviwil/emacs-from-scratch with some additions for scientific computing

* Installation

`git clone https://github.com/ggebbie/runemacs` 

Install in some usual places, like:
`cp -r runemacs ~/.config/emacs` 
or
`cp -r runemacs ~/.emacs.d` 

Make sure not to overwrite .emacs.d if you need to save your old configuration. Remove `~/.emacs` so that it doesn't interrupt the initialization. 

Invoke emacs and open Emacs.org

`M-x org-babel-tangle`

to create the ~/.emacs.d/init.el file. Close and open emacs and you should have the new configuration. It will download many packages, including those from MELPA, the first time, but startup will be faster on subsequent occasions. 

The use of the vterm in various packages requires compilation in your system. Emacs will ask for confirmation a few times during the first startup.

For ubuntu users, the first startup  will not be complete unless a few fonts packages are installed on the system. Installation can be done using:
`sudo apt install fonts-firacode` \
`sudo apt install fonts-cantarell`

The emacs directory editor "dired" uses some fancy icons. It appears that they need to be manually installed. In an open emacs session, invoke the command:
`M-x all-the-icons-install-fonts`

* Keyboard suggestion

Emacs frequently relies upon the Control key. I recommend using the left Control with right-hand keys, and vice versa. The left Control is more ergonomic if remapped to the Caps Lock key. Ideally, shift-CapsLock could retain the CapsLock function.

* Evil mode

This hand-rolled configuration of Emacs uses the default "vanilla" keybindings. These keybindings have been called many things including "insane." You can use vi modal editing keybindings in emacs by invoking evil mode \\
`M-x evil-mode` \\
Alternatively, change to evil mode by default by changing Emacs.org to `(evil-mode 1)`.

* Julia 

`julia-snail` is the recommended Julia IDE. 

`julia-repl` with the vterm backend is also installed. `julia-repl` minor mode commands still work as long as they don't conflict with `julia-snail` commands.

* Python 

A python IDE using `lsp-mode` is set up. It requires system installation of `pyls` with the command \\
`pip install --user "python-language-server[all]"`

Open a python file. Then open an inferior python shell using `M-x run-python` or maybe even `M-x python`. Use commands like `M-x python-shell-send-region` to send code from the python file to the REPL, although there really should be some better shortcuts.
Use `M-x treemacs-symbols` to see a workspace dashboard. You should see a breadcrumb list in the header as well. Linting is on and will detect syntax errors. Code completion and function signatures should work although they might be slow due to pyls. Debugging also available with `dap-mode`. More info available at https://www.youtube.com/watch?v=jPXIP46BnNA. 

* R

Use `ess` to edit R files. The `ess-mode` interacts poorly with `doom-modeline` such that the buffer name disappears in the mode line. Use `M-x R` to open an R REPL. R needs to be independently installed on your system.  


* Things to modify to your preference

Org files are hard coded to go at ~/OrgFiles. Email information should be changed to your preference.
