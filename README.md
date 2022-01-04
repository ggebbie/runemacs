# runemacs
Emacs configuration following daviwil/emacs-from-scratch

`git clone https://github.com/ggebbie/runemacs` 

`cp -r runemacs ~/.emacs.d` 

Make sure not to overwrite .emacs.d if you need to save your old configuration. 

Invoke emacs and open Emacs.org

`M-x org-babel-tangle`

to create the ~/.emacs.d/init.el file. Close and open emacs and you should have the new configuration. It will download many packages, including those from MELPA, the first time, but startup will be faster on subsequent occasions. 

The use of the vterm in various packages requires compilation in your system. Emacs will ask for confirmation a few times during the first startup.

For ubuntu users, the first startup  will not be complete unless a few fonts packages are installed on the system. Installation can be done using:
`sudo apt install fonts-firacode` \\
`sudo apt install fonts-cantarell`

The emacs directory editor "dired" uses some fancy icons. It appears that they need to be manually installed. In an open emacs session, invoke the command:
`M-x all-the-icons-install-fonts`
