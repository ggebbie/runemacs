
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 130)

;(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; helpful for seeing commands on the side, maybe not necessary
(use-package command-log-mode)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
;;  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;  (evil-set-initial-state 'messages-buffer-mode 'normal)
;;  (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))


;; simplified mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;;(use-package general)

;; I pick palenight below
;(load-theme 'wombat)
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status)
;  :custom
					;  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package julia-mode)

(use-package julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; problem with flatpak, may not be necessary now
(setq julia-repl-executable-records
      '((default "julia")                  ; having trouble finding it for some reason.
        (master "/opt/julia-1.6.0/bin/julia"))) ; give some help

;; Error (use-package): Cannot load matlab-mode
(use-package matlab-mode)
(require 'matlab-mode)
(require 'matlab-load)

(setq matlab-indent-function-body t)  ; if you want function bodies indented
 (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
 (defun my-matlab-mode-hook ()
     (setq fill-column 76))              ; where auto-fill should wrap
 (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

 (defun my-matlab-shell-mode-hook ()
   '())
 (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

