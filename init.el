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

;; Make frame transparency overridable
;;  (defvar efs/frame-transparency '(90 . 90))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Set frame transparency
;;  (set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;  (add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))

;; maximize frame
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package general
    :after evil
    :config
    (general-create-definer efs/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (efs/leader-keys
      "t"  '(:ignore t :which-key "toggles")
      "tt" '(counsel-load-theme :which-key "choose theme")
      "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package hydra
   :defer t)

 (defhydra hydra-text-scale (:timeout 4)
   "scale text"
   ("j" text-scale-increase "in")
   ("k" text-scale-decrease "out")
   ("f" nil "finished" :exit t))

; (efs/leader-keys
;   "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

    (setq org-agenda-files
        '("~/.emacs.d/OrgFiles/Tasks.org")))
;          "~/.emacs.d/OrgFiles/Habits.org"
;          "~/.emacs.d/OrgFiles/Birthdays.org")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("W" "Work Tasks" tags-todo "+work-email")

  ;; Low-effort next actions
  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
   ((org-agenda-overriding-header "Low Effort Tasks")
    (org-agenda-max-todos 20)
    (org-agenda-files org-agenda-files)))

  ("w" "Workflow Status"
   ((todo "WAIT"
          ((org-agenda-overriding-header "Waiting on External")
           (org-agenda-files org-agenda-files)))
    (todo "REVIEW"
          ((org-agenda-overriding-header "In Review")
           (org-agenda-files org-agenda-files)))
    (todo "PLAN"
          ((org-agenda-overriding-header "In Planning")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "BACKLOG"
          ((org-agenda-overriding-header "Project Backlog")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "READY"
          ((org-agenda-overriding-header "Ready for Work")
           (org-agenda-files org-agenda-files)))
    (todo "ACTIVE"
          ((org-agenda-overriding-header "Active Projects")
           (org-agenda-files org-agenda-files)))
    (todo "COMPLETED"
          ((org-agenda-overriding-header "Completed Projects")
           (org-agenda-files org-agenda-files)))
    (todo "CANC"
          ((org-agenda-overriding-header "Cancelled Projects")
           (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp "~/.emacs.d/OrgFiles/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("jj" "Journal" entry
         (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)
    ("jm" "Meeting" entry
         (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

    ("w" "Workflows")
    ("we" "Checking Email" entry (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org" "Weight")
     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

(setq org-refile-targets
  '(("~/.emacs.d/OrgFiles/Archive.org" :maxlevel . 1)
    ("~/.emacs.d/OrgFiles/Tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Automatically tangle our Emacs.org config file when we save it
    (defun efs/org-babel-tangle-config ()
      (when (string-equal (file-name-directory (buffer-file-name))
                          (expand-file-name user-emacs-directory))
        ;; Dynamic scoping to the rescue
        (let ((org-confirm-babel-evaluate nil))
          (org-babel-tangle))))

    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

  (with-eval-after-load 'org
    (org-babel-do-load-languages
    'org-babel-load-languages
   '((emacs-lisp . t)
;;       (matlab . t)
;;       (julia . t)
       (python . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-confirm_babel-evaluate nil)

  (with-eval-after-load 'org
    ;; This is needed as of Org 9.2
    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("ma" . "src matlab")))

;;    (use-package jupyter)

;;  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
;;                                                      (:session . "jl")
;;                                                      (:kernel . "julia-1.6")))

(use-package command-log-mode)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 0)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
;  Use visual line motions even outside of visual-line-mode buffers
; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
 (evil-set-initial-state 'messages-buffer-mode 'normal)
 (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; simplified mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; I pick palenight below
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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

;; NOTE: Make sure to configure a GitHub token before using this package!
  ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
  ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo.gpg"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(setq matlab-indent-function-body t)  ; if you want function bodies indented
 (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
 (defun my-matlab-mode-hook ()
     (setq fill-column 76))              ; where auto-fill should wrap
 (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

 (defun my-matlab-shell-mode-hook ()
   '())
 (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

(use-package julia-mode)

(use-package julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; problem with flatpak, may not be necessary now
(setq julia-repl-executable-records
      '((default "julia")  ; having trouble finding it for some reason.
        (master "/opt/julia-1.6.0/bin/julia"))) ; give some help

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

;;  (use-package company
  ;;  :after lsp-mode
;;    :hook (lsp-mode . company-mode)
;;    :bind (:map company-active-map
;;           ("<tab>" . company-complete-selection))
;;          (:map lsp-mode-map
;;           ("<tab>" . company-indent-or-complete-common))
;;    :custom
;;    (company-minimum-prefix-length 1)
;;    (company-idle-delay 0.0))

;;  (use-package company-box
;;    :hook (company-mode . company-box-mode))

;;      (setq lsp-julia-package-dir nil)
;;      (setq lsp-julia-flags `("-J ~/.julia/languageserver.so"))
;;      (require 'lsp-julia) ;must come after this!

;;    (use-package lsp-julia
;;      :config
;;      (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
;;      (setq lsp-enable-folding t))

;;    (add-hook 'julia-mode-hook #'lsp-mode)

(use-package winum
  :config
(winum-mode))

(use-package ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(use-package winner
  :ensure nil
  :config
  (winner-mode))

(use-package buffer-move)

;;  (windmove-default-keybindings nil)
(global-set-key (kbd "C-M-b")  'windmove-left)
(global-set-key (kbd "C-M-f") 'windmove-right)
(global-set-key (kbd "C-M-p")    'windmove-up)
(global-set-key (kbd "C-M-n")  'windmove-down)
