;; Set better defaults
(setq-default
 inhibit-startup-message t
 custom-file "~/.emacs.d/custom-file.el"
 fill-column 90
 frame-title-format (format "%s's Emacs" (capitalize user-login-name))
 create-lockfiles nil
 indent-tabs-mode nil
;; backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 auto-save-default nil
 enable-recursive-minibuffers t
 )
(fset 'yes-or-no-p 'y-or-n-p)
(when (eq system-type 'darwin)
  (setq
   mac-option-modifier nil
   mac-right-option-modifier nil
   mac-command-modifier 'meta))
(load-file custom-file)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      10 ; how many of the newest versions to keep
      kept-old-versions      5)

;; Fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'variable-pitch nil :font "Arial" :height 140 :weight 'regular)

;; (global-set-key (kbd "M-o") 'other-window) ;; Rebind key for changing window
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line numbers in mode line and buffer
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode t)

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-auto-revert-mode t) ;; Update changed files

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package manager
(require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.org/packages/") t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")))
;;    ("nognu" . "https://elpa.nognu.org/nognu/")
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep config directory clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
			url-history-file (expand-file-name "url/history" user-emacs-directory))
(use-package no-littering
						 :ensure t)


;; Completion
;;(use-package company
;;  :ensure t
;;  :bind (:map company-active-map
;;              ("C-n" . company-select-next)
;;              ("C-p" . company-select-previous))
;;  :config
;;  (setq company-idle-delay 0.3)
;;  (global-company-mode t))

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
         ("C-d" . ivy-reverse-i-serarch-kill))
  :config
  (ivy-mode 1))

;; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) ;; Defining a key binding for a specific mode map

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Doom modeline
;; NOTE: The first time this is loaded, you need to run:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 28)))

;; Color scheme
;;(use-package spacemacs-theme
;;  :config
;;  (setq spacemacs-theme-comment-bg nil)
;;  (setq spacemacs-theme-comment-italic t)
;;  (load-theme 'spacemacs-dark))

(use-package doom-themes
  :config
;;  (load-theme 'doom-gruvbox t)
;;  (load-theme 'doom-one t)
  (load-theme 'doom-dracula t)
 )

;; WhichKey
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Vim bindings (evil-mode)
;;(use-package evil
;;  :config
;;  (evil-mode 1))

;;(defun os/evil-hook ()
;;  (dolist (mode '(custom-mode
;;                  eshell-mode
;;                  git-rebase-mode
;;                  erc-mode
;;                  circe-server-mode
;;                  circe-chat-mode
;;                  circe-query-mode
;;                  sauron-mode
;;                  term-mode))
;;    (add-to-list 'evil-emacs-state-modes mode)))

(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

;; C-w for evil bindings
(use-package evil ;; C-z to toggle between evil and emacs mode
  :after undo-tree
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;;(setq evil-want-C-u-scroll t) ;; Rebind C-u universal argument
  (setq evil-want-C-i-jump nil) ;; Vim jumping keybinds disabled
  ;;  :hook (evil-mode . os/evil-hook)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General
(use-package general
  :config
;;  (general-define-key
;;    "M-o" 'other-window
;;    "C-M-j" 'counsel-switch-buffer)

  (general-create-definer os/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (os/leader-keys
    "t" '(:ignore t :which-key "Toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(os/leader-keys
  "ts" 'hydra-text-scale/body :which-key "scale text")

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-proect-search-path '("~/Projects/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(os/leader-keys
  "p" '(:ignore t :which-key "Projectile")
  "ps" '(counsel-projectile-rg :which-key "Search string")
  "pf" '(counsel-projectile-find-file :which-key "Find file")
  "po" '(counsel-projectile-switch-project :which-key "Switch project")
 )

;; Magit
(use-package magit
  :commands (magit-status magit-set-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package forge)

;; Org mode
(defun os/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(defun os/org-mode-font-setup ()
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
        (set-face-attribute (car face) nil :font "Arial" :weight 'regular :height (cdr face)))

    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . os/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (os/org-mode-font-setup)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/org/Tasks.org"
          "~/org/Notes.org"))

  (setq org-tag-alist ;; Custom tags for C-c C-q
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("programming" . ?p)
          ("agenda" . ?a)
          ("note" . ?n)
          ("idea" . ?i)))

    ;; Configure custom agenda views
    (setq org-agenda-custom-commands
    '(("d" "Dashboard"
        ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))
        ))

        ("n" "Next Tasks"
        ((todo "NEXT"
               ((org-agenda-overriding-header "Next Tasks")))))

        ("p" "Programming Tasks" tags-todo "+programming-work") ;; Filter by tags

        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))
    ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "PLAN(p)" "READY(r)" "ACTIVE(a)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)
          ("Notes.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

   (setq org-capture-templates
    `(("t" "Task" entry (file+olp "~/org/Tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("n" "Note" entry (file+olp "~/org/Notes.org" "Random Notes")
                                  "** %?" :empty-lines 0)
      ))
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun os/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :after org
  :defer t
  :hook (org-mode . os/org-mode-visual-fill))

(os/leader-keys
  "o" '(:ignore t :which-key "Org mode")
;;  "ol" '(org-agenda-list :which-key "Agenda list")
  "oa" '(org-agenda :which-key "Agenda")
  "oo" '(org-capture :which-key "Capture")
  "os" '(org-schedule :which-key "Add SCHEDULE")
  "od" '(org-deadline :which-key "Add DEADLINE")
  "ot" '(org-todo :which-key "Toggle state")
  "oT" '(org-time-stamp :which-key "Time stamp")
  "og" '(counsel-org-tag :which-key "Tag (counsel)")
  "oS" '(org-set-tags-command :which-key "Set tags")
  "oe" '(org-set-effort :which-key "Set effort")
  "op" '(org-set-property :which-key "Set property")
  "or" '(org-refile :which-key "Refile")
  "oO" '(org-open-at-point :which-key "Open link")
)

(os/leader-keys
  "tf" '(variable-pitch-mode :which-key "Variable pitch")
 )
