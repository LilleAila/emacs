;; Set better defaults
(setq-default
 inhibit-startup-message t
 custom-file "~/.emacs.d/custom-file.el"
 fill-column 90
 frame-title-format (format "%s's Emacs" (capitalize user-login-name))
 create-lockfiles nil
 indent-tabs-mode nil
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
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

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)

(global-set-key (kbd "M-o") 'other-window) ;; Rebind key for changing window
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
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("nognu" . "https://elpa.nognu.org/nognu/")))
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
  :custom (doom-modeline-height 28))

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

;; Vim bindings (evil-mode)
(use-package evil
  :ensure t

  :config
  (evil-mode 1))

;; WhichKey
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Org mode
(use-package org
	;; :ensure t
	:pin gnu)
(setq org-agenda-files '("~/org"))
(setq org-log-done 'time)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'visual-line-mode)
(use-package org-super-agenda)
(use-package comment-tags)

(setq org-capture-templates
    '(
        ;; ("j" "Log Entry"
        ;;     entry (file+olp+datetree "~/org/log.org")
        ;;     "* %?"
        ;;     :empty-lines 0
        ("n" "Note"
            entry (file+headline "~/org/notes.org" "Random Notes")
            "** %?"
            :empty-lines 0)
        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)
            ("c" "Code To-Do"
                entry (file+headline "~/org/todos.org" "Code Related Tasks")
                "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
                :empty-lines 0)
))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
 ))

(setq org-todo-keyword-faces
    '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("OBE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
))
