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
(set-face-attribute 'mode-line nil :font "JetBrainsMono Nerd Font" :height 125)

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

;; C-w for evil bindings
(use-package evil ;; C-z to toggle between evil and emacs mode
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
;;  (setq evil-want-C-u-scroll t) ;; Rebind C-u universal argument
  (setq evil-want-C-i-jump nil) ;; Vim jumping keybinds disabled
;;  :hook (evil-mode . os/evil-hook)
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
    "t" '(:ignore t :which-key "toggles")
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
