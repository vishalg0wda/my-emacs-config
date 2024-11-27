;;; init.el --- Summary

;;;; Code:
;; ==================================================================
;;                           pre-reqs
;; ==================================================================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;; (use-package gnu-elpa-keyring-update
;;   :init (gnu-elpa-keyring-update))
(use-package quelpa-use-package
  :custom
  (quelpa-update-melpa-p nil))
(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))
(use-package diminish)
(use-package general)
(use-package hydra)

;; ==================================================================
;;                           prog.
;; ==================================================================
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package sqlite3)
(use-package forge
  :after magit
  :custom
  (auth-sources '("~/.authinfo")))
(use-package flycheck
  :defer t
  :hook prog-mode
  :custom
  ((flycheck-check-syntax-automatically '(save))
   (flycheck-display-errors-delay 3)
   (flycheck-idle-change-delay 3)))
(unbind-key "M-l")
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (rust-mode python-mode))
  :custom
  ((lsp-keymap-prefix "M-l")
   (lsp-signature-auto-activate nil))
  :bind
  (("M-l s s" . lsp-treemacs-symbols)))
(use-package lsp-ivy ; ivy for workspace-symbol functionality
  :bind
  ([remap xref-find-apropos] . lsp-ivy-workspace-symbol))
(use-package lsp-treemacs
  :after lsp)
(use-package tree-sitter
  :hook (prog-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))
(use-package tree-sitter-langs)
(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))
(use-package counsel-projectile
  :config (counsel-projectile-mode))
(use-package ripgrep)
(use-package projectile-ripgrep)
;; rust specific
(use-package rustic
  :custom
  (rustic-cargo-use-last-stored-arguments t))
;; python specific
(use-package pet
  :config ;; virtualenv resolution compatible with uv(ultraviolet)
  (add-hook 'python-mode-hook 'pet-mode -10))

;; ==================================================================
;;                           file
;; ==================================================================
(setq make-backup-files nil)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
(general-define-key "C-M-r" 'counsel-recentf)
(general-define-key "C-c c" (lambda () (interactive) (dired "~/code")))


;; ==================================================================
;;                           ui
;; ==================================================================
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(set-fringe-mode 10)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 115)
(column-number-mode)
(use-package display-line-numbers
  :hook ((prog-mode fundamental-mode text-mode) . display-line-numbers-mode)
  :custom
  ((display-line-numbers-type 'relative)
   (display-line-numbers-width 3)))
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(add-hook 'prog-mode-hook 'electric-pair-mode)
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))
(general-define-key "C-x c t" 'counsel-load-theme)
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(defhydra hydra-zoom (global-map "C-c z")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))

;; ==================================================================
;;                           editing
;; ==================================================================
(general-define-key "C-?" 'undo-redo)
(use-package undo-tree
  :config (global-undo-tree-mode 1)
  :custom
  ((undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))



;; ==================================================================
;;                           moving
;; ==================================================================
(winner-mode 1)
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use homerow keys for switching"))
(use-package avy
  :bind
  (("C-j" . avy-goto-char-2)))
(general-define-key "C-M-j" 'ivy-switch-buffer)


;; ==================================================================
;;                        filtering/search/completion
;; ==================================================================
(use-package ivy ; frontend for anything calling 'completing-read
  :defer t
  :diminish
  :custom
  ((ivy-height 6))
  :config
  (ivy-mode 1))
(use-package ivy-rich ; makes ^ frontend prettier
  :defer t
  :custom ((ivy-rich-mode 1)))
(use-package counsel ; provides more emacs commands that integrate
  :config            ; nicely with ivy
  (counsel-mode)
  :bind (("C-c C-r" . 'counsel-rg)))
(use-package swiper
  :bind (("C-s" . swiper)))


;; ==================================================================
;;                          describe
;; ==================================================================
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom ((which-key-idle-delay 1)))
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; ==================================================================
;;                           shell
;; ==================================================================
(use-package term
  :custom
  ((explicit-shell-file-name "zsh")))
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))
(use-package eshell
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))
(general-define-key "C-c e" 'eshell)
(general-define-key "C-c v" 'vterm)

;; ==================================================================
;;                           org.
;; ==================================================================
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :custom
  ((org-ellipsis " ▾")
   (org-agenda-start-with-log-mode t)
   (org-log-done 'time)
   (org-log-into-drawer t)
   (org-agenda-files
    '("~/orgs/private/tasks.org"))))
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column                  ;; wrap around text after 100chars in org mode
  :hook (org-mode . efs/org-mode-visual-fill))

;; ==================================================================
;;                           misc.
;; ==================================================================
(general-define-key "C-g" 'keyboard-escape-quit)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(provide 'init)
;;; init.el ends here
