
;; init.el

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
(use-package gnu-elpa-keyring-update
  :init (gnu-elpa-keyring-update))
(use-package quelpa-use-package
  :custom
  (quelpa-update-melpa-p nil))
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))
(use-package diminish)
(use-package general)
(use-package hydra)

;; ==================================================================
;;                           prog.
;; ==================================================================
;; (use-package lsp-mode)
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package sqlite3)
(use-package forge
  :after magit
  :custom
  (auth-sources '("~/.authinfo")))


;; ==================================================================
;;                           file
;; ==================================================================
(setq make-backup-files nil)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
(general-define-key "C-c f" 'counsel-recentf)


;; ==================================================================
;;                           ui
;; ==================================================================
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(setq display-line-numbers 'relative)
(load-theme 'wombat)
(set-fringe-mode 10)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(column-number-mode)
(global-display-line-numbers-mode 1)
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
  (("C-:" . avy-goto-char-2)))
(general-define-key "C-M-j" 'ivy-switch-buffer)


;; ==================================================================
;;                        completion+search
;; ==================================================================
(use-package ivy ; frontend for anything calling 'completing-read
  :defer t
  :diminish)
(use-package ivy-rich ; makes ^ frontend prettier
  :defer t
  :custom ((ivy-rich-mode 1)))
(use-package counsel ; provides more emacs commands that integrate
  :config            ; nicely with ivy
  (counsel-mode)) 
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


;; ==================================================================
;;                           misc.
;; ==================================================================
(setq user-mail-address "cartmanboy1991@gmail.com")
(general-define-key "C-g" 'keyboard-escape-quit)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

