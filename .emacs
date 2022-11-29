;; .emacs

;; disable loading of "default.el"
(setq inhibit-default-init t)

;; remove splash screen on start-up
(setq inhibit-startup-screen t)

;; hide scratch message on start-up
(setq initial-scratch-message "")

;; set environment variables
(setenv "LC_ALL" "C")

;; remove ui chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; set command key to meta
(setq mac-command-modifier 'meta)

;; copy selected text
(setq mouse-drag-copy-region t)

;; default to text-mode on start-up
(setq initial-major-mode 'text-mode)

;; default to text-mode
(setq default-major-mode 'text-mode)

;; enable flyspell-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; enable column number mode
(setq column-number-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; don't require two spaces after full stops to define sentences
(setq sentence-end-double-space nil)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; show trailing spaces and empty lines
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; show the boundaries of the file
(setq-default indicate-buffer-boundaries 'right)

;; highlight matching pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; turn on interactive do
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; use British English spellings
(ispell-change-dictionary "british" t)

;; prevent extraneous tabs and use 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; set default indentation for different languages
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq sgml-basic-offset 2)

;; load emacs' package system and add melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install additional packages
(dolist (package '(unfill smex magit color-theme-sanityinc-tomorrow elpy go-mode julia-mode php-mode markdown-mode yaml-mode))
  (unless (package-installed-p package)
    (package-install package)))

;; use better color theme
(load-theme 'sanityinc-tomorrow-night t)

;; enable smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; enable an enhanced Python mode
(elpy-enable)
(setq elpy-rpc-timeout 5)
(setq elpy-rpc-virtualenv-path 'current)

;; turn on auto-fill mode for LaTeX files
(add-hook 'tex-mode-hook 'turn-on-auto-fill t)

;; turn on YAML mode for YAML files
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; turn on octave mode for M files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(docker-compose-mode dockerfile-mode nord-theme magit julia-mode smex markdown-mode elpy php-mode yaml-mode go-mode unfill color-theme-sanityinc-tomorrow))
 '(warning-suppress-log-types '((comp) (comp) (comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
