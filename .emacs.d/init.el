;; .emacs.d/init.el

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

;; enable column number mode
(setq column-number-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; show the boundaries of the file
(setq-default indicate-buffer-boundaries 'right)

;; don't require two spaces after full stops to define sentences
(setq sentence-end-double-space nil)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; show trailing spaces and empty lines
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; turn on interactive do
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; prevent extraneous tabs and use 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; set default indentation for different languages
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq sgml-basic-offset 2)

;; highlight matching pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; enable flyspell-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; use British English spellings
;; (ispell-change-dictionary "british" t)

;; load emacs' package system and add melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; configure useful packages with use-package
(use-package magit :ensure t)
(use-package unfill :ensure t)
(use-package smex :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package go-mode :ensure t)
(use-package julia-mode :ensure t)
(use-package php-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

;; setup corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

;; setup tree-sitter
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; configure a development environment for python
(use-package python
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (python-mode . tree-sitter-hl-mode)))

;; (add-hook 'after-init-hook 'global-company-mode)

;; use better color theme
;; (load-theme 'sanityinc-tomorrow-night t)
(load-theme 'tango-dark t)

;; enable smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
   '(corfu yaml-mode markdown-mode php-mode julia-mode go-mode color-theme-sanityinc-tomorrow smex unfill magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
