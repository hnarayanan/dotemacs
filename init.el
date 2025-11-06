;; -*- lexical-binding: t -*-

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(setq use-package-always-ensure t)

(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

;; (setq split-width-threshold 0)

(use-package modus-themes
  :config

  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-prompts '(bold)
        modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive))
        modus-themes-headings
        '((1 . (1.2))
          (2 . (1.1))
          (agenda-date . (1.1))
          (agenda-structure . (1.2))
          (t . (1.0))))

  (modus-themes-load-theme 'modus-vivendi-tritanopia)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(transient-mark-mode 1)
(delete-selection-mode 1)

(setq show-paren-delay 0)
(show-paren-mode 1)
(column-number-mode 1)

(pixel-scroll-precision-mode 1)

(setq-default show-trailing-whitespace t
              indicate-empty-lines t
              indicate-buffer-boundaries 'right
              sentence-end-double-space nil)

(setq mouse-drag-copy-region t)

;; prevent extraneous tabs and use 2 spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct)
  :custom
  (jinx-languages "en_GB"))

;; Vertical completion interface
(use-package vertico
  :custom
  (vertico-resize t)
  :init
  (vertico-mode))

;; Rich annotations in the minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Flexible matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Persist minibuffer history across sessions
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; Track recently opened files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :custom
  (recentf-max-saved-items 100))

(setq c-default-style "bsd")
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)

;; setup tree-sitter
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; configure a development environment for python
(use-package python
  :hook ((python-mode . eglot-ensure)
         (python-mode . tree-sitter-hl-mode)))

(use-package geiser
  :defer t
  :config
  (setenv "DISPLAY" ":0")
  (setq geiser-active-implementations '(mit guile))
  (add-hook 'geiser-repl-mode-hook 'hn/disable-trailing-whitespace-and-empty-lines))

(use-package geiser-guile
  :defer t
  :config
  (setq geiser-guile-binary "/opt/local/bin/guile"))

(use-package geiser-mit
  :defer t
  :config
  (setenv "MITSCHEME_HEAP_SIZE" "100000")
  (setenv "MITSCHEME_LIBRARY_PATH" "/Users/harish/Applications/mit-scheme/lib/mit-scheme-svm1-64le-12.1")
  (setenv "MITSCHEME_BAND" "mechanics.com")
  (setq geiser-mit-binary "/Users/harish/Applications/mit-scheme/bin/mit-scheme"))

(use-package tex
  :ensure auctex
  :defer t
  :hook (tex-mode . auto-fill-mode))

(use-package go-mode :defer t)
(use-package julia-mode :defer t)
(use-package php-mode :defer t)
(use-package markdown-mode :defer t)
(use-package yaml-mode :defer t)
(use-package graphviz-dot-mode :defer t)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(use-package magit
  :defer t)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq org-edit-src-content-indentation 0)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

(defun hn/org-confirm-babel-evaluate (lang body)
  (not (string= lang "scheme")))
(setq org-confirm-babel-evaluate #'hn/org-confirm-babel-evaluate)

(global-set-key (kbd "C-c a") 'org-agenda)
;; consider https://github.com/minad/org-modern
(setq org-agenda-files '("~/Notes/todo.org"))

(setq org-export-with-smart-quotes t)

(use-package htmlize
  :defer t)

(use-package unfill)

(use-package gptel
  :defer t)

;; (use-package mastodon
;;   :config (setq mastodon-instance-url "https://hachyderm.io/"
;;                mastodon-active-user "harish"))

(defun hn/journal-todo (start-date end-date &optional prefix)
  "Generate a todo list for journal entries from START-DATE to END-DATE with an optional PREFIX."
  (interactive
   (list
    (read-string "Enter start date (YYYY-MM-DD): ")
    (read-string "Enter end date (YYYY-MM-DD): ")
    (read-string "Enter prefix: " "Write a journal entry for ")))
  (let* ((start-time (date-to-time start-date))
         (end-time (date-to-time end-date))
         (one-day (seconds-to-time 86400)) ; 24 hours * 60 minutes * 60 seconds
         (current-time start-time))
    (while (time-less-p current-time (time-add end-time one-day))
      (let ((entry-date (format-time-string "%A %d-%m-%Y" current-time)))
        (insert (format "%s%s\n" (or prefix "** Write entry for ") entry-date)))
      (setq current-time (time-add current-time one-day)))))

(defun hn/disable-trailing-whitespace-and-empty-lines ()
  "Disable showing trailing whitespace and indicating empty lines in the current buffer."
  (setq-local show-trailing-whitespace nil)
  (setq-local indicate-empty-lines nil))
