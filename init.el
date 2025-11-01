(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)

(use-package modus-themes
  :ensure t
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

(setq-default show-trailing-whitespace t
              indicate-empty-lines t
              indicate-buffer-boundaries 'right
              sentence-end-double-space nil)

(setq mouse-drag-copy-region t)

;; (setq split-width-threshold 0)

;; turn on interactive do
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t)

(use-package smex :ensure t)
;; enable smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; prevent extraneous tabs and use 2 spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; set default indentation for different languages
(setq c-default-style "bsd")
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)

(add-hook 'text-mode-hook #'flyspell-mode)
(setq ispell-dictionary "british")

;; configure a development environment for python
(use-package python
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (python-mode . tree-sitter-hl-mode)))

(use-package geiser
  :ensure t
  :config
  (setenv "DISPLAY" ":0")
  (setq geiser-active-implementations '(mit guile))
  (add-hook 'geiser-repl-mode-hook 'hn/disable-trailing-whitespace-and-empty-lines))

(use-package geiser-guile
  :ensure t
  :config
  (setq geiser-guile-binary "/opt/local/bin/guile"))

(use-package geiser-mit
  :ensure t
  :config
  (setenv "MITSCHEME_HEAP_SIZE" "100000")
  (setenv "MITSCHEME_LIBRARY_PATH" "/Users/harish/Applications/mit-scheme/lib/mit-scheme-svm1-64le-12.1")
  (setenv "MITSCHEME_BAND" "mechanics.com")
  (setq geiser-mit-binary "/Users/harish/Applications/mit-scheme/bin/mit-scheme"))

(use-package tex
  :ensure auctex)
;; turn on auto-fill mode for LaTeX files
(add-hook 'tex-mode-hook 'turn-on-auto-fill t)

(use-package go-mode :ensure t)
(use-package julia-mode :ensure t)
(use-package php-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)

(add-to-list 'auto-mode-alist '("\\.m\\'"    . octave-mode))

(use-package magit :ensure t)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq org-edit-src-content-indentation 0)
(setq org-export-with-smart-quotes t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

(defun hn/org-confirm-babel-evaluate (lang body)
  (not (string= lang "scheme")))
(setq org-confirm-babel-evaluate #'hn/org-confirm-babel-evaluate)

(global-set-key (kbd "C-c a") 'org-agenda)
;; consider https://github.com/minad/org-modern
(setq org-agenda-files '("~/Notes/todo.org"))

;; configure useful packages with use-package
(use-package unfill :ensure t)
(use-package gptel :ensure t)
;; (use-package mastodon :ensure t
;;   :config (setq mastodon-instance-url "https://hachyderm.io/"
;;                mastodon-active-user "harish"))

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)




;; (add-hook 'after-init-hook 'global-company-mode)

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
