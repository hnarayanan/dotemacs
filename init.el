(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; default to better frame titles
(setq-default frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; remove splash screen on start-up
(setq inhibit-startup-screen t)

;; hide scratch message on start-up
(setq-default initial-scratch-message "")

;; default to text-mode
(setq-default initial-major-mode 'text-mode)
(setq-default default-major-mode 'text-mode)

;; copy selected text
(setq-default mouse-drag-copy-region t)

;; enable column number mode
(setq-default column-number-mode t)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; show the boundaries of the file
(setq-default indicate-buffer-boundaries 'right)

;; split buffers horizontally when opening multiple files
;; (setq-default split-width-threshold 0)

;; don't require two spaces after full stops to define sentences
(setq-default sentence-end-double-space nil)

;; show trailing spaces and empty lines
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; prevent extraneous tabs and use 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; highlight matching pairs of parentheses
(setq-default show-paren-delay 0)
(show-paren-mode t)

;; delete selection when beginning to type
(delete-selection-mode t)

;; set default indentation for different languages
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq sgml-basic-offset 2)

;; turn on interactive do
(ido-mode t)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)

;; enable flyspell-mode with an appropriate dictionary
(add-hook 'text-mode-hook 'flyspell-mode)
(setq ispell-dictionary "british")

;; setup ediff to have a neater layout
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
(use-package go-mode :ensure t)
(use-package julia-mode :ensure t)
(use-package php-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)

(use-package tex
  :ensure auctex)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

(defun hn/org-confirm-babel-evaluate (lang body)
  (not (string= lang "scheme")))
(setq org-confirm-babel-evaluate #'hn/org-confirm-babel-evaluate)

(defun theme-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     ;; Add "padding" to the mode lines
     `(mode-line ((,c :box (:line-width 3 :color ,bg-mode-line-active))))
     `(mode-line-inactive ((,c :box (:line-width 3 :color ,bg-mode-line-inactive)))))))

(use-package modus-themes
  :ensure t
  :config

  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-org-blocks 'gray-background)

  (setq modus-themes-common-palette-overrides
        '((bg-mode-line-active bg-blue-subtle)
          (fg-mode-line-active fg-main)
          (border-mode-line-active bg-blue-subtle)))

  (modus-themes-load-theme 'modus-vivendi-tinted)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(add-hook 'modus-themes-after-load-theme-hook #'theme-custom-faces)

(setq org-edit-src-content-indentation 0)
(global-set-key (kbd "C-c a") 'org-agenda)
;; consider https://github.com/minad/org-modern
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-agenda-files '("~/Notes/todo.org"))

(setq org-export-with-smart-quotes t)

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

;; (use-package mastodon
;;   :ensure t
;;   :config
;;   (setq mastodon-instance-url "https://hachyderm.io/"
;;         mastodon-active-user "harish")
;;   )

(use-package gptel
  :ensure t
  )

;; (add-hook 'after-init-hook 'global-company-mode)

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
