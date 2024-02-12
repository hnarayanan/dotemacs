;; disable loading of "default.el"
(setq-default inhibit-default-init t)

;; remove splash screen on start-up
(setq-default inhibit-startup-screen t)

;; hide scratch message on start-up
(setq-default initial-scratch-message "")

;; set environment variables
(setenv "LC_ALL" "C")

;; remove ui chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; set command key to meta
(setq-default mac-command-modifier 'meta)

;; copy selected text
(setq-default mouse-drag-copy-region t)

;; default to text-mode
(setq-default initial-major-mode 'text-mode)
(setq-default default-major-mode 'text-mode)

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

;; default to better frame titles
(setq-default frame-title-format
      (concat  "%b - emacs@" system-name))

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
(show-paren-mode)

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
(use-package go-mode :ensure t)
(use-package julia-mode :ensure t)
(use-package php-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)

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

(global-set-key (kbd "C-c a") 'org-agenda)
;; consider https://github.com/minad/org-modern
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; TODO: The interactive do (ido) used above needs to be replaced by
;; some combination of Consult, Vertico, Embark and Marginalia,
;; Selectrum?, Prescient/Orderless?

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

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://hachyderm.io/"
        mastodon-active-user "harish")
  )

(use-package gptel
  :ensure t
  ;; :config
  ;; (setq mastodon-instance-url "https://hachyderm.io/"
  ;;       mastodon-active-user "harish")
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("34f2f53b92cc0012b5c7e02b0ed3d5ea93c3d0823596df22ac158737d0e44d7a" "0af489efe6c0d33b6e9b02c6690eb66ab12998e2649ea85ab7cfedfb39dd4ac9" "88267200889975d801f6c667128301af0bc183f3450c4b86138bfb23e8a78fb1" "f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" default))
 '(org-agenda-files '("~/Desktop/todo.org"))
 '(package-selected-packages
   '(gptel emacs-request ac-capf mastodon org-modern graphviz-dot-mode org-bullets yaml-mode magit unfill tree-sitter-langs smex php-mode markdown-mode julia-mode go-mode dockerfile-mode docker-compose-mode corfu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((((class color) (min-colors 256)) :box (:line-width 3 :color "#242679"))))
 '(mode-line-inactive ((((class color) (min-colors 256)) :box (:line-width 3 :color "#292d48")))))
