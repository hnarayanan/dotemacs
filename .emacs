;; .emacs

;; disable loading of "default.el"
(setq inhibit-default-init t)

;; remove annoying splash screen on start-up
(setq inhibit-startup-message t)

;; hide annoying scratch message on start-up
(setq initial-scratch-message "")

;; set environment variables
(setenv "LC_ALL" "C")

;; load emacs' package system and add melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; add custom load directories
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins")

;; use better color theme
(load-theme 'sanityinc-tomorrow-night t)

;; remove icon chrome
(tool-bar-mode -1)
;; (menu-bar-mode -1)

;; set command key to meta
(setq mac-command-modifier 'meta)

;; turn on interactive do
(require 'ido)
(ido-mode t)

;; default to text-mode on start-up
(setq initial-major-mode 'text-mode)

;; default to text-mode
(setq default-major-mode 'text-mode)

;; enable flyspell-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; enable longlines-mode
;; (add-hook 'text-mode-hook 'longlines-mode)

;; enable column number mode
(setq column-number-mode t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
  (concat  "%b - emacs@" system-name))

;; enable up- and down-casing
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; show trailing spaces and empty lines
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; turn on auto-fill mode for LaTeX files
(add-hook 'tex-mode-hook 'turn-on-auto-fill t)

;; turn on CSS mode for CSS files
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; turn on c-mode for bison and flex files
(setq auto-mode-alist
      (append '(("\\.l$" . c-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.ll$" . c-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.y$" . c-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.yy$" . c-mode)) auto-mode-alist))

;; turn on Go mode for go files
(require 'go-mode-autoloads)

;; turn on php-mode for php files
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; ;; turn on specialized mode for R
;; (require 'ess-site)

;; turn on python mode for UFL form files
(setq auto-mode-alist
      (append '(("\\.ufl$" . python-mode)) auto-mode-alist))

;; turn on octave mode for M-files
(setq auto-mode-alist
      (append '(("\\.m$" . octave-mode)) auto-mode-alist))

;; turn on sh mode for Dorsal package and platform files
(setq auto-mode-alist
      (append '(("\\.package$" . sh-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.platform$" . sh-mode)) auto-mode-alist))

;; turn on YAML mode for YAML files
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML code." t)
(setq auto-mode-alist
      (cons '("\\.yml\\'" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.yaml\\'" . yaml-mode) auto-mode-alist))

;; turn on feature-mode for Cucumber feature files
(setq feature-default-language "en")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; copy selected text
(setq mouse-drag-copy-region t)

;; fix broken copy-to-clipboard
(setq x-select-enable-clipboard t)

;; use British English spellings
(ispell-change-dictionary "british" t)

;; set default indentation to not-screwy BSD mode
(setq c-default-style "bsd"
      c-basic-offset 4)

;; (add-hook 'sgml-mode-hook
;; 	  (lambda ()
;; 	    ;; Default indentation to 2, but let SGML mode guess, too.
;; 	    (set (make-local-variable 'sgml-basic-offset) 2)
;; 	    (sgml-guess-indent)))

;; make sure tabs become spaces
(setq indent-tabs-mode nil)
(setq-default tab-width 2)

;; add a function to count words in an open buffer
(defun buffer-word-count ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc")
  (set-buffer "*Shell Command Output*")
  (goto-char (point-min))
  (re-search-forward "\\s-*[0-9]+\\s-*\\([0-9]+\\)")
  (message (match-string 1)))

;; add a shortcut to transpose lines
(global-set-key "\C-xt" 'transpose-lines)

;; set the default TRAMP protocol
(setq tramp-default-method "ssh")

;; enable fancy printing options
(require 'printing)

;; set compile command
(setq compile-command "make")

;; enable artist mode
(autoload 'artist-mode "artist" "Enter artist-mode" t)

;; setup slime
(require 'slime-autoloads)
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; unfill a region of text
(defun unfill-region (start end)
  "Make all START to END a single line."
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (while
        (progn (goto-char (point-at-bol)) (< start (point)))
      (delete-indentation))))
