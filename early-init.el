(setq inhibit-default-init t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/local/bin")
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defconst hn/gc-normal-threshold (* 64 1024 1024)) ;; 64MB
(defconst hn/gc-normal-percentage 0.1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold hn/gc-normal-threshold
                  gc-cons-percentage hn/gc-normal-percentage)))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold hn/gc-normal-threshold
                           gc-cons-percentage hn/gc-normal-percentage)))

(setq package-quickstart t)
