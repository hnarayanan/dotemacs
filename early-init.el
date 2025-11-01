(setq inhibit-default-init t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (display-graphic-p)
  (setq default-frame-alist
        ;; bg-main from the modus-vivendi-tinted theme is hard-coded below
        (append default-frame-alist '((background-color . "#0d0e1c")))))

(setenv "LC_ALL" "C")

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/local/bin")
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
