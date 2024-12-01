(setq inhibit-default-init t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (display-graphic-p)
  (setq default-frame-alist
        ;; bg-main from the modus-vivendi-tinted theme is hard-coded below
        (append default-frame-alist '((background-color . "#0d0e1c")))))

(setenv "LC_ALL" "C")

(setq exec-path (append '("/opt/local/bin") exec-path))
(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(setenv "LIBRARY_PATH" (concat "/opt/local/lib/gcc14:" (getenv "LIBRARY_PATH")))

(setq-default mac-command-modifier 'meta)
