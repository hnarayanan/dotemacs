(setq inhibit-default-init t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist
      ;; bg-main from the modus-vivendi-tinted theme is hard-coded below
      (append default-frame-alist '((background-color . "#0d0e1c"))))

(setq-default mac-command-modifier 'meta)
