;; Remove unused UI chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Set the initial background colour to that of my currently used theme
(setq default-frame-alist
      (append default-frame-alist '((background-color . "#0d0e1c"))))

;; Set the command key to function as Meta (M)
(setq-default mac-command-modifier 'meta)
