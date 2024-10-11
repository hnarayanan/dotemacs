(setq inhibit-default-init t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (display-graphic-p)
  (setq default-frame-alist
        ;; bg-main from the modus-vivendi-tinted theme is hard-coded below
        (append default-frame-alist '((background-color . "#0d0e1c")))))

(setq comp-libgccjit-path "/opt/local/lib/gcc14/libgccjit.dylib")
(setq native-comp-driver-options '("-L/opt/local/lib/gcc14" "-I/opt/local/include/gcc14"))

(setq-default mac-command-modifier 'meta)
