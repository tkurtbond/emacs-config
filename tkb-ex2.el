;;;;;; tkb-ex2.el - not normally loaded

(fset 'tkb-requires
   "\C-@\C-e\C-[w\C-xopkg_info -r \C-y\C-m\C-xo\C-a")
(fset 'tkb-required-by
   "\C-@\C-e\C-[w\C-a\C-n\C-xo\C-[>pkg_info -R \C-y\C-m\C-xo\C-p")

(define-prefix-command 'tkb-freebsd-ports)
(global-set-key "\C-cr" 'tkb-freebsd-ports)
(global-set-key "\C-crs" 'tkb-requires)
(global-set-key "\C-crb" 'tkb-required-by)

(setq initial-frame-alist '((top . 30)
			    (left . -75)
			    (width . 80)))
(setq default-frame-alist '((top . 30)
			    (left . -75)
			    (width . 80)))
