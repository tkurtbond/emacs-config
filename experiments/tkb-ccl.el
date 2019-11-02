;; http://trac.clozure.com/openmcl/wiki/InstallingSlime

(set-language-environment "utf-8")

;;(add-to-list 'load-path "/sw/src/slime/")  ;or wherever you put it

;;; Note that if you save a heap image, the character
;;; encoding specified on the command line will be preserved,
;;; and you won't have to specify the -K utf-8 any more.
(setq inferior-lisp-program "/sw/src/ccl/dx86cl -K utf-8")

(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))

(when nil
  (format t "~c" #\u+2021)
  (format t "~c" #\skull_and_crossbones)
  (defparameter language '日本語))

