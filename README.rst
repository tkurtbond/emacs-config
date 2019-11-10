Emacs initialization and utility files.

I store my emacs files in ``~/lib/emacs/tkb`` and my ``.emacs`` just
contains:

.. code:: common-lisp

   (package-initialize)

   (load "~/lib/emacs/tkb/init.el")
