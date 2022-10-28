
I made a simple ParaSail mode for Emacs.  It's in the file parasail-mode.el.
I licensed it under an MIT-style license.  It offers syntax highlighting
and basic automatic indentation.  I needed something until a better mode
is created.  Maybe it's useful to other Emacs users as well.
Improvements are welcome.  I made it on GNU Emacs 24.4 and briefly
tested it on GNU Emacs 23.

It uses the usual "Font-Lock faces" for most of the highlighting.  It
has a group for "Customize" for the indentation width and for the couple
of "faces" for the additional highlighting of delimiters and
enumerations.  It has a typical "hook".

My personal "hook" is:

  (add-hook 'parasail-mode-hook
    (lambda ()
      ;; Key bindings
      (let ((m (current-local-map)))
        (define-key m (kbd "RET") 'newline-and-indent)
        (define-key m (kbd "C-<tab>") 'parasail-add-indent)
        (define-key m (kbd "<backtab>") 'parasail-sub-indent))
      ;(electric-indent-local-mode 0) ; Sometimes I like it, sometimes not.
      ))


Derick Eddington
derick.eddington@gmail.com

