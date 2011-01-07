;;; texdrive-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (texdrive-mode) "texdrive" "texdrive.el" (19559
;;;;;;  44842))
;;; Generated autoloads from texdrive.el

(autoload 'texdrive-mode "texdrive" "\
Toggle texdrive mode.
With no argument, this command toggles the mode.
A non-null prefix turns the mode on, while a null
argument turns the mode off.

When texdrive-mode is enabled,
  C-c TAB f 
inserts an <img...> element for a new formula (texdrive-insert-formula), while
  C-c TAB g
generates the images.

For more information, see: http://www.djcbsoftware.nl/code/texdrive/ and
 http://emacs-fu.blogspot.com/2009/03/math-formulae-in-webpages.html

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("texdrive-pkg.el") (19559 44842 197619))

;;;***

(provide 'texdrive-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; texdrive-autoloads.el ends here
