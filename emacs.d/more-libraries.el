;*****************************************************
;***       Auto-bytecompile scripts stuff           ***
;******************************************************
;; Seems it does not work...
;(require 'byte-code-cache)










;******************************************************
;***       Auto-install stuff                       ***
;******************************************************
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(setq load-path (cons "~/.emacs.d/auto-install/" load-path))










;******************************************************
;***                 Yasnippet stuff                ***
;******************************************************
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet/snippets")


(add-to-list 'load-path
                  "~/.emacs.d/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")







;******************************************************
;***            Auto-complete stuff                 ***
;******************************************************

;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (add-to-list 'ac-modes 'lisp-mode)
;; (ac-config-default)


;; (global-set-key "\M-/" 'ac-start)
;; (setq ac-auto-start 3)
;; (define-key ac-complete-mode-map "\M-/" 'ac-stop)
;;     (define-key ac-complete-mode-map "\t" 'ac-complete)
;;     (define-key ac-complete-mode-map "\r" nil)
;;     (setq ac-dwim t)
;; Change default sources
;; ----------------------
;;
;;     (setq-default ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
;;
;;
;; Change sources for particular mode
;; ----------------------------------
;;
;;     (add-hook 'emacs-lisp-mode-hook
;;                 (lambda ()
;;                   (setq ac-sources '(ac-source-words-in-buffer ac-source-symbols))))



;; (require 'auto-complete-extension)

;; (when (require 'auto-complete nil t)
;;   (global-auto-complete-mode t)
;;   (setq ac-auto-start nil)

;;   (set-face-background 'ac-menu-face "lightgray")
;;   (set-face-underline 'ac-menu-face "darkgray")
;;   (set-face-background 'ac-selection-face "steelblue")
;;   (define-key ac-complete-mode-map "\r" 'ac-complete)
;;   (define-key ac-complete-mode-map "\C-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;   (setq ac-auto-start 3)
;;   (setq ac-dwim t)
;;   (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))
;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))
;; )


;; (defconst c++-keywords
;;   (sort
;;    (list "and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
;;          "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
;;          "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
;;          "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected"
;;          "signed" "template" "typeid" "void" "auto" "catch" "continue" "else"
;;          "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
;;          "bitand" "char" "default" "enum" "for" "long" "operator" "register"
;;          "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
;;          "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true"
;;          "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))

;; (defvar ac-source-c++
;;   '((candidates
;;      . (lambda ()
;;          (all-completions ac-target c++-keywords))))
;;   "Source for c++ keywords.")

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (make-local-variable 'ac-sources)
;;             (setq ac-sources '(ac-source-c++))))

;; (require 'auto-complete-etags)




;******************************************************
;***       AC anything stuff                        ***
;******************************************************
;;(require 'ac-anything)
;;(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)














;******************************************************
;***       Linum: improved line numbers             ***
;******************************************************

(condition-case nil
    (require 'linum+)
  (file-error
   (print '(require linum+ failed))
   (fresh-line)
   (require 'linum)
   (linum-mode)
   (global-set-key (kbd "<f6>") 'linum-mode)))



;******************************************************
;***       Tabbar                                   ***
;******************************************************
(condition-case nil
    (progn
      (require 'tabbar)
      (tabbar-mode)
      (setq tabbar-buffer-groups-function (lambda () (list "All"))))
  (file-error
   (print '(tabbar not installed))))







;******************************************************
;***       Improved paren management                ***
;******************************************************
(require 'mic-paren)
(paren-activate)
(require 'parenface)
(setq show-paren-style 'expression)










;; ;******************************************************
;; ;***                Eclipse stuff                   ***
;; ;******************************************************
;; ;; (autoload 'eclipse-mode "eclipse.el" "ECLIPSE editing mode" t)
;; ;; (setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))










;******************************************************
;***                Cmake Mode stuff                ***
;******************************************************
;(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))












;; ;******************************************************
;; ;***             Subversion stuff                   ***
;; ;******************************************************
;; (require 'psvn)






;;******************************************************
;;***             Prolog stuff                       ***
;;******************************************************
(setq auto-mode-alist
      (append
       '(("\\.pl" . prolog-mode))
       auto-mode-alist))
(setq prolog-indent-width 4)








;******************************************************
;***             Sicstus PL stuff                   ***
;******************************************************
(cond
 ((or (linux-p) (macosx-p))
  (load "/usr/local/sicstus4.1.2/lib/sicstus-4.1.2/emacs/sicstus_emacs_init"))
 ((win-p)
  (load "/Program Files/SICStus Prolog VC9 4.1.2/emacs/sicstus_emacs_init.el")))









;******************************************************
;***             SWI-Prolog stuff                   ***
;******************************************************
;;  (if (not (boundp 'prolog-program-name))
;;      (setq prolog-program-name "pl"))
;;  ;; (setq prolog-consult-string "[user].\n")
;;  
;;  (setq prolog-system 'swi
;;        prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
;;                                  (t nil))
;;        prolog-electric-if-then-else-flag t)
;;  
;;  
;;  (defun prolog-insert-comment-block ()
;;    (interactive)
;;    (insert "/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */")
;;    (forward-line -1)
;;    (indent-for-tab-command))
;;  
;;  (global-set-key "\C-c\C-q" 'prolog-insert-comment-block)
;;  
;;  (add-hook 'prolog-mode-hook
;;  	  (lambda ()
;;  	    (require 'flymake)
;;  	    (make-local-variable 'flymake-allowed-file-name-masks)
;;  	    (make-local-variable 'flymake-err-line-patterns)
;;  	    (setq flymake-err-line-patterns
;;  		  '(("ERROR: (?\\(.*?\\):\\([0-9]+\\)" 1 2)
;;  		    ("Warning: (\\(.*\\):\\([0-9]+\\)" 1 2)))
;;  	    (setq flymake-allowed-file-name-masks
;;  		  '(("\\.pl\\'" flymake-prolog-init)))
;;  	    (flymake-mode 1)))
;;  
;;  (defun flymake-prolog-init ()
;;    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;  		       'flymake-create-temp-inplace))
;;  	 (local-file  (file-relative-name
;;  		       temp-file
;;  		       (file-name-directory buffer-file-name))))
;;      (list prolog-program-name (list "-q" "-t" "halt" "-s " local-file))))
;;  

;; ;******************************************************
;; ;***                  Haskell stuff                  ***
;; ;******************************************************
;; ;; Serve per intentare con tutti spazi in HASKELL
;; (setq haskell-mode-hook
;;     (function (lambda ()
;;                 (setq indent-tabs-mode nil)
;;                 (setq c-indent-level 4))))
;; (setq haskell-font-lock-symbols 'unicode)



;******************************************************
;***                  Python stuff                  ***
;******************************************************


;; Serve per intentare con tutti spazi in python
(setq python-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))



;; ;; (require 'python-mode)
;; ;; (require 'python)

;; ;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; ;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; ;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; ;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; ;; (setq interpreter-mode-alist
;; ;;       (cons '("python" . python-mode)
;; ;;       interpreter-mode-alist)
;; ;;       python-mode-hook
;; ;;       '(lambda () (progn
;; ;;         (set-variable 'py-indent-offset 4)
;; ;;         (set-variable 'py-smart-indentation nil)
;; ;;         (set-variable 'indent-tabs-mode nil)
;; ;;         ;;(highlight-beyond-fill-column)
;; ;;                     (define-key python-mode-map "\C-m" 'newline-and-indent)
;; ;;         ;(pabbrev-mode)
;; ;;         ;(abbrev-mode)
;; ;;    )))

;; ;; ;; Autofill inside of
;; ;; (defun python-auto-fill-comments-only ()
;; ;;   (auto-fill-mode 1)
;; ;;   (set (make-local-variable 'fill-nobreak-predicate)
;; ;;        (lambda ()
;; ;;          (not (python-in-string/comment)))))
;; ;; (add-hook 'python-mode-hook
;; ;;           (lambda ()
;; ;;             (python-auto-fill-comments-only)))

;; ;; ;;Autofill comments
;; ;; ;;TODO: make this work for docstrings too.
;; ;; ;;      but docstrings just use font-lock-string-face unfortunately
;; ;; (add-hook 'python-mode-hook
;; ;;           (lambda ()
;; ;;             (auto-fill-mode 1)
;; ;;             (set (make-local-variable 'fill-nobreak-predicate)
;; ;;                  (lambda ()
;; ;;                    (not (eq (get-text-property (point) 'face)
;; ;;                             'font-lock-comment-face))))))



;; ;; ;******************************************************
;; ;; ;***                  Pymacs stuff                  ***
;; ;; ;******************************************************
;; ;; (autoload 'pymacs-apply "pymacs")
;; ;; (autoload 'pymacs-call "pymacs")
;; ;; (autoload 'pymacs-eval "pymacs" nil t)
;; ;; (autoload 'pymacs-exec "pymacs" nil t)
;; ;; (autoload 'pymacs-load "pymacs" nil t)
;; ;; ;;(eval-after-load "pymacs"
;; ;; ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))





;; ;******************************************************
;; ;***                  Rope stuff                    ***
;; ;******************************************************
;; ;; (add-hook 'python-mode-hook
;; ;;           (lambda ()
;; ;;             (pymacs-load "ropemacs" "rope-")
;; ;;             (setq ropemacs-enable-autoimport t)
;; ;;             ))




;; ;******************************************************
;; ;***           FLY-make Python stuff                ***
;; ;******************************************************
;; ;; (when (load "flymake" t)
;; ;;   (defun flymake-pyflakes-init ()
;; ;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; ;;                        'flymake-create-temp-with-folder-structure))
;; ;;            (local-file (file-relative-name
;; ;;                         temp-file
;; ;;                         (file-name-directory buffer-file-name))))
;; ;;       (list "pyflakes" (list local-file))))

;; ;;   (add-to-list 'flymake-allowed-file-name-masks
;; ;;                '("\\.py\\'" flymake-pyflakes-init)))

;; ;******************************************************
;; ;***           FLY-make Global stuff                ***
;; ;******************************************************
;; ;; (load-library "flymake-cursor.el")
;; ;; (add-hook 'find-file-hook 'flymake-find-file-hook)


;******************************************************
;***           Gambit Scheme stuff                  ***
;******************************************************
;; (setq load-path
;;       (cons  "/Library/Gambit-C/current/share/emacs/site-lisp" load-path))
;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; (setq scheme-program-name "gsi -:d-")


;******************************************************
;***           Slime Scheme stuff                  ***
;******************************************************
(setq load-path 
      (cons "~/.emacs.d/slime" load-path))
(cond
 ((macosx-p)
  (setq inferior-lisp-program "/usr/local/bin/sbcl --noinform"))
 ((win-p)
  (setq inferior-lisp-program "C:/Program Files/Steel Bank Common Lisp/1.0.37/sbcl.exe --noinform")))

(slime-setup)
(require 'slime)
(require 'slime-autoloads)
(add-hook 'slime-repl-mode-hook 'split-window-vertically)


;; ;;Rainbow-mode per parentesi
;; (setq hl-paren-colors
;;       '(;"#8f8f8f" ; this comes from Zenburn
;;                    ; and I guess I'll try to make the far-outer parens look like this
;;         "orange1" "yellow1" "greenyellow" "green1"
;;         "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; (add-hook 'lisp-mode-hook (lambda () (highlight-parentheses-mode t) (wide-column-mode t)))


;; ;; (setq slime-lisp-implementations
;; ;;       `((sbcl ("/usr/local/bin/sbcl"))
;; ;;         (clisp ("/usr/local/bin/clisp"))))
;; ;; (add-hook 'lisp-mode-hook
;; ;;           (lambda ()
;; ;;             (cond ((not (featurep 'slime))
;; ;;                    (require 'slime)
;; ;;                    (normal-mode)))))
;; ;; (eval-after-load "slime"
;; ;;    '(slime-setup '(slime-fancy slime-banner)))


;******************************************************
;***                   Scheme                       ***
;******************************************************

(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist
      (cons '("\\.scm" . scheme-mode)
            (cons '("\\.sls" . scheme-mode)
                  (cons '("\\.ss" . scheme-mode) auto-mode-alist))))

;(put 'form-id 'scheme-indent-function number-of-special-subforms)

(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
(add-hook 'scheme-mode-hook 'balanced-on)

;; ;; (custom-set-variables '(scheme-program-name "petite"))
;; ;; (setq scheme-program-name "petite")




;; C
;; Serve per indentare bene con C e simili
(add-hook 'c-mode-common-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'lisp-mode-hook '(lambda ()
			(local-set-key (kbd "RET") 'newline-and-indent)))


;; ERLANG
;; FIXME this should use paths!
(when macosx-p
  (setq *base-erlang* "/usr/local/lib/erlang/")
  (setq *erlang-tools-version* "2.6.5")
  (push (concat  *base-erlang*
                 "lib/tools-" *erlang-tools-version*
                 "/emacs/")
        load-path)
  (setq erlang-root-dir *base-erlang*)
  (push (concat *base-erlang* "bin")
        exec-path)
  (require 'erlang-start))
