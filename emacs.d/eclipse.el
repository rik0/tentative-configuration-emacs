;	$Id: eclipse.el,v 1.20.2.2 2006/08/12 04:17:05 kish Exp $   

;;; eclipse.el --- major mode for editing and running ECLiPSe under Emacs

;; Copyright (C) 1986, 1987, 2001 - 2006
;;   Free Software Foundation, Inc.

;; Author: Thorsten Winterer <thorsten.winterer@acm.org>
;; based on the ECLiPSe mode from
;; Helmut Simonis <Helmut.Simonis@parc-technologies.com>
;; which was based on the prolog mode from
;; Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This package provides a major mode for editing ECLiPSe. It knows
;; about ECLiPSe syntax and comments, and can send regions to an inferior
;; ECLiPSe interpreter process.

;; The main difference to the previous Prolog mode is the extensive syntax
;; colouring done for ECLiPSe.
;; It supports in particular the comment/2 facility by highlighting
;; sections and pieces of the text

;; the system also knows about C-style comments
;; and indents properly after a ';'

;; Function keys:

;; TAB indents the current line.
;; M-C-\ indent a region
;; S-TAB inserts TAB character. Also \C-c<SPC>
;; C-c-p-TAB indents the current predicate
;; C-c-q-TAB indents the current clause
;; C-c-b-TAB indents the current clause

;; M-[, M-], C-c[, C-c] all call dabbrev-expand or dabbrev-completion,
;;    but check predefined ECLiPSe keywords first
;;    however, there are slight differences in there behaviour:
;; M-[ calls dabbrev-expand, and returns the predicate template
;; M-] calls dabbrev-completion, and returns the predicate template
;; C-c-[ calls dabbrev-expand, and returns the predicate name + (
;; C-c-] calls dabbrev-completion, and returns the predicate name + (

;; C-c-l loads all (local) project files mentioned in the current ECLiPSe
;;    buffer
;; C-c-C-l loads all (local) project files mentioned in any ECLiPSe buffer

;; C-l re-centres and re-fontifies
;; C-c-c comments out a region
;; C-c-r uncomments a region
;; C-c-i inverts the commenting of a region
;; C-c-C-f toggles the auto-line-break mode
;; C-c-C-j toggles the auto-indent mode

;; C-c-m-b marks the buffer
;; C-c-m-p marks the predicate
;; C-c-m-q marks the clause

;; M-C-a jumps to the next beginning of a predicate
;; M-C-e jumps to the next end of a predicate
;; M-a jumps to the next beginning of a clause
;; M-e jumps to the next end of a clause
;; C-c-C-z toggles the quick-jumps-mode

;; C-c-t inserts the template of the current predicate
;; C-c-s inserts the specs of the current predicate
;; M-RET inserts a new clause head
;; M-C-i inserts a new clause head without arguments

;; C-c-/ insert a short comment/2 template
;; C-c-\ insert a full comment/2 template

;; C-c-a anonymises the variables in the region
;; C-c-C-a replaces the variables in the region with anonymous variables

;; C-c-C-e starts an inferior ECLiPSe process
;; C-c-C-b compiles the buffer
;; C-c-C-v compiles a region
;; C-c-C-y compiles a region and switches to the ECLiPSe process
;; C-c-C-g passes a (region as) command to the ECLiPSe process
;; C-c-C-q stops the ECLiPSe process
;; C-c-C-k kills the ECLiPSe process
;; C-c-C-t starts the ECLiPSe-Tk-Tools

;; C-c-v-b checks the buffer for syntax errors by compiling
;;    it in an ECLiPSe process. The error messages are parsed and made
;;    clickable to link to the position in the source code
;;    however, this is not always possible
;; C-c-v-v checks the current region
;; C-c-v-p checks the current predicate
;; C-c-v-q checks the current clause

;; C-c-h highlights the current word
;; C-c-d removes any highlighting
;; C-c-> moves to next occurrence of highlighted word
;; C-c-< moves to last occurrence of highlighted word

;; C-c-C-h calls the ECLiPSe help function

;; C-c-l counts lines and comments in the current buffer
;; C-c-C-l counts lines and comments in all ECLiPSe buffers

;; C-c-@-@ marks the current outline subtree
;; C-c-@-n jumps to the next visible outline heading
;; C-c-@-p jumps to the previous visible outline heading
;; C-c-@-u jumps to the outline heading one level above
;; C-c-@-f jumps to the next outline heading on the same level
;; C-c-@-b jumps to the previous outline heading on the same level
;; C-c-@-h hides all predicates
;; C-c-@-t hides the current predicate
;; C-c-@-c hides all clause bodies
;; C-c-@-e hides the current clause body
;; C-c-@-l hides the current block
;; C-c-@-a shows all
;; C-c-@-s shows all predicates (synonymous with C-c-@-a)
;; C-c-@-r shows the current predicate
;; C-c-@-d shows all clauses (synonymous with C-c-@-a)
;; C-c-@-m shows the current clause
;; C-c-@-k shows the current block

;; Variables:

;; eclipse-indent-width   Describes the number of space characters inserted
;;    when increasing the indentation. the default value is 4.
;; eclipse-esp-indent-width   Describes the number of space characters inserted
;;    at the beginning of the line in ESP mode. the default value is 2.
;; eclipse-tab-width   Describes the number of space characters inserted at
;;    the beginning of a line, if its indented. The default is 8.
;; eclipse-indent-closing-parenthesis-to-match-opening   If t, the closing
;;    parenthesis always is indented to the same column as the opening
;;    parenthesis. If nil, the closing parenthesis matches either the opening
;;    parenthesis if this is a 'stand-alone' parenthesis, or the column of
;;    the first letter of the corresponding predicate call. The default is t.
;; eclipse-indent-to-parenthesis   If non-nil, indentation of the body of
;;    if-then-clauses or for-loops is calculated from the preceding opening
;;    paranthesis. Otherwise is calculated from the column of the 
;;    if-clause/for-clause. The default is t.
;; eclipse-tab-mode   If non-nil, tabs are used for indentation, otherwise
;;    space characters only. The default is nil.
;; eclipse-autolinebreak-selected   If non-nil, auto-line-break is used.
;;    The default is t.
;; eclipse-autoindent-selected   If non-nil, auto-indent is used.
;;    The default is t.
;; eclipse-quick-jumps-selected   If non-nil, quick jumps are used.
;;    The default is nil.
;; eclipse-font-lock-default  Contains the default level for the
;;    fontification. The default is 3.
;; for XEmacs: the colours for the fontification are defined in variables
;;    eclipse-*-face-val

;; There are more customisable variables, but they are less likely to be
;; changed. Check the customisation options for group eclipse.

;; There used to be a variable eclipse-backtab that described the
;;    the key-sequence for "backtab". This seems to depend on what Emacs
;;    and what GUI you use. With this key, additional tab characters
;;    (or equivalent space characters) are inserted. However, customisation
;;    did not work properly, so I removed the variable. 
;;    If [backtab] does not work, you now have to change the value directly
;;    in function eclipse-mode-commands in the line 
;;
;;    (define-key map [backtab] 'eclipse-insert-tab)
;;
;;    You can try [S-kp-tab] or [S-tab] instead. 

;; Running of ECLiPSe in an inferior mode has not been thoroughly tested,
;; I normally use the tkeclipse environment.

;; Opening the speedbar from within the ECLiPSe mode will automatically add
;; .ecl to the supported extensions.
;; If you want to load the speedbar automatically when starting Emacs, add
;;
;; (speedbar)
;; (speedbar-add-supported-extension ".ecl")
;;
;; to your .emacs file.
;; If you do not load speedbar automatically but open one before loading
;; the first ECLiPSe file, you have to add .ecl to the list of supported
;; extensions by either calling (speedbar-add-supported-extension ".ecl")
;; or add
;;
;;(custom-set-variables
;;  '(speedbar-supported-extension-expressions
;;     (quote (".ecl" <whatever is in the variable now>))))
;;
;; to your .emacs file.
;;
;;
;; Add the following lines to your .emacs, changing <PATH> to the path where
;; this eclipse.el can be found:
;;
;; (autoload 'eclipse-mode "<PATH>/eclipse.el" "ECLiPSe editing mode" t)
;; (autoload 'eclipse-esp-mode "<PATH>/eclipse.el" "ECLiPSe-ESP editing mode" t)
;; (setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.esp" . eclipse-esp-mode) auto-mode-alist))


;; This version has been tested on emacs 21.3 for Linux
;; Your mileage may vary.


;; New:
;; ESP editing mode
;; Metrics

;;; NOTE: If there is a problem with entering commands in the inferior 
;;; ECLiPSe process window, disable the line
;;;               (define-key map "\r" 'eclipse-next-line)
;;; in the definition of function eclipse-mode-commands


;; To do:
;; - wait for the new ECLiPSe compiler, so that source code tracing can be added
;; - code clean-up


;;; Code:

;; what Emacs is it?

(defvar eclipse-emacs-21 (equal (substring (version) 0 12) "GNU Emacs 21"))
(defvar eclipse-xemacs (equal (substring (version) 0 6) "XEmacs"))

;;
;; Definitions
;;

(defvar eclipse-mode-syntax-table nil)
(defvar eclipse-esp-mode-syntax-table nil)
(defvar eclipse-mode-abbrev-table nil)
(defvar eclipse-mode-map nil)

(defgroup eclipse nil
  "Major mode for editing and running ECLiPSe under Emacs."
  :group 'languages)

(defconst eclipse-mode-version 7)

(defvar eclipse-version 0.0
  "Variable is set when ECLiPSe process is started")

;; path definitions and program calls

(defcustom eclipse-path "/Users/enrico/eclipse/bin/i386_macosx/"
  "Path where ECLiPSe can be found.

Change only, if ECLiPSe path is not in environment variable PATH"
  :type 'string
  :group 'eclipse)

(defconst eclipse-program-name "eclipse"
  "Program name for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-version-call "get_flag(version,Version).\n"
  "ECLiPSe command to get the version number.")

(defconst eclipse-tktools-name "tktools"
  "Program name for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-program-call
  (concat eclipse-path eclipse-program-name)
  "Program call for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-tktools-call
  (concat eclipse-path eclipse-tktools-name)
  "Program call for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-name "remote_tools"
  "ECLiPSe library for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-pred "lib"
  "How remote_tools.pl shall be loaded: as library (lib) or module
(use_module).")

(defconst eclipse-tktools-lib-call
  (concat eclipse-tktools-lib-pred "(" eclipse-tktools-lib-name "), ")
  "ECLiPSe tktools library call.")

(defconst eclipse-54-tktools-call 
  (concat eclipse-tktools-lib-call "attach_tools(Host/Port,block,writeln([Host,Port])).\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.4 and later.

The first parameter of attach_tools/3 returns the Host and Port, 
the second parameter is the timeout in seconds (or 'block' for no timeout),
the third parameter is a predicate called after establishing the connection.")

(defconst eclipse-53-tktools-call
  (concat eclipse-tktools-lib-call "attach_tools.\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.3 and earlier.")

(defconst eclipse-run-tktools-func 'eclipse-run-tktools
  "Elisp function to extract Host and Port values from output and start
tktools.

Is added to 'comint-preoutput-filter-functions, and must remove itself from
this list when the output line containing host and port is processed.")

(defconst eclipse-reconsult-string "reconsult(user).\n"
  "*(Re)Consult mode (for C-Prolog and Quintus Prolog).")

(defconst eclipse-consult-string "consult(user).\n"
  "*Consult mode.")

(defconst eclipse-compile-string "compile(user).\n"
  "*Compile mode.")

(defconst eclipse-eof-string "end_of_file.\n"
  "*String that represents end of file for eclipse.
nil means send actual operating system end of file.")

(defconst eclipse-halt-string "halt.\n"
  "*Command that stops the eclipse process.")

(defconst eclipse-help-call1 
  (concat eclipse-program-call " -e \"help(")
  "First part of help call to ECLiPSe system.")

(defconst eclipse-help-call2 ").\""
  "Second part of help call to ECLiPSe system.")

;; indentation definitions

(defcustom eclipse-indent-width 4
  "Standard additional indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-esp-indent-width 2
  "Standard indentation in ECLiPSe-ESP buffers."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-tab-width 8
  "Minimum indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defvar eclipse-old-tab-width eclipse-tab-width)

(defcustom eclipse-indent-mode nil
  "If set to t, indentation will always increase/decrease by
`eclipse-indent-width'."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-closing-parenthesis-to-match-opening t
  "If set to t, indentation will indent closing parentheses to the
same column as the matching opening parentheses."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-to-parenthesis t
  "Indentation if if-then-clauses and for-loops calculated from column of
preceding opening parenthesis."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-first-line-std-indent t
  "Always indent the first line of a predicate using `eclipse-tab-width'."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-tab-mode nil
  "Indentation in ECLiPSe buffers with spaces or tabs?
Set this variable to nil to insert only space characters.
To change the behaviour during editing, use \\[eclipse-tab-mode-toggle]."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-autolinebreak-selected t
  "Automatic line-break in ECLiPSe buffer."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-autoindent-selected t
  "Automatic indentation in ECLiPSe buffer."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-clause-heads nil
  "If t, clause heads will be indented to column 0.
If nil, indentation of clause heads will not be changed."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-quick-jumps-selected nil
  "If t, the 'go to' commands determine the place to jump to by the next
empty line. If nil, the correct place to jump to is computed correctly, but
this may be slow if the buffer text is long."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-timeout 2
  "Timeout for indentation in seconds."
  :type 'integer
  :group 'eclipse)

;; speedbar support definitions

(defvar eclipse-speedbar-selected nil)
  ;; Variable to store the status of the speedbar.
  ;; If t, the speedbar is running, if nil, the speedbar is off.

(defvar eclipse-speedbar-supported nil)
  ;; Variable is t, if speedbar supports .ecl extension.

(defconst eclipse-imenu-generic-expression
  (list (list nil (purecopy "^\\([a-z].*\\)\\([:?]-\\|\n\\)") 1)
 	(list "Directives" "^\\([ \t]*[:?]-[ \t]*\\)\\([a-z].*\\)\\(.\\|\n\\)" 2))
  "Imenu generic expression for ECLiPSe mode. See `imenu-generic-expression'.")

(defconst eclipse-imenu-prev-index-position-function
  ;; my own function to find the previous index function
  'eclipse-goto-prev-index-position)

(defconst eclipse-imenu-create-index-function
  ;; my own function to create the imenu index
  'eclipse-create-index)

(defconst eclipse-imenu-extract-index-name-function
  ;; my own function to extract the name for the index function
  'eclipse-extract-index-name)

;; highlighting definitions

(defvar eclipse-overlays nil)
  ;; list of highlighting overlays

(defvar eclipse-highlighted nil)
  ;; currently highlighted word

(defcustom eclipse-highlight-face-bg-val "cornflower blue"
    "Type face background for highlighting."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-highlight-face-fg-val "white"
    "Type face foreground for highlighting."
    :type 'color
    :group 'eclipse)

;; make face for highlighting
(make-face 'eclipse-highlight-face)
(cond (eclipse-xemacs
       (set-face-property 'eclipse-highlight-face
			  'background eclipse-highlight-face-bg-val)
       (set-face-property 'eclipse-highlight-face
			  'foreground eclipse-highlight-face-fg-val))
      (t
       (set-face-background 'eclipse-highlight-face
			    eclipse-highlight-face-bg-val)
       (set-face-foreground 'eclipse-highlight-face
			    eclipse-highlight-face-fg-val)))

;; font lock definitions

(defcustom eclipse-font-lock-default 3
  "The default level for the fontification."
  :type 'integer
  :group 'eclipse)  

(when eclipse-xemacs
  ;; set colours to GNU-Emacs-like values!
  (defcustom eclipse-builtin-face-val "magenta"
    "Builtin face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-warning-face-val "red"
    "Warning face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-keyword-face-val "purple"
    "Keyword face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-function-name-face-val "blue"
    "Function name face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-variable-name-face-val "darkorange"
    "Variable name face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-comment-face-val "red3"
    "Comment face colour for XEmacs."
    :type 'color
    :group 'eclipse)
  ;; for whatever reason, XEmacs does not always use this value...
  (defcustom eclipse-string-face-val "salmon"
    "String face colour for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-constant-face-val "aquamarine4"
    "Constant face for XEmacs."
    :type 'color
    :group 'eclipse)
  (defcustom eclipse-type-face-val "forestgreen"
    "Type face for XEmacs."
    :type 'color
    :group 'eclipse)
  ;; create the faces that XEmacs doesn't know
  (make-face 'font-lock-builtin-face)
  (make-face 'font-lock-constant-face)
  (make-face 'font-lock-warning-face)
  (make-face 'font-lock-keyword-face)
  (make-face 'font-lock-function-name-face)
  (make-face 'font-lock-variable-name-face)
  (make-face 'font-lock-comment-face)
  (make-face 'font-lock-string-face)
  (make-face 'font-lock-type-face)
  (set-face-property 'font-lock-builtin-face
		     'foreground eclipse-builtin-face-val)
  (set-face-property 'font-lock-warning-face
		     'foreground eclipse-warning-face-val)
  (set-face-property 'font-lock-keyword-face
		     'foreground eclipse-keyword-face-val)
  (set-face-property 'font-lock-function-name-face
		     'foreground eclipse-function-name-face-val)
  (set-face-property 'font-lock-variable-name-face
		     'foreground eclipse-variable-name-face-val)
  (set-face-property 'font-lock-comment-face
		     'foreground eclipse-comment-face-val)
  (set-face-property 'font-lock-string-face
		     'foreground eclipse-string-face-val)
  (set-face-property 'font-lock-constant-face
		     'foreground eclipse-constant-face-val)
  (set-face-property 'font-lock-type-face
		     'foreground eclipse-type-face-val))

;; create syntax table
(defun eclipse-create-syntax-table (esp-flag)
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?\' "." table)
    (cond (esp-flag
	   (modify-syntax-entry ?\? ". 14b" table)
	   (modify-syntax-entry ?< ". 3b" table)
	   (modify-syntax-entry ?> ". 2b" table)
	   (modify-syntax-entry ?/ "." table)
	   (modify-syntax-entry ?* "." table))
	  (t
	   (modify-syntax-entry ?/ ". 14b" table)
	   (modify-syntax-entry ?* ". 23b" table)
	   (modify-syntax-entry ?< "." table)
	   (modify-syntax-entry ?> "." table)))
    table))

(unless eclipse-mode-syntax-table
  (setq eclipse-mode-syntax-table (eclipse-create-syntax-table nil)))

(unless eclipse-esp-mode-syntax-table
  (setq eclipse-esp-mode-syntax-table (eclipse-create-syntax-table t)))

(define-abbrev-table 'eclipse-mode-abbrev-table ())

;;
;; Font lock regexps
;;

(defconst eclipse-font-lock-colondash
  (list 
   ;; :- not necessarily at end of line
   '("[:?]-" 0 font-lock-builtin-face)))

(defconst eclipse-font-lock-basic
  (list
   ;; quoted atoms
   '("'\\(\\\\'\\|[^'\n]\\)*'" 0 font-lock-string-face)
   ;; show variables
   '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-variable-name-face)
   ;; predicate definitions (that can be fontified) always start at bol
   '("^[a-z][a-zA-Z0-9_]*" 0 font-lock-function-name-face)
   ;; critical builtins with arity 0 or prefix op
   ;; special for cut, not   (\\?-\\s-*$)???
   '("\\(\\<\\(abort\\|exit\\|fa\\(il\\|lse\\)\\|halt\\|make\\|not\\|pause\\)\\>\\|!\\|\\\\\\+\\)" 0 font-lock-warning-face)
   '("[ \t(]\\(~\\)[ \t(]" 1 font-lock-warning-face)
   ;; true
   '("\\<true\\>" 0 font-lock-constant-face)))

(defconst eclipse-font-lock-basic-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\([:?]-\\)\\s-*$\\|\\<is\\>\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|>=\\|=<\\|<=\\|<\\|=\\|\\\\=\\|~=\\|[ \t(]\\([-*+/^]\\|//\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(-?>\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\(\\<do\\|[:?]-\\)\\s-*$\\|\\<is\\>\\|[*]?->\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|#<=>\\|#<=\\|#\\\\\\+\\|[#$&]?\\(>=\\|>\\|=<\\|<\\|=\\)\\|@\\(>=\\|>\\|=<\\|<\\)\\|[#$]\\\\=\\|#\\(#\\|\\\\/\\|/\\\\\\|::\\)\\|#\\|[$&`]::\\|::\\|`<>\\|`\\(<\\|=\\)\\|~=<\\|~=\\|[ \t(]\\([-*+/^~]\\|//\\|/\\\\\\|<<\\|>>\\|\\\\/\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(->\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\<\\(once\\|nl\\|do\\)\\>" 0 font-lock-builtin-face)
   '("\\<\\(a\\(bs\\|cos\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\)?\\)\\|b\\(ag\\(_\\(create\\|dissolve\\|enter\\)\\|of\\)\\|etween\\|ind\\|lock\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_c\\)?\\|nonical_path_name\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\)\\|o\\(mp\\(are\\|ile\\(_\\(stream\\|term\\)\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|reate_module\\)\\|d\\(ate\\|e\\(l\\(ayed_goals\\(_number\\)?\\)\\|mon\\|i\\(m\\|splay\\)\\|omain\\)\\|e\\(val\\|x\\(\\|i\\(sts\\|t_block\\)\\|p\\(and_goal\\)?\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(et\\(_\\(char\\|flag\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\)?\\|round\\)\\|help\\|i\\(n\\(stance\\|teger\\(_atom\\)?\\)\\)\\|join_string\\|k\\(eysort\\|ill\\)\\|l\\(ength\\|n\\|o\\(ad\\|oop_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\)\\|in\\|od\\|ultifor\\)\\|n\\(l\\|o\\(n\\(ground\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_string\\)?\\)\\)\\|o\\(pen\\|nce\\)\\|p\\(a\\(ram\\|thname\\)\\|ipe\\|lus\\|r\\(ed\\|intf?\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\)\\|e\\(a\\(d\\(_\\(directory\\|string\\|token\\)\\|var\\)\\|[dl]\\)\\|corded\\(_list\\)?\\|verse\\)\\|ound\\)\\|s\\(etof\\|h\\(elf_\\(abolish\\|create\\|get\\|set\\)\\)?\\|in\\|ort\\)\\|plit_string\\|qrt\\|t\\(ring\\(_l\\(ength\\|ist\\)\\)?\\)\\|u\\(b\\(call\\|string\\)\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(string\\|variables\\)\\|ype_of\\)\\|var\\|w\\(rite\\(clause\\|ln\\|q\\)?\\)\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-medium-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\([ \t]^[ \t]\\|\\<\\(env\\|garbage_collect\\|listing\\|once\\|nl\\|do\\|peer_multitask_\\(confirm\\|terminate\\)\\|statistics\\|trimcore\\)\\>\\)" 0 font-lock-builtin-face)
   '("\\<\\('C'\\|a\\(bs\\|c\\(cept\\|os\\|yclic_term\\)\\|dd_attribute\\|l\\(arm\\|s\\)\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\|tached_suspensions\\)?\\)\\|b\\(ag\\(_\\(abolish\\|c\\(ount\\|reate\\)\\|dissolve\\|e\\(nter\\|rase\\)\\|retrieve\\)\\|of\\)\\|etween\\|ind\\|lock\\|real\\(_\\(bounds\\|from_bounds\\|m\\(ax\\|in\\)\\)\\)?\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_\\(c\\|priority\\)\\)?\\|n\\(cel_after_event\\|onical_path_name\\)\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\|rbit\\)\\|o\\(mp\\(are\\(_instances\\)?\\|ile\\(_\\(stream\\|term\\)\\|d_stream\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|urrent_\\(a\\(fter_events?\\|rray\\|tom\\)\\|built_in\\|compiled_file\\|domain\\|error\\|functor\\|host\\|interrupt\\|m\\(acro\\|odule\\(_predicate\\)?\\)\\|op\\|pr\\(agma\\|edicate\\)\\|record\\|s\\(t\\(ore\\|r\\(eam\\|uct\\)\\)\\|uspension\\)\\|trigger\\)\\)\\|d\\(ate\\|e\\(layed_goals\\(_number\\)?\\|mon\\|nominator\\|precated\\)\\|i\\(m\\|splay\\)\\|omain_index\\)\\|e\\(nter_suspension_list\\|rr\\(no_id\\|or_id\\)\\|v\\(al\\|vent\\(_\\(create\\|disable\\|enable\\|retrieve\\)\\|s_\\(defer\\|nodefer\\)\\)\\)\\|x\\(ec\\(_group\\)?\\|i\\(st\\(ing_file\\|s\\)\\|t_block\\)\\|p\\(and_\\(clause\\|goal\\|macros\\)\\)?\\|ternal\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(atten\\(_array\\)?\\|o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\|k\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(cd\\|et\\(_\\(ch\\(ar\\|tab\\)\\|e\\(rror_handler\\|vent_handler\\)\\|f\\(ile_info\\|lag\\)\\|interrupt_handler\\|leash\\|module_info\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\|bit\\|cwd\\|env\\|val\\)?\\|round\\)\\|help\\|i\\(n\\(stance\\|teger\\(s\\|_atom\\)\\)\\|s_\\(built_in\\|dynamic\\|event\\|handle\\|list\\|predicate\\|record\\|suspension\\)\\)\\|join_string\\|keysort\\|l\\(cm\\|ength\\|isten\\|n\\|o\\(ad\\|c\\(cal_time\\(_string\\)?\\|k\\)\\|op_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\|ta\\(_\\(attribute\\|bind\\)\\)?\\)\\|in\\|kdir\\|od\\|sort\\|u\\(ltifor\\|tex\\(_init\\)?\\)\\)\\|n\\(ame\\|ew_socket_server\\|l\\|o\\(n\\(ground\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_\\(merge\\|s\\(ort\\|tring\\)\\)\\)?\\|erator\\)\\)\\|o\\(ccurs\\|nce\\|pen\\|s_file_name\\)\\|p\\(a\\(ram\\|thname\\)\\|eer\\(_\\(d\\(eregister_multitask\\|o_multitask\\)\\|get_property\\|multitask_\\(confirm\\|terminate\\)\\|queue_\\(c\\(lose\\|reate\\)\\|get_property\\)\\|register_multitask\\)\\)?\\|hrase\\|ipe\\|lus\\|ortray_\\(goal\\|term\\)\\|r\\(ed\\|intf?\\|ofile\\|une_instances\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\(ize\\)?\\)\\|e\\(a\\(d\\(_\\(directory\\|exdr\\|string\\|t\\(erm\\|oken\\)\\)\\|var\\)?\\|l\\)\\|corded\\(_list\\)?\\|ferenced_record\\|mote_\\(connect\\(_\\(accept\\|setup\\)\\)?\\|disconnect\\|yield\\)\\|name\\|verse\\)\\|ound\\)\\|s\\(e\\(e[dk]\\|lect\\|t\\(_stream\\(_property\\)?\\|of\\)\\)\\|gn\\|h\\(elf_\\(abolish\\|create\\|dec\\|get\\|inc\\|set\\)\\)?\\|in\\|leep\\|o\\(cket\\|rt\\)\\|plit_string\\|qrt\\|t\\(atistics\\|ore\\(_\\(c\\(o\\(ntains\\|unt\\)\\|reate\\)\\|delete\\|erase\\|get\\|inc\\|set\\)\\|d_keys\\(_and_values\\)?\\)?\\|r\\(eam_truncate\\|ing\\(_\\(code\\|l\\(ength\\|ist\\)\\)\\)?\\)\\)\\|u\\(b\\(call\\|s\\(cript\\|tring\\)\\)\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(hash\\|string\\|to_bytes\\|variables\\)\\|imes\\|ool_body\\|y\\(pe_of\\|[io]\\)\\)\\|un\\(get\\|lock\\)\\|var\\(ia\\(ble\\|nt\\)\\)?\\|w\\(ait\\|rite\\(_\\(canonical\\|exdr\\|term\\)\\|clause\\|ln\\|q\\)?\\)\\|xor\\|yield\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-low
  (list
   ;; critical builtins
   '("\\<\\(a\\(ssert[az]?\\|ttach_suspensions\\)\\|call_priority\\|de\\(cval\\|fine_macro\\|lete\\)\\|e\\(r\\(ase\\(_\\(a\\(ll\\|rray\\)\\|m\\(acro\\|odule\\)\\)\\)?\\|ror\\)\\|vent\\(_\\(after\\(_every\\)?\\|create\\|retrieve\\)\\|s_after\\)?\\)\\|in\\(c\\(lude\\|val\\)\\|it_suspension_list\\|sert_suspension\\)\\|kill\\(_suspension\\)?\\|m\\(ake_suspension\\|erge_suspension_list\\)\\|notify_constrained\\|re\\(cord[az]?\\|record\\|set_e\\(vent\\|rror\\)_handler\\|tract\\(_all\\)?\\)\\|s\\(chedule_suspensions\\|et\\(_\\(chtab\\|default_error_handler\\|e\\(rror_handler\\|vent_handler\\)\\|flag\\|interrupt_handler\\|suspension_data\\|var_bounds\\)\\|arg\\|bit\\|val\\)\\|uspend\\)\\|t\\(est_and_setval\\|rigger\\)\\|update_struct\\|x\\(get\\|set\\)\\)(" 1 font-lock-warning-face)
   '("\\<\\(reset_error_handlers\\|trimcore\\)\\>" 0 font-lock-warning-face)))

(defconst eclipse-font-lock-medium
  (list
   ;; base operator
   '("\\([0-9]\\)\\('\\)\\([a-zA-Z0-9]+\\)" 2 font-lock-builtin-face)
   ;; directives
   '("^:-\\s-*\\(comment\\|d\\(emon\\|ynamic\\)\\|export\\|import\\|p\\(arallel\\|ragma\\)\\|reexport\\)\\>" 0 font-lock-keyword-face)
   '("\\<\\(comment\\|d\\(emon\\|ynamic\\)\\|export\\|import\\|p\\(arallel\\|ragma\\)\\|reexport\\|local\\|global\\)(" 1 font-lock-keyword-face)
   ;; highlight mode/tool declaration
   '("^\\(:-\\s-*mode\\)\\([^.]*\\)\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
   '("^\\(:-\\s-*tool\\)(\\([a-z][a-zA-Z0-9_]*/[0-9]+\\),[ ]*\\([a-z][a-zA-Z0-9_]*/[0-9]+\\))\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face)
     (3 font-lock-constant-face))
   '("^:-\\s-*\\(ensure_loaded\\|inline\\|mode\\|tool\\)\\>" 0 font-lock-keyword-face)
   ;; module names & structures
   '("^\\(:-\\s-*\\(module\\|module_interface\\|begin_module\\|create_module\\|use_module\\|lib\\)\\)(\\([a-zA-Z_0-9]+\\))"
     (1 font-lock-keyword-face) (3 font-lock-constant-face))
   '("\\<\\(module\\|module_interface\\|begin_module\\|create_module\\|use_module\\|lib\\)(\\([a-zA-Z_0-9]+\\))"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
;   '("^\\(:-\\s-*\\(ensure_loaded\\)(\\([a-zA-Z_0-9. ']+\\))"
;     (1 font-lock-keyword-face)) ; (2 font-lock-constant-face))
   '("^\\(:-\\s-*\\(local\\|global\\|export\\)\\)[ \t\n]*\\(s\\(helf\\|t\\(ore\\|ruct\\)\\|yntax_option\\)\\|macro\\|op\\|portray\\|re\\(cord\\|ference\\)\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face t) (3 font-lock-keyword-face t))
   '("\\<\\(s\\(helf\\|truct\\|yntax_option\\)\\|macro\\|op\\|portray\\|reference\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face t))
   '("\\<\\(store\\|record\\)([a-z][a-zA-Z0-9_]*)"
     (1 font-lock-keyword-face t))
   '("^\\(:-\\s-*local\\|global\\)[ \t]"
     (1 font-lock-keyword-face))
   ;; import from module
   '("\\(from\\)\\s-+\\([a-z0-9_]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
   ;; special case for structures   
   '("\\<[a-z][a-zA-Z0-9_]*[ \t]+\\(of [a-z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-constant-face)
   '("\\<[a-z][a-zA-Z0-9_]*[ \t]+with\\>" 0 font-lock-constant-face)
   '("\\<\\([a-z][a-zA-Z0-9_]*\\){" 1 font-lock-constant-face)
   ;; structure elements and calls from other module
   '("\\<[a-z][a-zA-Z0-9_]*:" 0 font-lock-constant-face)
   ;; calls "as if inside other module"
   '("@[ \t]*[A-Za-z0-9_]+" 0 font-lock-constant-face)
   ;; critical builtins with arity 0 or prefix op
   '("\\(\\<\\(re\\(peat\\|set_error_handlers\\)\\|wake\\)\\>\\|-->\\|-\\?->\\|\\?-\\)" 0 font-lock-warning-face)
   ;; critical builtins
   '("\\<\\(arr_create\\|current_array\\|erase_array\\|local_array\\|meta_attribute\\)(" 1 font-lock-warning-face)
   ;; debugging
   '("\\<\\(debug\\(_compile\\|ging\\)?\\|no\\(debug\\|spy\\|trace\\)\\|s\\(kipped\\|py\\(_\\(term\\|var\\)\\)?\\)\\|trace\\(_exit_port\\)?\\)\\>" 0 font-lock-constant-face)
   '("\\<\\(debug\\|[gs]et_leash\\|leash\\|\\(kill\\|make\\)_display_matrix\\|trace\\(_\\(call\\|parent\\|point\\)port\\|able\\)?\\|un\\(skipped\\|traceable\\)\\)(" 1 font-lock-constant-face)
   ;; some other stuff: constants, etc.
   '("\\(\\<\\(a\\(fter_event_timer\\|ll\\(sols\\|_dynamic\\)\\|nti_first_fail\\|rrays\\)\\|b\\(ased_bignums\\|bs\\|lanks_in_nil\\|rea\\(k_level\\|l_exceptions\\)\\)\\|c\\(o\\(mplete\\|ntrol\\|routine\\)\\|redit\\|wd\\)\\|d\\(bs\\|e\\(bug\\(_compile\\|ging\\)\\|fault_language\\|nse_output\\)\\|oubled_quote_is_quote\\)\\|e\\(clipse_\\(info\\|object\\)_suffix\\|n\\(able_interrupts\\|d_of_file\\)\\|xtension\\)\\|f\\(irst_fail\\|loat_precision\\)\\|g\\(c\\(_\\(interval\\(_dict\\)?\\|policy\\)\\)?\\|oal_expansion\\)\\|host\\(arch\\|id\\|name\\)\\|i\\(gnore_eof\\|n\\(domain\\(_\\(interval\\|m\\(ax\\|edian\\|i\\(ddle\\|n\\)\\)\\|random\\|split\\)\\)?\\|put_order\\|stallation_directory\\)\\|so_\\(base_prefix\\|escapes\\)\\)\\|l\\(a\\(rgest\\|st_errno\\)\\|ds\\|i\\(brary_path\\|mit_arg_precedence\\)\\|oaded_library\\)\\|m\\(a\\(cro_expansion\\|x_\\(global_trail\\|local_control\\|predicate_arity\\|regret\\)\\)\\|ost_constrained\\)\\|n\\(ested_commentsl_in_quotes\\|o_\\(array_subscripts\\|blanks\\)\\)\\|o\\(bject_suffix\\|ccur\\(_check\\|rence\\)\\|utput_mode\\)\\|p\\(p?id\\|r\\(efer_rationals\\|int_depth\\|olog_suffix\\)\\)\\|remote_protocol_version\\|s\\(mallest\\|yntax_option\\)\\|t\\(mp_dir\\|oplevel_module\\)\\|unix_time\\|v\\(ariable_names\\|ersion\\(_as_list\\)?\\)\\|w\\(m_window\\|orker\\(ids\\|s\\)?\\)\\)\\)\\([ \t\n,)]\\)" 1 font-lock-constant-face)
   ;; 'with attributes'/2
   '("'with attributes'" 0 font-lock-builtin-face t)))

(defconst eclipse-comment-font-lock
  (list
   ;; fontification of comment predicate
   '("\\(^\\|comment(.*\\)\\s-*[[]?\\s-*\\(a\\(lias\\|mode\\|rgs\\|uthor\\)\\|copyright\\|d\\(ate\\|esc\\)\\|e\\(g\\|xceptions\\)\\|f\\(ail_if\\|ields\\)\\|in\\(clude\\|dex\\)\\|resat\\|s\\(ee_also\\|ummary\\)\\|template\\)\\s-*[,:]" 2 font-lock-type-face)
   ;; special case for comment/2 predicate
   '("^:-\\s-*comment(\\([a-z_][a-z0-9_]*\\s-*/\\s-*[0-9]+\\)" 1 font-lock-function-name-face)
   ;; predicate definitions in comment/2
   '("\\(<[Pp]>\\|</[Pp]>\\|<[Bb]>\\|</[Bb]>\\|<[Ll][Ii]>\\|</[Ll][Ii]>\\|<[Uu][Ll]>\\|</[Uu][Ll]>\\|<[Aa][^>]*>\\|</[Aa]>\\|<[Ii]>\\|</[Ii]>\\|<[Dd][Ll]>\\|</[Dd][Ll]>\\|<[Dd][Tt]>\\|</[Dd][Tt]>\\|<[Dd][Dd]>\\|</[Dd][Dd]>\\|<[Tt][Tt]>\\|</[Tt][Tt]>\\|<[Ee][Mm]>\\|</[Ee][Mm]>\\|<[Pp][Rr][Ee]>\\|</[Pp][Rr][Ee]>\\)" 0 font-lock-function-name-face t)
   ;; override html markup in strings and comments
   ;; show variables in args field of comment, overrides comments
   '("\\(^\\s-*\\|[[]\\)\"\\([_A-Z][a-zA-Z0-9_]*\\)\"\\s-*:" 2 font-lock-variable-name-face t))
  "Font lock description of additional comment/2 expressions.")

(defconst eclipse-esp-font-lock
  (list
   '("^\\s-*\\(<[?]\\)\\(esp\\)\\(:[^ \t\n]*\\)[ \t\n]"
     (1 font-lock-comment-face t) (2 font-lock-function-name-face t)
     (3 font-lock-constant-face t))
   '("^\\s-*\\(<[?]\\)\\(esp\\)[ \t\n]"
     (1 font-lock-comment-face t) (2 font-lock-function-name-face t)))
  "Font lock description of esp expressions.")

(defconst eclipse-font-lock-keywords-1
  (append
   eclipse-font-lock-basic
   eclipse-font-lock-basic-infix
   eclipse-font-lock-colondash)
  "Basic (Prolog) expressions for font-lock mode.")

(defconst eclipse-font-lock-keywords-2
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-low-builtins
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)
  "Essential ECLiPSe expressions for font lock mode.")

(defconst eclipse-font-lock-keywords-3
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-medium-builtins
   eclipse-font-lock-medium
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)   
  "Highlights ECLiPSe expressions except comment/2.")

(defconst eclipse-font-lock-keywords-4
 (append
   eclipse-font-lock-keywords-3
   eclipse-comment-font-lock)
  "Highlights all ECLiPSe expressions.")

(defconst eclipse-font-lock-keywords-5
  (append
   eclipse-font-lock-keywords-3
   eclipse-esp-font-lock)
  "Highlights ECLiPSe expressions in ESM mode.")

(defconst eclipse-font-lock-keywords
   (cond ((= eclipse-font-lock-default 0) nil)
	 ((= eclipse-font-lock-default 1) eclipse-font-lock-keywords-1)
	 ((= eclipse-font-lock-default 2) eclipse-font-lock-keywords-2)
	 ((= eclipse-font-lock-default 3) eclipse-font-lock-keywords-3)
	 ((= eclipse-font-lock-default 4) eclipse-font-lock-keywords-4)
	 ((= eclipse-font-lock-default 5) eclipse-font-lock-keywords-5))
  "Additional expressions to highlight in ECLiPSe mode.")

(if eclipse-xemacs
    (put 'eclipse-mode 'font-lock-keywords '(eclipse-font-lock-keywords nil))
  (put 'eclipse-mode 'font-lock-defaults '(eclipse-font-lock-keywords nil)))

;;
;; Mode map
;;

(defun eclipse-mode-variables (&optional esp)
  (if esp
      (set-syntax-table eclipse-esp-mode-syntax-table)
    (set-syntax-table eclipse-mode-syntax-table))
  (setq local-abbrev-table eclipse-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function) 'eclipse-indent-line)
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (setq case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression) eclipse-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist) '(("+-*/.<>=?!$%_&~^:" . "w")))
  (set (make-local-variable 'imenu-prev-index-position-function) eclipse-imenu-prev-index-position-function)
  (set (make-local-variable 'imenu-extract-index-name-function) eclipse-imenu-extract-index-name-function)
  (set (make-local-variable 'comment-indent-function) 'eclipse-indent-line)
  (set (make-local-variable 'imenu-create-index-function) eclipse-imenu-create-index-function)
  (setq imenu-sort-function 'imenu--sort-by-name))

(defun eclipse-mode-commands (map)
  "Contains the key-bindings for the major ECLiPSe mode.
The following commands are available:

\\{eclipse-mode-map}"
  (define-key map "\r" 'eclipse-next-line)
  (define-key map "\t" 'eclipse-indent-line)
  (define-key map "\M-\C-\\" 'eclipse-indent-region)
; M-TAB seems to be the same as C-M-i !?
;  (define-key map "\M-\t" 'eclipse-indent-region)
  (define-key map "\M-[" 'eclipse-dabbrev-expand)
  (define-key map "\M-/" 'eclipse-dabbrev-expand0)
  (define-key map "\M-]" 'eclipse-dabbrev-completion)
  (define-key map "\C-c[" 'eclipse-dabbrev-expand1)
  (define-key map "\C-c]" 'eclipse-dabbrev-completion1)
  (define-key map "\C-c\C-l" 'eclipse-load-all-modules)
  (define-key map "\C-cl" 'eclipse-load-modules)
  (define-key map "\C-cb\t" 'eclipse-indent-buffer)
  (define-key map "\C-cp\t" 'eclipse-indent-predicate)
  (define-key map "\C-cq\t" 'eclipse-indent-clause)
; <backtab> seems to be defined differently in almost every window
; manager. so you've got to customize it...
  (define-key map "\C-c " 'eclipse-insert-tab)
  (define-key map [backtab] 'eclipse-insert-tab)
  (define-key map "\C-l" 'eclipse-recenter)
  (define-key map "\C-c\C-e" 'run-eclipse)
  (define-key map "\C-c\C-q" 'stop-eclipse)
  (define-key map "\C-c\C-k" 'kill-eclipse)
  (define-key map "\C-c\C-t" 'eclipse-start-tools)
  (define-key map "\C-c\C-g" 'eclipse-run-region)
  (define-key map "\C-c\C-b" 'eclipse-compile-buffer)
  (define-key map "\C-c\C-v" 'eclipse-compile-region)
  (define-key map "\C-c\C-y" 'eclipse-compile-region-and-go)
  (define-key map "\C-cvb" 'eclipse-check-buffer)
  (define-key map "\C-cvv" 'eclipse-check-region)
  (define-key map "\C-cvp" 'eclipse-check-predicate)
  (define-key map "\C-cvq" 'eclipse-check-clause)
  (define-key map "\C-c\C-h" 'eclipse-call-help)
  (define-key map "\C-cc" 'eclipse-comment-region)
  (define-key map "\C-cr" 'eclipse-uncomment-region)
  (define-key map "\C-ci" 'eclipse-invert-comment-region)
  (define-key map "\C-c\C-f" 'eclipse-autolinebreak-toggle)
  (define-key map "\C-c\C-j" 'eclipse-autoindent-toggle)
  (define-key map "\C-ca" 'eclipse-anonymise-variables)
  (define-key map "\C-c\C-a" 'eclipse-anonymous-variables)
  (define-key map "\C-cmb" 'eclipse-mark-buffer)
  (define-key map "\C-cmp" 'eclipse-mark-predicate)
  (define-key map "\C-cmq" 'eclipse-mark-clause)
  (define-key map "\M-\C-a" 'eclipse-goto-predicate-begin)
  (define-key map "\M-\C-e" 'eclipse-goto-predicate-end)
  (define-key map "\M-a" 'eclipse-goto-clause-begin)
  (define-key map "\M-e" 'eclipse-goto-clause-end)
  (define-key map "\C-c\C-z" 'eclipse-quick-jumps-toggle)
  (define-key map "\C-ct" 'eclipse-insert-predicate-template)
  (define-key map "\C-cs" 'eclipse-insert-predicate-spec)
  (define-key map "\C-c/" 'eclipse-insert-comment-pred-short)
  (define-key map "\C-c\\" 'eclipse-insert-comment-pred-full)
  (define-key map "\M-\C-m" 'eclipse-insert-clause-head)
  (define-key map "\M-\C-i" 'eclipse-insert-clause-head-empty)
  (define-key map "\C-ch" 'eclipse-highlight)
  (define-key map "\C-cd" 'eclipse-dehighlight)
  (define-key map "\C-c>" 'eclipse-goto-highlight-forward)
  (define-key map "\C-c<" 'eclipse-goto-highlight-backward)
  (define-key map "\C-cl" 'eclipse-display-metrics)
  (define-key map "\C-c\C-l" 'eclipse-display-metrics-all))

(defun eclipse-outline-define-map (map)
  (define-key map "\C-c@@" 'eclipse-outline-mark-subtree)
  (define-key map "\C-c@n" 'eclipse-outline-next-visible-heading)
  (define-key map "\C-c@p" 'eclipse-outline-previous-visible-heading)
  (define-key map "\C-c@u" 'eclipse-outline-up-heading)
  (define-key map "\C-c@f" 'eclipse-outline-forward-same-level)
  (define-key map "\C-c@b" 'eclipse-outline-backward-same-level)
  (define-key map "\C-c@h" 'eclipse-hide-predicates)
  (define-key map "\C-c@t" 'eclipse-hide-predicate)
  (define-key map "\C-c@c" 'eclipse-hide-clauses)
  (define-key map "\C-c@e" 'eclipse-hide-clause)
  (define-key map "\C-c@l" 'eclipse-hide-block)
  (define-key map "\C-c@a" 'eclipse-show-all)
  (define-key map "\C-c@s" 'eclipse-show-predicates)
  (define-key map "\C-c@r" 'eclipse-show-predicate)
  (define-key map "\C-c@d" 'eclipse-show-clauses)
  (define-key map "\C-c@m" 'eclipse-show-clause)
  (define-key map "\C-c@k" 'eclipse-show-block))

(unless eclipse-mode-map
  (setq eclipse-mode-map (make-sparse-keymap))
  (eclipse-mode-commands eclipse-mode-map))

;;
;; Menu definitions
;;

(easy-menu-define
 eclipse-process-menu eclipse-mode-map
 "ECLiPSe-Process Menu in ECLiPSe mode.
Contains commands that are associated with an inferior ECLiPSe process."
 '("ECLiPSe-Process"
   ["Run ECLiPSe" run-eclipse t]
   ["Stop ECLiPSe" stop-eclipse t]
   ["Kill ECLiPSe" kill-eclipse t]
   "--"
   ["Compile buffer" eclipse-compile-buffer (not eclipse-esp-selected)]
   ["Compile region" eclipse-compile-region (not eclipse-esp-selected)]
   ["Compile region and switch" eclipse-compile-region-and-go (not eclipse-esp-selected)]
   ["Run region (as command)" eclipse-run-region (not eclipse-esp-selected)]
   "--"
   ["Start TkTools" eclipse-start-tools t]
   "--"
   ["Call ECLiPSe help" eclipse-call-help t]))

(easy-menu-define
 eclipse-edit-menu eclipse-mode-map
 "ECLiPSe-Edit Menu in ECLiPSe mode.
Contains commands that are associated with editing an ECLiPSe file."
 '("ECLiPSe-Edit"
   ("Indent"
    ["Indent line" eclipse-indent-line t]
    ["Indent region" eclipse-indent-region t]
    ["Indent buffer" eclipse-indent-buffer t]
    ["Indent predicate" eclipse-indent-predicate (not eclipse-esp-selected)]
    ["Indent clause" eclipse-indent-clause t])
   ("Mark"
    ["Mark buffer" eclipse-mark-buffer t]
    ["Mark predicate" eclipse-mark-predicate (not eclipse-esp-selected)]
    ["Mark clause" eclipse-mark-clause t])
   ("Comment"
    ["Comment out region" eclipse-comment-region t]
    ["Uncomment region" eclipse-uncomment-region t]
    ["Invert commenting of region" eclipse-invert-comment-region t])
   ("Text"
    ["Go to beginning of predicate" eclipse-goto-predicate-begin (not eclipse-esp-selected)]
    ["Go to end of predicate" eclipse-goto-predicate-end (not eclipse-esp-selected)]
    ["Go to beginning of clause" eclipse-goto-clause-begin t]
    ["Go to end of clause" eclipse-goto-clause-end t]
    "--"
    ["Highlight current word" eclipse-highlight t]
    ["Remove highlighting" eclipse-dehighlight t]
    ["Go to next" eclipse-goto-highlight-forward t]
    ["Go to previous" eclipse-goto-highlight-backward t]
    "--"
    ["Show metrics" eclipse-display-metrics t]
    ["Show metrics (all buffers)" eclipse-display-metrics-all t]
    "--"
    ["Re-fontify & re-center" eclipse-recenter t]
    ["Switch to HTML editing mode" eclipse-toggle-html-mode eclipse-esp-selected])
   ("Edit"
    ["Anonymise variables in region" eclipse-anonymise-variables t]
    ["Replace with anonymous variables" eclipse-anonymous-variables t]
    "--"
    ["Insert predicate template" eclipse-insert-predicate-template (not eclipse-esp-selected)]
    ["Insert predicate specification" eclipse-insert-predicate-spec (not eclipse-esp-selected)]
    ["Insert clause head" eclipse-insert-clause-head (not eclipse-esp-selected)]
    "--"
    ["Insert comment/2 template" eclipse-insert-comment-pred-short (not eclipse-esp-selected)]
    ["Insert comment/2 template with arguments" eclipse-insert-comment-pred-full (not eclipse-esp-selected)]
    "--"
    ["Load project files" eclipse-load-modules (not eclipse-esp-selected)]
    ["Load all project files" eclipse-load-all-modules (not eclipse-esp-selected)]
    "--"
    ["Auto-expand to name" eclipse-dabbrev-expand0 t]
    ["Auto-expand to template" eclipse-dabbrev-expand t]
    ["List expansion templates" eclipse-dabbrev-completion t]
    ["Auto-expand to name + (" eclipse-dabbrev-expand1 t]
    ["List expansion names" eclipse-dabbrev-completion1 t])
   ("Check"
    ["Check buffer" eclipse-check-buffer (not eclipse-esp-selected)]
    ["Check region" eclipse-check-region (not eclipse-esp-selected)]
    ["Check predicate" eclipse-check-predicate (not eclipse-esp-selected)]
    ["Check clause" eclipse-check-clause (not eclipse-esp-selected)])
   ("Outline"
    ("Hide"
     ["Hide All Predicates" eclipse-hide-predicates (not eclipse-esp-selected)]
     ["Hide Predicate" eclipse-hide-predicate (not eclipse-esp-selected)]
     ["Hide All Clauses" eclipse-hide-clauses (not eclipse-esp-selected)]
     ["Hide Clause" eclipse-hide-clause (not eclipse-esp-selected)]
     ["Hide Block" eclipse-hide-block (not eclipse-esp-selected)])
    ("Show"
     ["Show All Predicates" eclipse-show-predicates (not eclipse-esp-selected)]
     ["Show Predicate" eclipse-show-predicate (not eclipse-esp-selected)]
     ["Show All Clauses" eclipse-show-clauses (not eclipse-esp-selected)]
     ["Show Clause" eclipse-show-clause (not eclipse-esp-selected)]
     ["Show Block" eclipse-show-block (not eclipse-esp-selected)]
     ["Show All" eclipse-show-all (not eclipse-esp-selected)])
    ("Headings"
     ["Previous Same Level" eclipse-outline-backward-same-level (not eclipse-esp-selected)]
     ["Next Same Level" eclipse-outline-forward-same-level (not eclipse-esp-selected)]
     ["Previous" eclipse-outline-previous-visible-heading (not eclipse-esp-selected)]
     ["Next" eclipse-outline-next-visible-heading (not eclipse-esp-selected)]
     ["Up" eclipse-outline-up-heading (not eclipse-esp-selected)])
    "--"
    ["Speedbar on/off" eclipse-speedbar-toggle
     :style toggle
     :selected eclipse-speedbar-selected])
   "--"
   ("Preferences"
     ["Auto-line-break on/off" eclipse-autolinebreak-toggle
      :style toggle
      :selected eclipse-autolinebreak-selected]
     ["Auto-indent on/off" eclipse-autoindent-toggle
      :style toggle
      :selected eclipse-autoindent-selected]
     ["'Classic' indentation on/off" eclipse-indent-toggle
      :style toggle
      :selected eclipse-indent-mode]
     ["Match parentheses on/off" eclipse-match-parenthesis-toggle
      :style toggle
      :selected eclipse-indent-closing-parenthesis-to-match-opening]
     ["Quick jumps on/off" eclipse-quick-jumps-toggle
      :style toggle
      :selected eclipse-quick-jumps-selected]
     "--"
     ("Font Lock"
      ["Font Lock Off" eclipse-font-lock-0 t]
      ["Font Lock Level 1 (Basic)" eclipse-font-lock-1 t]
      ["Font Lock Level 2 (Low)" eclipse-font-lock-2 t]
      ["Font Lock Level 3 (Medium)" eclipse-font-lock-3 t]
      ["Font Lock Level 4 (High)" eclipse-font-lock-4 t])
     "--"
     ["Customize" eclipse-customize-group t])))

;;;###autoload
(defun eclipse-mode ()
  "Major mode for editing ECLiPSe code.

Commands:
\\{eclipse-mode-map}

Entry to this mode calls the value of `eclipse-mode-hook' if that value is
non-nil.

The auto-line-break mode is set to on on start-up. It can be toggled by
calling \\[eclipse-autolinebreak-toggle], or customised by setting the variable 
`eclipse-autolinebreak-selected'.
A non-nil value means that auto-line-break is on.

The auto-indent mode is set to on on start-up. It can be toggled by calling
\\[eclipse-autoindent-toggle], or customised by setting the variable
`eclipse-autoindent-selected'. A non-nil value means that auto-indent is on.

The tab mode is set to \"use space characters\" on start-up. It can be
toggled by calling \\[eclipse-tab-mode-toggle], or customised by setting the
variable `eclipse-tab-mode'. A non-nil value means that tab characters are
used is possible, otherwise space characters only.

The width of the initial indentation at the beginning of a line is stored in
the variable `eclipse-tab-width'. Further indentations, after open brackets,
use the value stored in `eclipse-indent-width'.

If `eclipse-indent-mode' is set to a non-nil value, indentation will always
increase/decrease by `eclipse-indent-width'. The default is nil.
Toggling the variable will also set `eclipse-tab-width' to
`eclipse-indent-width'.

If `eclipse-first-line-std-indent' is set to a non-nil value, the first line
in a clause will always be indented using `eclipse-tab-width'. The default
value is nil.

You can insert additional tab characters (or the equivalent number of
space characters) with \\[eclipse-insert-tab].

Text can be indented using \\[eclipse-indent-line], \\[eclipse-indent-region], \\[eclipse-indent-predicate], and \\[eclipse-indent-clause].
Note that the indentation of regions can be slow.

Regions can be marked using \\[eclipse-mark-buffer], \\[eclipse-mark-predicate], and \\[eclipse-mark-clause].

The text can be navigated with \\[eclipse-goto-clause-begin], \\[eclipse-goto-clause-end], \\[eclipse-goto-predicate-begin], and \\[eclipse-goto-predicate-end].
If `eclipse-quick-jumps-selected' is non-nil, the functions jump to the next
empty line. Otherwise, the correct position for the jump is computed. Since
this may be slow, the default value for the variable is t.

You can highlight the current word with \\[eclipse-highlight]. The highlighting will persist
during editing. You can navigate between highlighted areas by using \\[eclipse-goto-highlight-forward] to
jump to the next highlighted area, and \\[eclipse-goto-highlight-backward] to jump to the previous.
\\[eclipse-dehighlight] removes the highlighting.

\\[eclipse-recenter] re-centers and re-fontifies the buffer.

Regions can be commented out with \\[eclipse-comment-region].
\\[eclipse-uncomment-region] deletes leading '%' characters, and
\\[eclipse-invert-comment-region] combines the two previous actions.

Variables in a region can be anonymised with \\[eclipse-anonymise-variables]: '_' will be added to each
variable. Variables in a region can be replaced by '_' with \\[eclipse-anonymous-variables].

The function \\[eclipse-insert-predicate-template] adds a template for the preceding predicate in the style
'foo(,,)' and jumps to the position of the first ',' (if present).
\\[eclipse-insert-predicate-spec] adds the specification for the preceding predicate in the style 'foo/3'.

\\[eclipse-insert-clause-head] adds an empty clause head for the current (preceding) predicate in the
style 'foo(,,) :-' and jumps to the first ',' (if present).

\\[eclipse-insert-comment-pred-short] insert ':- comment(,).'.
\\[eclipse-insert-comment-pred-full] inserts a full 'comment/2' entry, including the arguments, for the
following predicate.

\\[eclipse-load-modules] recursively loads all files listed in compile, use_module, inlude, and
ensure_loaded commands, starting with the current buffer. \\[eclipse-load-all-modules] repeats the
same for all ECLiPSe buffers.

The ECLiPSe mode can pass code to ECLiPSe for syntax checking. The compiler
output will be parsed and passed to a compile mode buffer. These errors and
warnings will be made clickable and linked to the position in the source code
that caused the error (or where the error was detected). This does not work
for errors detected during the module system initialisation phase.

To check the syntax of the whole buffer content, use \\[eclipse-check-buffer]. \\[eclipse-check-region] checks
the region, \\[eclipse-check-predicate] the current predicate, and \\[eclipse-check-clause] the current clause.
Look in the compile mode description for more information.

The ECLiPSe help can be called with \\[eclipse-call-help]. As a default, the current word
will be looked up. Information on other keywords can be looked up by entering
the predicate specification, e.g. 'foo/2'.

The fontification can be set to four different levels:
\\[eclipse-font-lock-1], \\[eclipse-font-lock-2], \\[eclipse-font-lock-3],
\\[eclipse-font-lock-4], or shut off using \\[eclipse-font-lock-0].
The default value is stored in `eclipse-font-lock-default'. The default value
is 3.

In XEmacs, the colours for the fontification are set in the
`eclipse-*-face-val' variables.


The ECLiPSe mode has support for the speedbar. The speedbar can be started by
calling \\[eclipse-speedbar-toggle]. For ECLiPSe and Prolog programs, the
speedbar will list all predicates and directives.


The ECLiPSe mode uses dabbrev for automatic expansion:

\\[eclipse-dabbrev-expand] expands the current word to a full predicate template. It will search the
output of the ECLiPSe help command and the content of the current and other
ECLiPSe buffers. Using \\[eclipse-dabbrev-expand] will return the next possible expansion.
\\[eclipse-dabbrev-expand0] only expands the keyword, without parameters. \\[eclipse-dabbrev-completion] returns the list of
possible expansions. \\[eclipse-dabbrev-expand1] works like \\[eclipse-dabbrev-expand0] but will add an opening parenthesis
if the predicate takes arguments. \\[eclipse-dabbrev-completion1] will return the list of such
expansions.


The ECLiPSe mode supports outlining text:

To hide text use \\[eclipse-hide-predicates] (hide all predicates), \\[eclipse-hide-predicate] (hide current
predicate), \\[eclipse-hide-clauses] (hide all clauses), \\[eclipse-hide-clause] (hide current clause), and
\\[eclipse-hide-block] (hide current block).

To show text use \\[eclipse-show-all] (show the whole buffer), \\[eclipse-show-predicate] (show the current
predicate), \\[eclipse-show-clause] (show the current clause), and \\[eclipse-show-block] (show the current
block).

\\[eclipse-outline-mark-subtree] marks the current subtree, and \\[eclipse-outline-headers-as-kill]
copies the headers in the region onto the kill ring.

To navigate between headings use \\[eclipse-outline-next-visible-heading] (next visible heading),
\\[eclipse-outline-previous-visible-heading] (previous visible heading), \\[eclipse-outline-up-heading] (previous heading of upper level),
\\[eclipse-outline-forward-same-level] (next heading of same level), and \\[eclipse-outline-backward-same-level] (previous heading of same
level).


The ECLiPSe mode also offers an inferior mode to run an ECLiPSe process:

\\[run-eclipse] opens inferior process buffer (if not already open) and starts ECLiPSe.
\\[stop-eclipse] interrupts the shell or its current subjob if any.
\\[kill-eclipse] sends a quit signal and closes the process
buffer.

\\[eclipse-start-tools] starts the TkTools program.

You can send text to the inferior ECLiPSe process from other buffers using
the commands \\[eclipse-compile-buffer] and \\[eclipse-compile-region].
\\[eclipse-compile-region-and-go] sends the region to the inferior process and switches to the process
buffer. Use \\[eclipse-run-region] to send text as ECLiPSe commands.


The variable settings for the ECLiPSe mode can be customised with
\\[eclipse-customize-group].
"
  (interactive)
  (eclipse-mode-common 'eclipse-mode "Eclipse")
  ;; start outline minor mode
  (eclipse-outline eclipse-mode-map)
  (run-hooks 'eclipse-mode-hook))

(defun eclipse-esp-mode ()
  "Major mode for editing ECLiPSe Server Pages code.

Commands:
\\{eclipse-mode-map}

For detailed description of variables and functions cf. \\[eclipse-mode]."
  (interactive)
  (eclipse-mode-common 'eclipse-esp-mode "Eclipse-ESP")
  ;; tab-width must be set to esp ident width
  (setq eclipse-local-tab-width eclipse-esp-indent-width
	eclipse-local-old-tab-width eclipse-tab-width)
  ;; switch to ESP mode
  (setq eclipse-esp-selected t)
  (eclipse-font-lock-5)
  (run-hooks 'eclipse-mode-hook))

(defun eclipse-mode-common (mode name)
  (kill-all-local-variables)
  (use-local-map eclipse-mode-map)
  (setq major-mode mode
	mode-name name)
  (eclipse-mode-variables (equal name "Eclipse-ESP"))
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(eclipse-font-lock-keywords nil)
	indent-tabs-mode eclipse-tab-mode)
  ;; add menus
  (easy-menu-add eclipse-edit-menu)
  (easy-menu-add eclipse-process-menu)
  ;; always start with auto-line-break mode
  (when eclipse-autolinebreak-selected
    (auto-fill-mode 1)
    (setq auto-fill-function 'eclipse-auto-fill-function))
  ;; start speedbar, if selected
  (eclipse-check-speedbar-supported)
  (eclipse-start-speedbar-if-selected)
  ;; use local variables for tab width
  (make-local-variable 'eclipse-local-tab-width)
  (make-local-variable 'eclipse-local-old-tab-width)
  (setq eclipse-local-tab-width eclipse-tab-width
	eclipse-local-old-tab-width eclipse-old-tab-width)
  ;; use local variable to indicate esp mode
  (make-local-variable 'eclipse-esp-selected)
  (setq eclipse-esp-selected nil))

;;
;; Customisation
;;

(defun eclipse-customize-group ()
  "Customize the ECLiPSe group variables."
  (interactive)
  (customize-group 'eclipse))

;;
;; Font lock toggle
;;

(defun eclipse-font-lock-0 ()
  "Switch font lock off."
  (interactive)
  (font-lock-mode 0))

(defun eclipse-do-font-lock (level)
  ;; switch to font-lock-level level
  (font-lock-mode 1)
  (setq font-lock-keywords level)
  (font-lock-fontify-buffer))
  
(defun eclipse-font-lock-1 ()
  "Switch font lock to basic level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-1))

(defun eclipse-font-lock-2 ()
  "Switch font lock to low level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-2))

(defun eclipse-font-lock-3 ()
  "Switch font lock to medium level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-3))

(defun eclipse-font-lock-4 ()
  "Switch font lock to high level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-4))

(defun eclipse-font-lock-5 ()
  "Switch font lock to ESP mode."
  (eclipse-do-font-lock eclipse-font-lock-keywords-5))

(defun eclipse-toggle-html-mode()
  "Switches to HTML mode when editing a .esp file"
  ;; TO DO: this works only when done the first time
  ;; subsequent switches do not succeed properly
  (interactive)
  (unless (not eclipse-esp-selected)
    (html-mode)
    ;; add menu entry for switching back to ECLiPSe ESP mode
    (easy-menu-define
      eclipse-html-menu sgml-mode-map
      "Switches back to ECLiPSe mode"
      '("ECLiPSe"
	["Back to ECLiPSe Mode" eclipse-esp-mode t]))))

;;
;; ECLiPSe speedbar support
;;

(defun eclipse-speedbar-toggle()
  "Toggle speedbar on/off.

If necessary, the extension '.ecl' is added to the list of supported
extensions."
  (interactive)
  (setq eclipse-speedbar-selected (not eclipse-speedbar-selected))
  (if (not eclipse-speedbar-selected)
      (speedbar -1)
    (require 'speedbar)
    (eclipse-start-speedbar-and-add-support)))

(defun eclipse-start-speedbar-if-selected ()
  ;; start speedbar if variable eclipse-speedbar-selected is t at startup
  ;; if speedbar does not support .ecl extensions, add support
  (when (eclipse-speedbar-loaded)
    (setq eclipse-speedbar-selected t))
  (when eclipse-speedbar-selected
    (eclipse-start-speedbar-and-add-support)))

(defun eclipse-start-speedbar-and-add-support ()
  ;; start speedbar, add support for .ecl extension, if necessary
  (speedbar)
  (unless eclipse-speedbar-supported
    (eclipse-add-speedbar-support)
    (speedbar-refresh)))

(defun eclipse-speedbar-loaded ()
  ;; check if speedbar is already loaded
  (let ((list (buffer-list)) (flag nil))
    (while (and list (not flag))
      (if (string-match "SPEEDBAR" (buffer-name (car list)))
	  (setq flag t)
	(setq list (cdr list))))
    flag))

(defun eclipse-add-speedbar-support ()
  ;; add .ecl to list of supported extensions if not in list
  (unless (eclipse-check-speedbar-supported)
    (speedbar-add-supported-extension ".ecl")
    (setq eclipse-speedbar-supported t)))

(defun eclipse-check-speedbar-supported ()
  ;; check if .ecl is supported speedbar extension
  (if (not (eclipse-speedbar-loaded))
      t ; speedbar not loaded: do nothing
    (let ((list speedbar-supported-extension-expressions) (flag nil) el)
      (while (and list (not flag))
	(setq el (car list)
	      list (cdr list))
	(when (string-match (concat "\\" el) ".ecl")
	  (setq flag t
		eclipse-speedbar-supported t)))
      flag)))

;;
;; ECLiPSe mode auto-fill
;;

(defun eclipse-autolinebreak-toggle ()
  "Toggle auto-line-break on/off in ECLiPSe mode.

When auto-line-break is on, strings, comments, and expressions are broken up
if they get too long to fit into a line."
  (interactive)
  (setq eclipse-autolinebreak-selected (not eclipse-autolinebreak-selected))
  (if (not auto-fill-function)
      (setq auto-fill-function 'eclipse-auto-fill-function)
    (setq auto-fill-function nil))
  (force-mode-line-update))

(defun eclipse-autoindent-toggle ()
  "Toggle auto-indent on/off in ECLiPSe mode.

When auto-indent is on, lines are automatically indented after pressing <RET>."
  (interactive)
  (setq eclipse-autoindent-selected (not eclipse-autoindent-selected)))

(defun eclipse-indent-toggle ()
  "Toggle fixed indentation indent on/off in ECLiPSe mode.

When fixed indentation is on, indentation increases/decreases by
`eclipse-indent-width' columns. When toggled on, `eclipse-local-tab-width' is set
to `eclipse-indent-width', when toggled off, the variable is set to its
previous value."
  (interactive)
  (setq eclipse-indent-mode (not eclipse-indent-mode))
  (if eclipse-indent-mode
      (setq eclipse-local-old-tab-width eclipse-local-tab-width
	    eclipse-local-tab-width eclipse-indent-width)
    (setq eclipse-local-tab-width eclipse-local-old-tab-width)))

(defun eclipse-quick-jumps-toggle ()
  "Toggle quick jumps on/off in ECLiPSe mode.

When quick jumps are on, the 'go to' commands jump to the next empty lines.
Otherwise, the correct target for the jump is computed, which can be quite
slow."
  (interactive)
  (setq eclipse-quick-jumps-selected (not eclipse-quick-jumps-selected)))

(defun eclipse-match-parenthesis-toggle ()
   "Toggle parentheses matching mode.

When on, closing parentheses are always indented to the same column as the
matching opening parentheses."
   (interactive)
   (setq eclipse-indent-closing-parenthesis-to-match-opening
	 (not eclipse-indent-closing-parenthesis-to-match-opening)))

(defun eclipse-next-line ()
  "This function is called when <RET> is pressed.
If auto-indent is on, the next line is automatically indented."
  ;; handle process marks myself, since older comint.el do not have
  ;; functions comint-set-process-mark and comint-goto-process-mark
  (interactive)
  (let (string (flag nil) proc beg aux end (name (buffer-name)))
    (cond ((string-equal name "*eclipse*")
	   ;; in eclipse process buffer?
	   (cond
	    ;; normal eclipse process command? (ends in '.')
	    ((save-excursion
	       (when (eobp)
		 (backward-char))
	       (skip-chars-backward " \t\n")
	       (setq beg (point))
	       (backward-char)
	       (looking-at "[^.]\\."))
	     (goto-char (+ 1 beg))
	     ;; this is the end of the input
	     (setq end (point))
	     ;; goto last process marker
	     (goto-char (process-mark (get-buffer-process (current-buffer))))
	     (setq beg (point))
	     ;; goto end of command
	     (eclipse-end-of-clause)
	     (setq string (buffer-substring beg (point)))
	     ;; could be several commands, so repeat until end of input reached
	     (while (> end (point))
	       (forward-line)
	       (beginning-of-line)
	       (skip-chars-forward " \t\n")
	       (setq aux (point))
	       (eclipse-end-of-clause)
	       (setq string (concat string (buffer-substring aux (point)))))
	     (insert "\n")
	     ;; send command to eclipse, fontify, etc.
	     (setq string (concat string "\n"))
	     (eclipse-set-process-mark)
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (process-send-string "eclipse" string)
	     (if eclipse-emacs-21
		 (comint-add-to-input-history string)
	       (ring-insert comint-input-ring string)))
	    ;; eclipse debugger command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at ".*%>"))
	     ;; goto begin of debugger command
	     (save-excursion
	       (re-search-backward "%>")
	       (forward-char 3)
	       (setq beg (point)))
	     (cond ((eq beg (point))
		    ;; if empty input, send CR to eclipse
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" "\x0d"))
		   (t
		    ;; else fontify and send command to eclipse
		    (setq string (buffer-substring beg (point)))
		    (when eclipse-emacs-21 (eclipse-change-face beg))
		    (insert "\n")
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" string))))
	    ;; eclipse interruption command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at "interruption"))
	     ;; get answer, fontify, and send to eclipse
	     (save-excursion
	       (search-backward "?")
	       (forward-char 2)
	       (setq beg (point)))
	     (setq string (buffer-substring beg (point)))
	     (when eclipse-emacs-21 (eclipse-change-face beg))
	     (insert "\n")
	     (eclipse-set-process-mark)
	     (process-send-string "eclipse" string))
	    ;; eclipse tracer command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at ".*[]:?]")
	       (unless (looking-at ".*\\[")
		 (when (looking-at "\\(set\\|.*[?]\\)")
		   (setq flag t)))
	       (when (looking-at "jump")
		 (setq flag t))
	       (setq proc (get-buffer-process (current-buffer))
		     beg (process-mark proc)))
	     ;; get answer, fontify, and send to eclipse
	     (setq string (buffer-substring beg (point)))
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (insert "\n")
	     (when flag
	       (setq string (concat string "\n")))
	     (eclipse-set-process-mark)
	     (process-send-string "eclipse" string))
	    ;; else: regular line, just insert new line
	    (t
	     (insert "\n")
	     (if eclipse-emacs-21
		 (insert "\t") ;; if emacs 21, do not attempt to indent
	       (eclipse-indent-line)))))
	  (t
	   ;; else in eclipse code buffer
	   (newline)
	   (if eclipse-autoindent-selected (eclipse-indent-line t))))))

;; the next two functions are copied & adapted from comint.el --- general
;; command interpreter in a window stuff
;; Copyright (C) 1988, 90, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;	Free Software Foundation, Inc.
;; Authors: Olin Shivers <shivers@cs.cmu.edu>
;;	    Simon Marshall <simon@gnu.org>)
(defun eclipse-set-process-mark ()
  (let ((proc (get-buffer-process (current-buffer))))
    (set-marker (process-mark proc) (point))))

(defun eclipse-change-face (beg)
  (let ((over (make-overlay beg (point) nil nil t)))
    (overlay-put over 'field 'input)
    (overlay-put over 'face 'comint-highlight-input)))

;; The autofill function was copied & adapted from simple.el --- basic
;; editing commands for Emacs
;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;        Free Software Foundation, Inc.
;;
;; the breaking-up of strings, quoted atoms, and comments needs special
;;   handling
;; comments partly deleted: look up in simple.el
(defun eclipse-auto-fill-function ()
  "Auto-fill function for ECLiPSe mode.

Example:
pred(Var) :-
        another_pred(
                        \"This is a very long string that will be\"
                        \" automatically wrapped around\", %% This is a
                                                         %% comment
                        A + Really + Very + Long + And + Nonsensical +
                        Expression
                    )."
  (let (fc give-up (ep nil))
    (if (or (null (setq fc (current-fill-column)))
	    (<= (current-column) fc))
	nil ;; Auto-filling not required
      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (bol-point first-break-point atom-break-point
	       (fill-point
		(let ((opoint (point)))
		  (save-excursion
		    (beginning-of-line)
		    (setq bol-point (point))
		    (move-to-column (1+ fc))
		    ;; Move back to the point where we can break the line.
		    (re-search-backward "[-/+*=<>:? \t,;]")
		    (cond ((looking-at "[-/+*=<>:?]") ; math operator
			   (re-search-forward "[^-/+*=<>:?]" (point-max) t)
			   (backward-char))
			  ((looking-at "[,;]") ; and/or
			   (forward-char))
			  (t t))
		    ;; check if we're inside a quoted atom
		    ;; if so, break before the start of the quoted atom
		    (setq first-break-point (point))
		    (beginning-of-line)
		    (let ((cc nil) (ac nil) (sc nil))
		      (while (< (point) first-break-point)
			(forward-char 1)
			(cond ((looking-at "\"")
			       (or ac cc (setq sc (not sc))))
			      ((looking-at "%")
			       (or sc ac (setq cc t)))
			      ((looking-at "'")
			       (or sc cc
				   (progn
				     (setq ac (not ac))
				     (or (not ac)
					 (save-excursion
					   (re-search-backward "[-/+*=<>:? \t,;]")
					   (when (looking-at "[-/+*=<>:?]")
					     (re-search-forward "[^-/*+=<>:?]" (point-max) t)
					     (backward-char))
					   (setq atom-break-point (point)))))))
			      (t t)))
		      (if ac (goto-char atom-break-point)))
		    ;; If we find nowhere on the line to break it,
		    ;; break after one word.
		    (cond ((bolp)
			   (re-search-forward "[ \t]" opoint t))
			  ((looking-at "[ \t]")
			   ;; Break the line at word boundary.
			   (skip-chars-backward " \t"))
			  ((looking-at "[-/*+=<>:?]")
			   (re-search-forward "[^-/+*=<>:?]" (point-max) t)
			   (backward-char))
			  ((eq (point) first-break-point) t) ;; same point: break here
			  (t (forward-char))) ;; Break the line after/before \c|.
		    (if (and enable-multibyte-characters
			     (not eclipse-xemacs)
			     (not (and (eq (charset-after (1- (point))) 'ascii)
				       (eq (charset-after (point)) 'ascii))))
			;; special function for the charset of non-ascii
			;; character to find the correct break point.
			;; Don't do in XEmacs, charset-after not available.
			(fill-find-break-point bol-point))
		    ;; move back before any whitespace here.
		    (skip-chars-backward " \t")
		    ;; that's the fill-point
		    (point)))))
	  ;; See whether the place we found is any good.
	  (if (save-excursion
		(goto-char fill-point)
		(and (not (bolp))
		     (not (save-excursion (skip-chars-forward " \t") (eolp)))))
	      ;; There is no use breaking at end of line...
	      ;; ...or beginning of line.
	      ;; (test for comment start deleted)
	      ;; Ok, we have a useful place to break the line.  Do it.
	      (let (counter (colmn nil) (prev-column (current-column)))
		;; now we must determine, if the break-point is
		;; (a) in a comment, or
		;; (b) in a string, or
		;; (c) in a regular line
		;; if (a), break the line, insert spaces until the beginning
		;;         of the comment, and insert as many percentage signs
		;; if (b), add \", break the line, indent, add \"
		;; if (c), break the line and indent
		;; quoted atoms have been dealt with while finding the
		;; break point. dealing with comments should be done at that
		;; point, too...
		(cond ((save-excursion
			 ;; inside a string?
			 (goto-char fill-point)
			 (setq counter 0
			       colmn nil)
			 (while (not (bolp))
			   (cond ((looking-at "\"")
				  (setq counter (+ counter 1)))
				 ((and (looking-at "%") (= (mod counter 2) 0))
				  (setq colmn t
					counter 0))
				 (t t))
			   (backward-char))
			 (not (and colmn (= (mod counter 2) 0)))
			 (> counter 0)
			 (= (mod counter 2) 1))
		       ;; close string before fill point,
		       ;; open string anew after indenting
		       (if (not eclipse-autoindent-selected)
			   (save-excursion
			     (goto-char fill-point)
			     (insert "\n"))
			 (save-excursion
			   (goto-char fill-point)
			   (if (save-excursion
				 (backward-char)
				 (looking-at "\""))
			       (progn (backward-char 2) (insert "\n"))
			     (insert "\"\n\"")))
			 (eclipse-indent-line nil t)
			 (skip-chars-forward " \t")))
		      ((save-excursion
			 ;; inside a comment?
			 (beginning-of-line)
			 (setq colmn nil)
			 (while (and (< (point) fill-point) (not colmn))
			   (cond ((looking-at "\"")
				  (eclipse-goto-end-of-string))
				 ((looking-at "'")
				  (eclipse-goto-end-of-quote))
				 ((looking-at "%")
				  (setq colmn (point)))
				 (t (forward-char))))
			 colmn)
		       ;; continue comment in next line
		       (if (not eclipse-autoindent-selected)
			   (save-excursion
			     (goto-char fill-point)
			     (insert "\n% "))
			 (save-excursion
			   (goto-char colmn)
			   (setq colmn (current-column)
				 counter 0)
			   (while (looking-at "%")
			     (forward-char)
			     (setq counter (+ counter 1)))
			   (goto-char fill-point)
			   (insert "\n")
			   (indent-to colmn)
			   (insert (make-string counter 37)))))
		      (t
		       (save-excursion
			 (goto-char fill-point)
			 (insert "\n")
			 (if eclipse-autoindent-selected
			     (eclipse-indent-line nil t))
			 (setq ep (point)))))
		;; If making the new line didn't reduce the hpos of
		;; the end of the line, then give up now.
		(if (>= (current-column) prev-column) (setq give-up t)))
	    ;; No good place to break => stop trying.
	    (setq give-up t))))
      (if (and ep (< (point) ep)) (goto-char ep))))) 

;;
;; ECLiPSe mode commenting in & out
;;

(defun eclipse-recenter ()
  "(Re-)Fontify the current buffer as ECLiPSe code and re-center it."
  (interactive)
  (let ((pos (- (point-max) (point))))
    (font-lock-fontify-buffer)
    (recenter)
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun eclipse-change-comment-region (mode)
  ;; change commenting of current region
  (let ((pos (point)) (regionend (region-end)) (max 0) type_str msg)
    (cond ((= mode 1) (setq type_str "Commenting out"))
	  ((= mode 2) (setq type_str "Un-commenting"))
	  ((= mode 3) (setq type_str "Inverting commenting of")))
    (setq msg (concat type_str " region..."))
    (message msg)
    (goto-char (region-beginning))
    ;; only change complete lines
    (beginning-of-line)
    (while (< (point) regionend)
      (cond ((and (or (= mode 2) (= mode 3)) (looking-at "%% "))
	     (delete-char 3)
	     (setq max (+ max 3)
		   regionend (- regionend 3)))
	    ((and (or (= mode 2) (= mode 3)) (looking-at "%%\t"))
	     (delete-char 2)
	     (setq max (+ max 2)
		   regionend (- regionend 2)))
	    ((and (or (= mode 2) (= mode 3)) (looking-at "%[ \t]"))
	     (delete-char 1)
	     (setq max (+ max 1)
		   regionend (- regionend 1)))
	    ((or (= mode 1) (= mode 3))
	     (insert "%% ")
	     (setq max (- max 3)
		   regionend (+ regionend 3)))
	    (t t))
      (end-of-line)
      (unless (eobp)
	(forward-line)
	(beginning-of-line)))
    (setq msg (concat msg "done"))
    (message msg)
    (goto-char (- pos max))))

(defun eclipse-comment-region ()
  "Comment out current region."
  (interactive)
  (eclipse-change-comment-region 1))

(defun eclipse-uncomment-region ()
  "Uncomment current region."
  (interactive)
  (eclipse-change-comment-region 2))

(defun eclipse-invert-comment-region ()
  "Invert commenting of current region."
  (interactive)
  (eclipse-change-comment-region 3))

;;
;; ECLiPSe mode indentation
;;

(defun eclipse-tab-mode-toggle ()
  "Toggle tab-mode on/off in ECLiPSe mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)
	eclipse-tab-mode (not eclipse-tab-mode)))

(defun eclipse-insert-tab ()
  "Insert a tab character, or, if eclipse-tab-mode is off,
`eclipse-indent-width' many space characters."
  (interactive)
  (if eclipse-tab-mode (insert "\t")
    (insert (make-string eclipse-indent-width 32))))

(defun eclipse-indent-line (&optional af flag)
  "Indent current line as ECLiPSe code."
  (interactive)
  (let* ((quotes (eclipse-count-quotes))
         (pos (- (point-max) (point))) beg)	
    ;; if inside string and auto-line-break is on,
    ;; break string and insert additional \"
    (when (and af eclipse-autolinebreak-selected quotes)
      (save-excursion
	(forward-line -1)
	(end-of-line)
	(insert "\"")))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward "[ \t]")
    (when (and af eclipse-autolinebreak-selected quotes)
      (insert "\"")
      (backward-char))
    (eclipse-indent-region-as-block beg (+ (point) 1) flag)
    (eclipse-backward-char)
    (goto-char (max (point) (- (point-max) pos)))))

(defun eclipse-indent-region-line (indent1)
  ;;Indent current line as ECLiPSe code.
  (let ((pos (- (point-max) (point))) beg
	(indent (if (not indent1) 0 indent1)))
    (beginning-of-line)
    (setq beg (point))
    (cond ((and (not eclipse-tab-mode) (looking-at " *\t[ \t]*"))
	   ;; change tabs to spaces if eclipse-tab-mode is nil
	   (skip-chars-forward " \t")
	   (delete-region beg (point))
	   (indent-to indent))
	  (t
	   (skip-chars-forward " \t")
	   (unless (zerop (- indent (current-column)))
	     (delete-region beg (point))
	     (indent-to indent))))
    (goto-char (max (point) (- (point-max) pos)))))

(defun eclipse-indent-region (&optional msg)
  "Indent current region as ECLiPSe code."
  (interactive)
  (let ((rb (region-beginning)) (re (region-end)) rbegin rend msg-str)
    (cond ((< rb re)
	   (goto-char rb)
	   (beginning-of-line)
	   (setq rbegin (point)
		 rend re))
	  (t
	   (goto-char re)
	   (beginning-of-line)
	   (setq rbegin (point)
		 rend rb)))
    (if (not msg) (setq msg-str "Indenting region...")
      (setq msg-str (concat "Indenting " msg "...")))
    (message msg-str)
    (eclipse-indent-region-as-block rbegin rend)
    (message (concat msg-str "done"))))

(defun eclipse-indent-buffer (&optional msg)
  "Indent buffer as ECLiPSe code."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max))
  (eclipse-indent-region msg))

(defun eclipse-indent-region-as-block (begin end &optional afflag)
  ;; Indent current region as ECLiPSe code
  (let ((stack '())    ; stack for parsing
	pos            ; current position
	(nlflag nil)   ; new line flag, t if at beginning of new line
	(lstnl t)      ; nl flag, t if last read element relevant for indentation was "\n"
	(cmtflag nil)  ; comment flag
	(n 0)          ; match length
	(indnt 0)      ; indentation value
	(flflag 0)     ; first line flag: 0 = head, 1 = first body line, 2 = further body line
	(level 0)      ; nesting level
	aux            ; auxiliary variable
	first-type first-clmn first-level ; values of first stack element
	(idtflag nil)  ; indent flag, t if line is to be indented
	clmn           ; current column
	(cmt2flag nil) ; comment/2 flag, t if inside comment/2 directive
	auxst          ; auxiliary stack
	(eobfl 0)      ; end of buffer flag, to distinguish between empty last buffer lines
	               ; and lines that contain code
	(time (+ eclipse-indent-timeout (cadr (current-time)) 1))
                       ; maxtime for indentation
	(timeflag nil) ; flag, t if indentation should time out
	str)           ; message string
    ;; goto beginning of current clause, the parse down to position begin,
    ;; to find out what the indentation at that position should be,
    ;; then indent every line down to position end
    (eclipse-goto-clause-begin t)
    (while (and (< (point) end) (< eobfl 2) (not (eq timeflag 2)))
      (when (and (not timeflag)
		 (<= time (car (cdr (current-time)))))
	;; timeout reached (...it's a very long clause!?)
	;; ask if the user wants to continue
	(beep)
	(setq str (read-from-minibuffer
		   (concat "Indentation takes more than "
			   (number-to-string eclipse-indent-timeout)
			   " second(s). Continue? (y/n/new timeout) ")))
	(cond ((= (string-to-char str) 121)
	       (setq timeflag 1)
	       (message "Indenting..."))
	      ((> (string-to-number str) 0)
	       (message "Do you want to save the new timeout in your custom-set variables? (y/n) ")
	       (when (= (read-char) 121)
		 (require 'cus-edit)
		 (customize-save-variable 'eclipse-indent-timeout (string-to-number str)))
	       (setq eclipse-indent-timeout (string-to-number str)
		     time (+ (+ eclipse-indent-timeout 1) (car (cdr (current-time)))))
	       (message "Indenting..."))
	      (t
	       (message "Do you want to switch auto-indent off? (y/n) ")
	       (when (= (read-char) 121)
		 (setq eclipse-autoindent-selected nil))
	       (setq timeflag 2)
	       (goto-char end))))
      (when (and (not idtflag) (>= (point) begin))
	;; we now want to indent!
	(setq idtflag t))
      (cond ((and nlflag cmtflag (looking-at "[ \t]*%"))
	     ;; looking at a comment, or inside one
	     (skip-chars-forward " \t")
	     (when idtflag
	       (setq pos (point))
	       (eclipse-indent-region-line (nth 1 (car stack)))
	       (setq end (eclipse-set-end end pos)))
	     (end-of-line)
	     (unless (eobp)
	       (forward-line)
	       (beginning-of-line)))
	    (t
	     (when nlflag
	       ;; when in new line, reset flags
	       (setq nlflag nil
		     cmtflag nil
		     lstnl t
		     n 0))
	     (skip-chars-forward " \t")
	     (setq pos (point))
	     (when (looking-at ":- comment(")
	       ;; comment/2 directive
	       (setq cmt2flag t
		     flflag 0))
	     (when (equal (nth 0 (car stack)) 'cmt)
	       ;; remove first element from stack, if comment:
	       ;; does not influence indentation
	       (setq stack (cdr stack)))
	     (if (null stack)
		 (setq first-type 'nul
		       first-clmn 0
		       first-level 0)
	       (setq first-type (nth 0 (car stack))
		     first-clmn (nth 1 (car stack))
		     first-level (nth 2 (car stack))))
	     (cond ((and cmt2flag (= flflag 2) (not (null stack)))
		    ;; inside a comment/2 directive
		    (when idtflag
		      (eclipse-indent-region-line eclipse-local-tab-width)
		      (setq end (eclipse-set-end end pos)))
		    (setq cmt2flag nil))
		   ((or (looking-at "\n") (eobp))
		    ;; at end of line or end of buffer
		    (when (and lstnl idtflag) ; indent line when required
		      (cond ((null stack)
			     (eclipse-indent-region-line 0))
			    ((member first-type (list 'el 'st))
			     (setq auxst stack
				   stack (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt)))
			     (unless (equal (nth 0 (cadr stack)) 'cd)
			       (setq stack (cdr stack)))
			     (if (and (member (nth 0 (car stack)) (list 'rb 'sb 'cb))
				      (not eclipse-indent-closing-parenthesis-to-match-opening))
				 (setq indnt (nth 1 (cadr stack))
				       level (nth 2 (cadr stack))
				       stack (cdr stack))
			       (setq indnt (nth 1 (car stack))
				     level (nth 2 (car stack))
				     stack (cdr stack)))
			     (when eclipse-indent-mode
			       (setq indnt (* level eclipse-indent-width)))
			     (if (not indnt)
				 (eclipse-indent-region-line eclipse-local-tab-width)
			       (eclipse-indent-region-line indnt))
			     (setq stack auxst))
			    ((and eclipse-esp-selected (equal first-type 'cd))
			     (setq indnt (nth 1 (car stack)))
			     (when eclipse-indent-mode
			       (setq indnt (* level eclipse-indent-width)))
			     (eclipse-indent-region-line indnt))
			    (t (eclipse-indent-region3 stack flflag)))
		      (setq end (eclipse-set-end end pos)))
		    (setq nlflag t)
		    (when (= flflag 1)
		      ;; this was the first body line
		      (setq flflag 2))
		    (unless (eobp)
		      (forward-line)
		      (beginning-of-line)))
		   ((looking-at ",")
		    ;; comma
		    (cond (lstnl ; if at beginning of new line
			   (when idtflag ; indent when required
			     (cond ((null stack)
				    (eclipse-indent-region-line 0))
				   ((equal first-type 'el)
				    (setq stack (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'if 'sc 'do)))
				    (unless (equal (nth 0 (car (cdr stack))) 'cd)
				      (setq stack (cdr stack)))
				    (setq indnt (nth 1 (car stack))
					  level (nth 2 (car stack))
					  stack (cdr stack))
				    (when eclipse-indent-mode
				      (setq indnt (* level eclipse-indent-width)))
				    (if (not indnt)
					(eclipse-indent-region-line eclipse-local-tab-width)
				      (eclipse-indent-region-line indnt)))
				   (t (eclipse-standard-indent2 stack level flflag))))
			   (setq stack (cons (list 'co (current-column) level) stack))) ; add to stack
			  (t ; else cut stack back to last comma on same level
			   (setq aux (eclipse-get-last-comma stack level)
				 stack (nth 0 aux)
				 level (nth 1 aux)
				 first-type (nth 0 (car stack))
				 first-clmn (nth 1 (car stack))
				 first-level (nth 2 (car stack)))
			   (unless (equal first-type 'co)
			     (setq stack (cons (list 'co (current-column) level) stack)))))
		    (forward-char))
		   ((looking-at ";")
		    ;; semicolon
		    (when lstnl ; if at beginning of new line
		      (unless (= level 1) ; adjust level
			(setq level (- level 1)))
		      (when idtflag ; indent when required
			;; cut auxiliary stack back to last element on this level to get the indentation
			(setq auxst (eclipse-get-last-b (eclipse-get-last-level-stack stack level)))
			(cond (eclipse-indent-mode
			       (eclipse-indent-region-line (* level eclipse-indent-width)))
			      ((or (= level 1) (equal (nth 0 (car (cdr auxst))) 'cd))
			       (eclipse-indent-region-line (nth 1 (car auxst))))
			      (t (eclipse-indent-region-line (nth 1 (car (cdr auxst))))))
			(setq end (eclipse-set-end end pos))))
		    (setq level (+ level 1)
			  stack (cons (list 'sc (current-column) -1) stack)) ; add to stack
		    (forward-char))
		   ((looking-at "\\.[ \t\n]")
		    ;; end of clause
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (setq stack '() ; reset stack and flags
			  level 0
			  flflag 0
			  cmt2flag nil)
		    (forward-char))
		   ((and (looking-at "<\\?esp:[^ \t\n]+") eclipse-esp-selected)
		    ;; esp command
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-indent-region-line 0)
		      (setq end (eclipse-set-end end pos)))
		    (setq level 1 ; reset stack and flags
			  flflag 1
			  cmt2flag nil
			  stack (cons (list 'cd 0 0) '()))
		    (forward-char (length (match-string 0))))
		   ((and (looking-at "<\\?esp") eclipse-esp-selected)
		    ;; esp command
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-indent-region-line 0)
		      (setq end (eclipse-set-end end pos)))
		    (setq level 1 ; reset stack and flags
			  flflag 1
			  cmt2flag nil
			  stack (cons (list 'cd 2 0) '()))
		    (forward-char 5))
		   ((and (looking-at "\\(is\\|with\\)[ \t\n]")
			 (save-excursion
			   (eclipse-backward-char)
			   (looking-at (concat "[ \t]" (match-string 0)))))
		    ;; "is" or "with"
		    (setq n (1- (length (match-string 0))))
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless eclipse-indent-mode
		      (setq level (+ level 1)))
		    (setq stack (cons (list 'inf (current-column) -1) stack)) ; add to stack
		    (forward-char n))
		   ((and (looking-at "do[ \t\n]")
			 (save-excursion
			   (eclipse-backward-char)
			   (looking-at "[ \t]do[ \t\n]")))
		    ;; "do"
		    (when lstnl
		      (unless eclipse-indent-mode
			(setq level (max 1 (- level 1))))
		      ;; cut stack back to last level
		      (setq stack (eclipse-get-last-level-stack stack level))
		      (when idtflag ; indent when required
			(eclipse-standard-indent1 stack level (nth 1 (car stack)))
			(setq end (eclipse-set-end end pos))))
		    (setq stack (cons (list 'do (current-column) level) stack)) ; add to stack
		    (unless eclipse-indent-mode
		      (setq level (+ level 1)))
		    (forward-char 2))
		   ((looking-at "\\([0-9]?\\.[0-9]+\\|[a-zA-Z0-9_]+\\|(\\.)\\)")
		    ;; word or number
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      ;; add to stack, unless last element is already some regular 'el element
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char n)) ; jump over word or number
		   ((looking-at "[#.](")
		    ;; "#" or "." with opening parenthesis: almost like normal word or number...
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char)) ; ...but we only jump ahead one character: we want to catch the "("
		   ((looking-at "-\\?->")
		    ;; matching operator
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent1 stack level eclipse-local-tab-width)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (setq stack (cons (list 'mt (current-column) level) stack)) ; add to stack
		    (forward-char 4))
		   ((looking-at "\\(:- mode \\|[:?]-\\|-->\\)")
		    ;; colon-dash and related things
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag) ; indent when required
		      (if eclipse-esp-selected
			  (eclipse-standard-indent1 stack level 0)
			(eclipse-standard-indent1 stack level eclipse-local-tab-width))
		      (setq end (eclipse-set-end end pos)))
		    (setq stack (cons (list 'cd (current-column) level) stack) ; add to stack
			  flflag 1            ; next line will be first body line
			  level (+ level 1))
		    (forward-char n))
		   ((looking-at "[*]?->")
		    ;; "if-then" operator (well, mostly)
		    (setq n (length (match-string 0)))
		    (when lstnl ; if at beginning of new line
		      ;; cut back stack to last opening bracket or semicolon
		      (setq level (- level 1)
			    stack (cdr (eclipse-get-last-sc stack)))
		      (when idtflag ; indent when required
			(eclipse-standard-indent1 stack level (nth 1 (car stack)))
			(setq end (eclipse-set-end end pos))))
		    (cond ((eclipse-in-list stack) ; if in list, it's an operator
			   (setq stack (cons (list 'inf (current-column) -1) stack)))
			  (t (setq stack (cons (list 'if (current-column) level) stack)))) ; else it's an "if-then"
		    (when (or lstnl (not eclipse-indent-mode))
		      (setq level (+ level 1)))
		    (forward-char n))
		   ((looking-at "#<?=>")
		    ;; constraints #=> and #<=>
		    (setq n (length (match-string 0)))
		    (when lstnl ; if at beginning of new line
		      (unless eclipse-indent-mode
			(setq level (- level 1)))
		      ;; cut back stack to last level
		      (setq stack (eclipse-get-last-level-stack stack level))
		      (when idtflag ; indent when required
			(eclipse-standard-indent1 stack level (nth 1 (car stack)))
			(setq end (eclipse-set-end end pos))))
		    (setq stack (cons (list 'if (current-column) level) stack)) ; add to stack
		    (unless eclipse-indent-mode
		      (setq level (+ level 1)))
		    (forward-char n))
		   ((looking-at "'")
		    ;; inverted comma: start of quoted atom
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (unless (eclipse-goto-end-of-quote) ; jump to end of quote
		      (setq stack (cons (list 'qu (current-column) level) stack)))) ; add to stack
		   ((looking-at "\"")
		    ;; beginning of string
		    (when (and lstnl idtflag) ; indent when required
		      (cond (eclipse-indent-mode
			     (eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
			    ((equal first-type 'st) ; multi-line string
			     (setq stack (eclipse-get-last-inf stack))
			     (eclipse-indent-region-line (nth 1 (car stack))))
			    (t (eclipse-indent-region3 stack flflag)))
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (unless (eclipse-goto-end-of-string) ; jump to end of string
		      (setq stack (cons (list 'st (current-column) level) stack)))) ; add to stack
		   ((looking-at "%+")
		    ;; comment
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag)
		      (if (looking-at "%[^%]")
			  (eclipse-standard-indent2 stack level flflag) ; single "%": indent as code
			(eclipse-indent-region-line 0)) ;; multiple "%": indent to column 0
		      (setq end (eclipse-set-end end pos)))
		    (setq stack (cons (list 'cmt (current-column) level n) stack) ; add to stack
			  nlflag t
			  cmtflag t)
		    (end-of-line) ; goto end of line, since rest of line will be comment
		    (unless (eobp)
		      (forward-line)
		      (beginning-of-line)))
		   ((looking-at "/\\*")
		    ;; beginning of c-style comment: do nothing
		    (unless (re-search-forward "\\*/" (point-max) t) ; is it closed? then jump there
		      (goto-char (point-max))))
		   ((and (looking-at "\\?>") eclipse-esp-selected)
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-indent-region-line 0)
		      (setq end (eclipse-set-end end pos)))
		    ;; beginning of HTML-section in ESP page: do nothing
		    (unless (re-search-forward "<\\?esp" (point-max) t) ; is it closed? then jump there
		      (goto-char (point-max)))
		    (backward-char 5)
		    (unless (looking-at "<\\?esp")
		      (forward-char 5)))
		   ((looking-at "[({[]")
		    ;; opening bracket
		    (let ((symbol (cond ((looking-at "(") 'rb)
					((looking-at "\\[") 'sb)
					(t 'cb))))
		      (when (and lstnl idtflag) ; indent when required
			(eclipse-standard-indent2 stack level flflag)
			(setq end (eclipse-set-end end pos)))
		      (unless (equal (nth 0 (car stack)) 'el)
			(setq stack (cons (list 'el (current-column) level) stack)))
		      (setq stack (cons (list symbol (current-column) level) stack) ; add to stack
			    level (+ level 1)) ; and increase level
		      (forward-char)))
		   ((looking-at "[])}]")
		    ;; closing bracket
		    (let* ((symbol (cond ((looking-at ")") 'rb)
					 ((looking-at "\\]") 'sb)
					 (t 'cb)))
			   (auxl (cond ((looking-at ")") (list 'sb 'cb))
				       ((looking-at "\\]") (list 'rb 'cb))
				       (t (list 'rb 'sb))))
			   (auxfl (member first-type (append (list 'co 'sc 'inf 'op 'do 'if) auxl))))
		      (when auxfl ; if last element was some kind of operator, indent now...
			(when (and lstnl idtflag) ; ...when required
			  (eclipse-standard-indent2 stack level flflag)
			  (setq end (eclipse-set-end end pos))))
		      ;; cut stack back to matching opening bracket
		      (setq stack (eclipse-get-last symbol stack))
		      (when (null stack) ; oops...
			(error "Empty stack. Check for '.' instead of ','"))
		      (if eclipse-indent-closing-parenthesis-to-match-opening
			  (setq indnt (nth 1 (car stack))
				level (nth 2 (car stack))
				stack (cdr stack))
			(setq indnt (nth 1 (cadr stack))
			      level (nth 2 (cadr stack))
			      stack (cdr stack)))
		      (when (not auxfl) ; if last element was no operator, indent now...
			(when (and lstnl idtflag) ; ...when required
			  (eclipse-standard-indent1 stack level indnt)
			  (setq end (eclipse-set-end end pos))))
		      (forward-char)))
		   ((looking-at "|")
		    ;; tail operator
		    (cond ((member first-type (list 'co 'sc 'inf 'op 'do 'if 'rb 'sb 'cb))
			   ;; if last element on stack is operator or bracket, indent normally
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos))))
			  (t
			   ;; else search last bracket and use that indentation
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent1 stack level (nth 1 (car (cdr (eclipse-get-last-b stack)))))
			     (setq end (eclipse-set-end end pos)))))
		    (setq stack (cons (list 'rs (current-column) level) stack)) ; add to stack
		    (forward-char))
		   ((looking-at "\\(!\\|[\\+]\\+\\)")
		    ;; cut or negation
		    ;; just the same as word or number...
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char n))
		   ((looking-at "\\(\\+[),]\\|~[ \t(]\\)")
		    ;; "+" before ")" or ",", or sound negation
		    ;; ...the same, but we know it's only one charater to jump ahead
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char))
		   ((looking-at "\\(#\\(#\\|=<?\\|<=\\|>=?\\|\\\\\\(+\\|=\\)\\)\\|$\\(=<\\|[<>]?=\\)\\|`\\(::\\|<>?\\|=\\)\\|\\*\\(=<?\\|>=\\)\\|@\\(=?<\\|>=?\\)\\|::?\\|=\\.\\.\\|[@^&]\\|=[:\\]?=\\|[~]?=<?\\|<<?\\|>[=>]?\\|\\\\==?\\)")
		    ;; comparison or constraint operators
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag) ; indent when required
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (setq clmn (current-column))
		    (forward-char n)
		    (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
			   ;; if used as predicate name, treat as regular 'el element
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el clmn level) stack))))
			  (t
			   ;; else treat as 'inf: infix operator
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (setq stack (cons (list 'inf clmn -1) stack)))))
		   ((looking-at "#\\(\\\\/\\|/\\\\\\)")
		    ;; "#\/" or "#/\" : almost the same...
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (setq clmn (current-column))
		    (forward-char n)
		    (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el clmn level) stack))))
			  (t
			   ;; ...but it's like a semicolon
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (setq stack (cons (list 'sc clmn -1) stack)))))
		   ((looking-at "\\(\\*\\|\\+\\|-\\|/[/\\]?\\|\\\\/?\\)")
		    ;; math operator: again almost the same...
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (setq clmn (current-column))
		    (forward-char n)
		    (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el clmn level) stack))))
			  (t ; but it's an 'op opreator
			   (setq stack (cons (list 'op clmn -1) stack)))))
		   ((looking-at "\\.\\.\\.+")
		    ;; three or more dots: treat like a word
		    (setq n (length (match-string 0)))
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char n))
		   ((looking-at "\\.\\.")
		    ;; two dots: infix operator
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless eclipse-indent-mode
		      (setq level (+ level 1)))
		    (setq stack (cons (list 'inf (current-column) -1) stack))
		    (forward-char 2))
		   ((looking-at "\\.[^ \t\n,]")
		    ;; one dot, but not followed by whitespace, newline, or comma
		    ;; treat like word
		    (when (and lstnl idtflag)
		      (eclipse-standard-indent2 stack level flflag)
		      (setq end (eclipse-set-end end pos)))
		    (unless (equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'el (current-column) level) stack)))
		    (forward-char))
		   (t (forward-char))) ; else go to next character
	     (setq lstnl nil)))
      (cond ((and (eobp) (bolp) (not (null stack))) (setq eobfl 1)) ; end of buffer
	    ((eobp) (setq eobfl 2)) ; end of buffer, but line not empty
	    (t t)))
    (goto-char end)
    (cond ((equal timeflag 2)
	   ;; indentation timed out: just indent like preceding line
	   (backward-char)
	   (save-excursion
	     (forward-line -1)
	     (beginning-of-line)
	     (skip-chars-forward " \t")
	     (setq indnt (current-column)))
	   (eclipse-standard-indent1 stack level indnt))
	  ((not (null stack))
	   ;; stack not empty: we may be inside string or quoted atom
	   (cond ((and (not afflag) (equal (nth 0 (car stack)) 'st))
		  (message "Inside string."))
		 ((and (not afflag) (equal (nth 0 (car stack)) 'qu))
		  (message "Inside quoted atom."))
		 ((equal timeflag 1)
		  (message "Indenting...done"))
		 (t t)))
	  ((equal timeflag 1)
	   (message "Indenting...done"))
	  (t t))))

(defun eclipse-standard-indent1 (stack level width)
  ;; standard indent cond block 1
  (cond ((null stack)
	 ;; empty stack: set indentation to 0, if clause heads are subject to indentation
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 ;; "classic" indentation
	 (eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
	(width (eclipse-indent-region-line width)) ; indentation is known
	(t (eclipse-indent-region-line eclipse-local-tab-width)))) ; else indent to standard tab-width

(defun eclipse-standard-indent2 (stack level flflag)
  ;; standard indent cond block 2: cases 1 and 2 as in eclipse-standard-indent1
  (cond ((null stack)
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 (eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
	(t (eclipse-indent-region3 stack flflag)))) ; indent according to stack

(defun eclipse-set-end (end pos)
  ;; update end of region
  (+ end (- (point) pos)))

(defun eclipse-indent-region3 (stack flag)
  ;; standard indentation function for lines in region
  (let ((typ (nth 0 (car stack))) (column (nth 1 (car stack)))
	(level (nth 2 (car stack))) (auxst stack))
    ;; what's the type of the top element in the stack?
    ;; i.e., the element put on the stack before the one that we now want to indent
    (cond ((member typ (list 'cd 'mt)) ; colon-dash or matching operator: tab-width
	   (eclipse-indent-region-line eclipse-local-tab-width))
	  ((member typ (list 'if 'do)) ; "->" or "do": like last bracket, colon-dash, or semicolon
	   (setq auxst (eclipse-get-last-sc auxst))
	   (when (and eclipse-indent-to-parenthesis (not (eq (nth 0 (car (cdr auxst))) 'cd)))
	     (setq auxst (cdr auxst)))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'sc) ; semicolon : like last bracket, colon-dash, or "->"
	   (setq auxst (eclipse-get-last-if auxst))
	   (eclipse-indent-region-line (nth 1 (car auxst))))
	  ((member typ (list 'rb 'sb 'cb)) ; opening bracket: increase indentation by indent-width
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'co) ; comma
	   (cond ((and (= flag 2) eclipse-first-line-std-indent (= level 1))
		  ;; on level 1: like the indentation of the other lines on that level
		  (setq column (min eclipse-local-tab-width (nth 1 (car (cdr stack)))))
		  (eclipse-indent-region-line column))
		 (t
		  ;; else like the last 'el element in the stack
 		  (setq column (nth 1 (car (cdr stack))))
 		  (eclipse-indent-region-line column))))
	  ((member typ (list 'el 'st 'qu))
	   ;; regular 'el element, or string or quoted atom: like last element of previous level
	   (setq auxst (eclipse-get-last-level-stack auxst (- level 1)))
	   (setq column (nth 1 (car stack)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'rs)
	   ;; "|": like last bracket or colon-dash
	   (setq auxst (eclipse-get-last-b auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'inf)
	   ;; infix operator: like last bracket or colon-dash or regular element
	   (setq auxst (eclipse-get-last-b-or-el auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'op)
	   ;; math operator: like last bracket or colon-dash or infix
	   (setq auxst (eclipse-get-last-inf auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'cmt)
	   ;; comment: ignore
	   (setq auxst (cdr auxst))
	   (eclipse-indent-region3 auxst flag)))))

(defun eclipse-in-list (stack)
  ;; returns t if stack contains element of type 'sb
  (cond ((null stack) nil)
	((equal (nth 0 (car stack)) 'sb) t)
	(t (eclipse-in-list (cdr stack)))))

(defun eclipse-get-last-comma (stack level)
  ;; return stack as it was at last level
  (let* ((last nil) (el (nth 0 (car stack)))
	 (found (member el (list 'co 'do 'if 'sc 'mt 'cd 'rb 'sb 'cb))))
    (while (not (or (null stack) found))
      (setq last (car stack)
	    stack (cdr stack)
	    el (nth 0 (car stack)))
      (cond ((and (equal el 'inf) (not eclipse-indent-mode))
	     (setq level (- level 1)))
	    ((member el (list 'co 'do 'if 'sc 'mt 'cd 'rb 'sb 'cb))
	     (setq found t))
	    (t t)))
    (while (not (or (null stack) (< (nth 2 (car stack)) level)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	(list stack level)
      (list (cons last stack) level))))

(defun eclipse-get-last-level-stack (stack level)
  ;; return stack as it was at last level
  (while (not (or (null stack) (= (nth 2 (car stack)) level)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last (typ stack)
  ;; return stack as it was at last typ
  (while (not (or (null stack) (equal (nth 0 (car stack)) typ)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last-type (stack typelist)
  ;; return stack as it was at point after last element of type in list
  (let ((last nil))
    (while (not (or (null stack) (member (nth 0 (car stack)) typelist)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	stack
      (cons last stack))))

(defun eclipse-get-last-b (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt)))

(defun eclipse-get-last-b-or-el (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'el)))

(defun eclipse-get-last-inf (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'inf)))

(defun eclipse-get-last-if (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'if)))

(defun eclipse-get-last-sc (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'sc)))

(defun eclipse-indent-predicate ()
  "Indent current predicate as ECLiPSe code."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-mark-predicate)
    (eclipse-indent-region "predicate")
    (beginning-of-line)
    (skip-chars-forward " \t")))

(defun eclipse-indent-clause ()
  "Indent current clause as ECLiPSe code."
  (interactive)
  (eclipse-mark-clause)
  (eclipse-indent-region "clause")
  (beginning-of-line)
  (skip-chars-forward " \t"))

;;
;; Mark regions
;;

(defun eclipse-mark-buffer ()
  "Mark complete buffer."
  (interactive)
  (push-mark (point-min))
  (goto-char (point-max)))

(defun eclipse-mark-predicate ()
  "Mark current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (unless (eclipse-check-predicate-begin)
      (eclipse-goto-predicate-begin))
    (push-mark (point))
    (eclipse-goto-predicate-end)))

(defun eclipse-mark-clause ()
  "Mark current clause."
  (interactive)
  (unless (eclipse-check-clause-begin)
    (eclipse-goto-clause-begin))
  (push-mark (point))
  (eclipse-goto-clause-end))

;;
;; Auxiliary functions
;;

(defun eclipse-check-predicate-begin ()
  ;; check if at beginning of predicate
  (let (aux1 aux2)
    (if (and (bolp)
	     (or (looking-at "[:?]-")
		 (and (looking-at "\\([a-z]\\|[^.\n \t]+[^.\n]*[:?]-\\)")
		      (progn
			(setq aux1 (eclipse-get-current-predicate-template t))
			(save-excursion
			  (eclipse-goto-predicate-begin)
			  (setq aux2 (eclipse-get-current-predicate-template t)))
			(not (string-equal aux1 aux2))))))
	t
      nil)))

(defun eclipse-check-clause-begin ()
  ;; check if at beginning of clause
  ;; works only under the assumption that we're likely at the beginning of
  ;; a clause, anyway!
  (if (and (or (bolp)
	       (eclipse-check-left-empty))
	   (looking-at "\\([a-z]\\|\\([^.\n \t]+[^.\n]*\\)?\\([:?]-\\|<\\?esp\\)\\)"))
      t
    nil))

(defun eclipse-check-left-empty ()
  ;; check if rest of the current line to the left is empty
  (let ((flag t))
    (save-excursion
      (while (and flag (not (bolp)))
	(backward-char)
	(when (not (looking-at "[ \t]"))
	  (setq flag nil))))
    flag))

(defun eclipse-backward-char (&optional n)
  ;; save backward-char. no error on bumping into beginning of buffer
  (or n (setq n 1))
  (while (and (> n 0) (not (bobp)))
    (backward-char)
    (setq n (- n 1))))

(defun eclipse-skip-comments-and-empty-lines (&optional flag)
  ;; skip forward through whitespace & comments
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (skip-chars-forward " \t")
      (cond ((looking-at "%") ;; comment: skip to next line
	     (forward-line)
	     (beginning-of-line))
	    ((looking-at "/\\*") ;; c-style comment: skip to end
	     (cond (flag
		    (unless (re-search-forward "\\*/" (point-max) t)
		      (goto-char (point-max))
		      (setq found t)))
		   (t
		    (cond ((looking-at "[^\n]*\\*/")
			   (forward-char 2)
			   (re-search-forward "\\*/" (point-max) t))
			  (t (setq found t))))))
	    ((looking-at "\n") ;; end-of-line: skip to next line
	     (forward-line)
	     (beginning-of-line))
	    (t (setq found t)))))) ;; non-empty character found

(defun eclipse-count-quotes ()
  ;; count the number of double quotes in the line
  (let ((quotes nil) (editpoint (- (point) 1)) (sq nil) (dq nil) (cmt nil))
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (while (and (not (>= (point) editpoint)) (not cmt))
	(cond ((and (looking-at "[^\\]\"") (not sq))
	       (if quotes
		   (setq quotes nil)
		 (setq quotes (current-column)))
	       (setq dq (not dq)))
	      ((and (looking-at "0'[^\n]") (not sq))
	       (forward-char 2))
	      ((looking-at "[1-9]'[0-9a-zA-Z]+")
	       (forward-char 2))
	      ((and (looking-at "[^\\]'") (not dq))
	       (forward-char 2)
	       (setq sq (not sq)))
	      ((and (looking-at "%") (not dq) (not sq))
	       (setq cmt t))
	      ((and (looking-at "/\\*") (not dq) (not sq))
	       (forward-char 2)
	       (if (looking-at "[^\n]*\\*/")
		   (re-search-forward "\\*/" (point-max) t)
		 (setq cmt t)))
	      (t t))
	(forward-char)))
    quotes))

(defmacro jump-over-string (str)
  (list 'forward-char)
  (list 'search-forward str 'eolpos 't)
  (list 'while (list 'not (list 'or 'flag 'comment))
	(list 'cond (list (list 'save-excursion
				(list 'eclipse-backward-char)
				(list 'looking-at (list 'concat "\\\\" str)))
			  (list 'forward-char)
			  (list 'search-forward str 'eolpos 't)
			  (list 'backward-char))
	      (list (list 'looking-at "[ \t]*\n") (list 'setq 'comment 't))
	      (list 't
		    (list 'setq 'flag 't)
		    (list 'forward-char))))
  (list 'setq 'flag 'nil))

(defun eclipse-end-of-clause ()
  ;; go to end of clause in this line
  (let* ((eolpos (save-excursion (end-of-line) (point)))
	 (comment nil) (flag nil) (empty t))
    (beginning-of-line)
    (while (and (not (= (point) eolpos)) (not comment))
      (cond ((looking-at "0'") ; base operator: jump ahead
	     (forward-char 2)
	     (or (looking-at "\n")
		 (forward-char)))
	    ((looking-at "[1-9]+'[0-9a-zA-Z]+") ; base operator: jump ahead
	     (re-search-forward "[^0-9a-zA-Z']" (point-max) t)
	     (backward-char))
 	    ((looking-at "'") ; quoted atom: jump to end
	     (jump-over-string "'"))
 	    ((looking-at "\"") ; string: jump to end
	     (jump-over-string "\""))
	    ((looking-at "%") (setq comment t)) ; comment
	    ((looking-at "\\(/\\*[^\n]*\\*/\\)?[ \t]*\n") ; empty to end of line
	     (setq comment t))
	    ((looking-at "/\\*[^\n]*\\*/") ; jump over short c-style comments
	     (re-search-forward "\\*/" (point-max) t))
	    ((looking-at "/\\*") (setq comment t)) ; beginning of multi-line comment
	    ((looking-at "\\*/[ \t]*\n")
	     ;; end of a multi-line comment: find beginning
	     (re-search-backward "/\\*")
	     (cond ((save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*/\\*"))
		    (while empty
		      (forward-line -1)
		      (beginning-of-line)
		      (if (bobp) (setq empty nil)
			(skip-chars-forward " \t")
			(or (looking-at "\\(%\\|\n\\)")
			    (setq empty nil))))
		    (setq empty t
			  eolpos (save-excursion (end-of-line) (point))))
		   (t (setq comment t))))
	    (t (forward-char))))
    (skip-chars-backward " \t")))

(defun eclipse-jump-over-strings (&optional eobflag cmtflag)
  ;; jump over constructs "...", '...', /*...*/, ?>...<?esp, %..., and whitespace
  (let ((found nil))
    (while (and (looking-at "[ \t\"'/%\n]") (not found))
      (cond ((looking-at "[ \t\n]")
	     (if (re-search-forward "[^ \t\n]" (point-max) t)
		 (backward-char)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((looking-at "%")
	     (end-of-line)
	     (if (eobp)
		 (setq found t)
	       (forward-line)
	       (beginning-of-line)))
	    ((looking-at "/\\*")
	     (unless (re-search-forward "\\*/" (point-max) t)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((looking-at "\\?>")
	     (unless (re-search-forward "<\\?esp" (point-max) t)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((looking-at "\"")
	     (eclipse-goto-end-of-string))
	    ((looking-at "'")
	     (if (not cmtflag)
		 (eclipse-goto-end-of-quote)
	       (setq found t)))
	    (t (forward-char))))))

(defun eclipse-goto-end-of-quote ()
  ;; goto to the end of the current quoted atom
  (eclipse-backward-char)
  (cond ((looking-at "0'") ; actually, just a base operator
	 (forward-char 2)
	 (or (looking-at "\n")
	     (forward-char)))
	((looking-at "[1-9]'[0-9a-zA-Z]") ; ditto
	 (forward-char 3))
	(t
	 (forward-char)
	 (eclipse-goto-end-of "'"))))

(defun eclipse-goto-end-of-string ()
  ;; goto to the end of the current string
  (eclipse-goto-end-of "\""))

(defun eclipse-goto-end-of (str)
  ;; goto to the end of the current string
  (let ((str1 (concat "[^\\]" str)))
    (if (re-search-forward str1 (point-max) t)
	t
      (goto-char (point-max))
      nil)))

(defun eclipse-percent-message (str length last &optional base)
  ;; print a message "Str... (XX%)"
  (let ((percent (truncate (* 100 (/ (* 1.0 (if base base (point)))
				     (* 1.0 length))))))
    (cond ((>= percent (+ last 10))
	   (message (concat str "... (" (number-to-string percent) "%%)"))
	   percent)
	  (t last))))

;;
;; Go-to commands
;;

(defun eclipse-goto-clause-begin (&optional flag)
  "Goto the beginning of the current clause."
  (interactive)
  (cond (eclipse-esp-selected
	 (let ((last (point)) (pnt (point)) (found nil) maxpnt)
	   ;; else go to beginning of buffer, search all clause"<?esp:" tags until we are past pnt
	   ;; the last "<?esp:" tag we found is the one we are looking for
	   (goto-char (point-min))
	   (beginning-of-line)
	   (setq last (point))
	   (if (<= pnt last)
	       (setq maxpnt (point-max))
	     (setq maxpnt pnt))
	   (while (and (not found) (not (eobp)))
	     (cond ((> (point) maxpnt)
		    (goto-char pnt)
		    (setq found t))
		   ((re-search-forward "<\\?esp" maxpnt t)
		    ;;(re-search-forward "<\\?esp\\(:[^ \t\n]+\\)?" maxpnt t)
		    (cond ((< (point) pnt)
			   (setq last (- (point) 5)))
			  (t (setq found t))))
		   (t
		    (goto-char pnt)
		    (setq found t))))
	   (goto-char last)))
	((and eclipse-quick-jumps-selected (not flag))
	 ;; if quick jumps selected, assume clause begins at previous empty line
	 (let ((found nil))
	   (if (bolp) (eclipse-backward-char))
	   (while (and (not found) (not (bobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (forward-line -1)
	       (setq found t)))
	   (setq found nil)
	   (while (and (not found) (not (bobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (setq found t)
	       (forward-line -1)))
	   (or (and (bobp) (not (looking-at "[ \t]*\n")))
	       (progn (forward-line) (beginning-of-line)))))
	(t
	 (let ((last (point)) (pnt (point)) (found nil) maxpnt)
	   ;; else go to beginning of buffer, search all clause beginnings until we are past pnt
	   ;; the last clause beginning we found is the one we are looking for
	   (goto-char (point-min))
	   (beginning-of-line)
	   (eclipse-jump-over-strings nil t)
	   (setq last (point))
	   (if (<= pnt last)
	       (setq maxpnt (point-max))
	     (setq maxpnt pnt))
	   (while (and (not found) (not (eobp)))
	     (cond ((> (point) maxpnt)
		    (goto-char pnt)
		    (setq found t))
		   ((re-search-forward "[.\"'%/]" maxpnt t)		    
		    (eclipse-backward-char)
		    (cond ((looking-at "'")
			   (eclipse-goto-end-of-quote))
			  ((looking-at "\"")
			   (eclipse-goto-end-of-string))
			  ((looking-at "%")
			   (end-of-line)
			   (unless (eobp)
			     (forward-line)
			     (beginning-of-line)))
			  ((looking-at "/\\*")
			   (unless (re-search-forward "\\*/" (point-max) t)
			     (goto-char (point-max))))
			  ((looking-at "/")
			   (forward-char))
			  ((looking-at "[.][.]+")
			   (forward-char (length (match-string 0))))
			  ((looking-at "[.][^ \t\n,]")
			   (forward-char))
			  ((looking-at "[.]")
			   (forward-char)
			   (let ((aux (point)))
			     (eclipse-jump-over-strings nil t)
			     (if (not (save-excursion
					(re-search-forward "[.\"'%/]" (point-max) t)))
				 (setq found t)
			       (cond ((eq aux (point))
				      (setq found t))
				     ((< (point) pnt)
				      (setq last (point)))
				     (t (setq found t))))))
			  (t
			   (goto-char pnt)
			   (setq found t))))
		   (t
		    (goto-char pnt)
		    (setq found t))))
	   (goto-char last)))))

(defun eclipse-goto-clause-end ()
  "Goto the end of the current clause."
  (interactive)
  (cond (eclipse-esp-selected
	 (if (looking-at "\\?>")
	     (forward-char 2))
	 (if (re-search-forward "\\?>" (point-max) t)
	     (backward-char 2)))
	(eclipse-quick-jumps-selected
	 ;; if quick jumps selected, assume clause ends at next empty line
	 (let ((found nil))
	   (if (save-excursion (eclipse-backward-char) (looking-at ","))
	       (eclipse-skip-comments-and-empty-lines t)
	     (if (save-excursion
		   (eclipse-backward-char 2)
		   (looking-at "[^.].[^.0-9]"))   ; should we update the regexp to include "." as atom?
		 (forward-line))
	     (beginning-of-line)
	     (eclipse-skip-comments-and-empty-lines t))
	   (while (and (not found) (not (eobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (forward-line)
	       (setq found t)))
	   (setq found nil)
	   (while (and (not found) (not (eobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (setq found t)
	       (forward-line)))
	   (eclipse-end-of-clause)
	   (if (eobp)
	       (skip-chars-backward " \t\n")
	     (forward-line -1)
	     (eclipse-end-of-clause))))
	(t
	 (let ((found nil))
	   ;; else go to beginning of buffer, search all clause ends until
	   ;; we are at the first clause beginning after pnt
	   ;; the last clause end we found is the one we are looking for
	   (eclipse-skip-comments-and-empty-lines t)
	   (unless (eclipse-check-clause-begin)
	     (eclipse-goto-clause-begin))
	   (while (not found)
	     (if (not (re-search-forward "[.\"'%/ \t]" (point-max) t))
		 (setq found t)
	       (eclipse-backward-char)
	       (cond ((eobp) (setq found t))
		     ((looking-at "'")
		      (eclipse-goto-end-of-quote))
		     ((looking-at "\"")
		      (eclipse-goto-end-of-string))
		     ((looking-at "[.][.]+")
		      (forward-char (length (match-string 0))))
		     ((looking-at "[.][^ \t\n,]")
		      (forward-char))
		     ((looking-at "[.]")
		      (forward-char)
		      (setq found t))
		     ((looking-at "%")
		      (forward-line)
		      (beginning-of-line))
		     ((looking-at "/\\*")
		      (unless (re-search-forward "\\*/" (point-max) t)
			(goto-char (point-max))))
		     ((looking-at "[ \t]")
		      (forward-char))
		     (t (forward-char)))))))))

(defun eclipse-goto-predicate (&optional flag)
  ;; common loop for goto-predicate functions
  (let ((found nil) (last nil))
    (while (not found)
      (if (not (re-search-forward "[.\"'%/]" (point-max) t))
	  (setq found t)
	(eclipse-backward-char)
	(cond ((and flag (eobp))
	       (setq found t))
	      ((looking-at "'")
	       (eclipse-goto-end-of-quote))
	      ((looking-at "\"")
	       (eclipse-goto-end-of-string))
	      ((looking-at "%")
	       (forward-line)
	       (beginning-of-line))
	      ((looking-at "/\\*")
	       (unless (re-search-forward "\\*/" (point-max) t)
		 (goto-char (point-max))))
	      ((looking-at "[.][.]+")
	       (forward-char (length (match-string 0))))
	      ((looking-at "[.][^ \t\n,]")
	       (forward-char))
	      ((looking-at "[.]")
	       (forward-char)
	       (cond (flag ; searching for predicate end
		      (setq last (point))
		      (eclipse-jump-over-strings)
		      (unless (string-equal template
					    (eclipse-get-current-predicate-template))
			(setq found t)
			(goto-char last)))
		     (t ; searching for predicate begin
		      (eclipse-jump-over-strings nil t)
		      (if (string-equal template
					(eclipse-get-current-predicate-template))
			  (setq found t)
			(forward-char)))))
	      (t (forward-char)))))))

(defun eclipse-goto-predicate-begin ()
  "Goto the beginning of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (if eclipse-quick-jumps-selected
	;; if quick jumps selected, go to clause begin, get template
	;; iterate until template does not match anymore, then return to last match
	(let ((found nil) (last nil) (template nil))
	  (eclipse-goto-clause-begin)
	  (setq template (eclipse-get-current-predicate-template)
		last (point))
	  (while (and (not found) (not (bobp)))
	    (eclipse-goto-clause-begin)
	    (if (string-equal template (eclipse-get-current-predicate-template))
		(setq last (point))
	      (setq found t)))
	  (or (not found) (goto-char last)))
      (let ((found nil) pnt template)
	;; else extract current template, go to beginning of buffer,
	;; search all clause beginnings until we find the first clause with matching template
	(eclipse-goto-clause-begin)
	(unless (looking-at "[:?]-")
	  (setq pnt (point)
		template (eclipse-get-current-predicate-template))
	  (goto-char (point-min))
	  (eclipse-jump-over-strings nil t)
	  (if (string-equal template (eclipse-get-current-predicate-template)) t
	    (eclipse-jump-over-strings nil)
	    (eclipse-goto-predicate)
	    (if (> (point) pnt) (goto-char pnt))))))))
  
(defun eclipse-goto-predicate-end ()
  "Goto the end of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (if eclipse-quick-jumps-selected
	;; if quick jumps selected, go to clause begin, get template
	;; then go to next clause end, jump to next clause begin and check template,
	;; iterate until template does not match anymore, then return to last clause end
	(let ((found nil) (last nil) (template nil))
	  (eclipse-goto-clause-end)
	  (setq last (point))
	  (save-excursion
	    (eclipse-goto-clause-begin)
	    (setq template (eclipse-get-current-predicate-template)))
	  (while (and (not found) (not (eobp)))
	    (eclipse-goto-clause-end)
	    (cond ((eq last (point)) (setq found t))
		  ((save-excursion
		     (eclipse-goto-clause-begin)
		     (string-equal template (eclipse-get-current-predicate-template)))
		   (setq last (point)))
		  (t (setq found t))))
	  (or (not found) (goto-char last)))
      (let ((found nil) (last nil) template)
	;; else extract current template, go to end of clause,
	;; search all following clause beginnings until we find the
	;; first clause with different template, then return to last clause end
	(eclipse-skip-comments-and-empty-lines t)
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(skip-chars-forward " \t")
	(if (looking-at "[:?]-")
	    (eclipse-goto-clause-end)
	  (setq template (eclipse-get-current-predicate-template))
	  (eclipse-goto-predicate t))))))

;;
;; Speedbar support
;;

(defun eclipse-goto-prev-index-position ()
  ;; go to the previous entry in the index
  (beginning-of-line)
  (if (bobp)
      nil
    (let ((now (point)))
      (eclipse-goto-predicate-begin)
      (if (eq now (point))
	  nil
	t))))

(defun eclipse-create-index ()
  ;; creates an index for the speedbar.
  ;; this function scancs the buffer top-down, which is faster than scanning
  ;; bottom-up, as is standard in speedbar/imenu, since this way, we can use
  ;; the information, that the point is always at the beginning of a
  ;; predicate when the next predicate is searched
  (save-excursion
    (let ((index-alist '()) (index-dir-alist '())
	  (length (- (point-max) (point-min))) (last -1) (pc 0) name
	  entry)
      (message "Indexing...")
      (goto-char (point-min))
      (eclipse-goto-clause-begin)   ;; quick and dirty...
      ;; Search for the function
      (while (and (not (eobp)) (< (point) (point-max)) (not (eq last (point))))
	(setq pc (eclipse-percent-message "Indexing" length pc)
	      last (point)
	      name (eclipse-extract-index-name)
	      entry (cons name last))
	(if (looking-at "[:?]-")
	    ;; if directive, at to list of directives
	    (setq index-dir-alist (cons entry index-dir-alist))
	  ;; else add to the list of normal entries
	  (setq index-alist (cons entry index-alist)))
	;; go to next predicate
	(eclipse-goto-predicate-end)
	(eclipse-jump-over-strings t t)
	(skip-chars-forward " \t"))
      (message "Indexing...done.")
      (and index-dir-alist
	   (setq index-alist (cons (cons "Directives" index-dir-alist) index-alist)))
      index-alist)))

(defun eclipse-extract-index-name ()
  ;; get the name to be listed in the index
  (let (start name)
    (save-excursion
      (cond ((looking-at "[ \t]*[:?]-")
	     ;; directive: extract all of it
	     (skip-chars-forward " \t")
	     (forward-char 2)
	     (re-search-forward "[a-z]" (point-max) t)
	     (setq start (- (point) 1))
	     (re-search-forward "[.\n%]" (point-max) t)
	     (backward-char)
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    ((looking-at "[A-Z]")
	     ;; variable: probably a clause head with infix operator
	     ;; extract until colon-dash or end of line
	     (setq start (point))
	     (re-search-forward "\\(\n\\|[:?]-\\)" (point-max) t)
	     (backward-char)
	     (when (looking-at "-")
	       (backward-char))
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    (t ; else extract predicate template
	     (setq name (eclipse-get-current-predicate-template t)))))
    name))

;;
;; Predicate template & args and other "edit" functions
;;

(defun eclipse-get-current-predicate-template (&optional specflag)
  ;; return the template for the current predicate
  ;; if specflag = t, return the specification for the current predicate
  ;; problem: cannot handle operators in clause heads:
  ;; X = Y :- ...
  ;; ++ X :- ...
  ;; X ++ :- ...
  ;; since Emacs doesn't know about the operator definitions. And since the
  ;; arguments in the clause heads may be atoms (just like the operators),
  ;; it is impossible to guarantee the correct behaviour in this case!
  (let ((template nil) (fb (point)) fe functor args (cc 0) (found nil) (bc 0))
    ;; we are at the beginning of a clause...
    (save-excursion
      ;; search end of predicate name
      (re-search-forward "\\([\n(.]\\|[:?]-\\)" (point-max) t)
      (eclipse-backward-char)
      (cond ((and (looking-at "-")
		  (save-excursion
		    (backward-char)
		    (looking-at "[:?]-")))
	     (backward-char))
;;	    ((looking-at "[\n.]")
;; 	     (backward-char))
	    (t t))
      (skip-chars-backward " \t")
      (setq fe (point)
	    functor (buffer-substring-no-properties fb fe))
      (cond ((looking-at "(")
	     ;; if there are any arguments, count them
	     (while (not found)
	       (cond ((eobp) (setq found t))
		     ((looking-at "'")
		      (eclipse-goto-end-of-quote))
		     ((looking-at "\"")
		      (eclipse-goto-end-of-string))
		     ((looking-at "%")
		      (forward-line)
		      (beginning-of-line))
		     ((looking-at "/\\*")
		      (unless (re-search-forward "\\*/" (point-max) t)
			(goto-char (point-max))))
		     ((looking-at "[({[]")
		      (forward-char)
		      (setq bc (+ bc 1)))
		     ((looking-at "[]})]")
		      (forward-char)
		      (setq bc (- bc 1))
		      (when (zerop bc)
			(setq found t)))
		     ((looking-at ",")
		      (forward-char)
		      (when (= bc 1)
			(setq cc (+ cc 1))))
		     (t (forward-char))))
	     (if specflag
		 (setq args (concat "/" (number-to-string (+ cc 1))))
	       (setq args (concat "(" (make-string cc 44) ")"))))
	    (t
	     (if specflag
		 (setq args "/0")
	       (setq args ""))))
      (setq template (concat functor args))
      template)))

(defun eclipse-get-current-predicate-args ()
  ;; return the arguments for the current term
  ;; this should be improved, so that comments get stripped automatically
  ;; also, this function and eclipse-get-current-predicate-template should
  ;; be rolled into one
  (let ((args '()) fb fe (found nil) (bc 0) next (arg nil))
    ;; we are at the beginning of a clause...
    (save-excursion
      ;; search end of predicate name
      (or (re-search-forward "[ \t\n(:]" (point-max) t)
	  (search-forward "."))
      (eclipse-backward-char)
      ;; if there are any arguments...
      (when (looking-at "(")
	(setq bc 1)
	(forward-char)
	(setq fb (point))
	;; extract the arguments, jumping over comments
	(while (not found)
	  (cond ((eobp) ; end of buffer: finish search
		 (setq found t
		       fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if (not arg)
		     (setq args (append args (list next)))
		   (setq args (append args (list (concat arg next)))
			 arg nil)))
		((looking-at "'") ; quoted atom: go to end
		 (eclipse-goto-end-of-quote))
		((looking-at "\"") ; string: go to end
		 (eclipse-goto-end-of-string))
		((looking-at "%") ; comment: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (forward-line)
		 (beginning-of-line)
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		((looking-at "/\\*") ; c-style comment: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (unless (re-search-forward "\\*/" (point-max) t)
		   (goto-char (point-max)))
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		((looking-at "[({[]") ; increase bracket counter
		 (forward-char)
		 (setq bc (+ bc 1)))
		((looking-at "[]})]") ; decrease bracket counter
		 (setq bc (- bc 1))
		 (cond ((zerop bc)
			(setq found t
			      fe (point)
			      next (buffer-substring-no-properties fb fe))
			(if (not arg)
			    (setq args (append args (list next)))
			  (setq args (append args (list (concat arg next)))
				arg nil))
			(forward-char))
		       (t (forward-char))))
		((looking-at ",")
		 (cond ((= bc 1)
			;; next argument found, add it to list
			(setq fe (point)
			      next(buffer-substring-no-properties fb fe))
			(if (not arg)
			    (setq args (append args (list next)))
			  (setq args (append args (list (concat arg next)))
				arg nil))
			(forward-char)
			(eclipse-skip-comments-and-empty-lines)
			(setq fb (point)))
		       (t (forward-char))))
		((looking-at "[ \t\n]") ; whitespace or new line: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		(t (forward-char)))))
      args)))

(defun eclipse-insert-predicate-template ()
  "Insert the template of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil))
      (save-excursion
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(setq template (eclipse-get-current-predicate-template)))
      (insert template)
      (when (save-excursion (backward-char) (looking-at ")"))
	(search-backward "(")
	(forward-char)))))

(defun eclipse-insert-predicate-spec ()
  "Insert the specification of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil))
      (save-excursion
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(setq template (eclipse-get-current-predicate-template t)))
      (insert template))))

(defun eclipse-insert-clause-head ()
  "Insert a new clause head of the current predicate with the arguments of the last clause."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil) (this (point)) spec vars functor arity aux next)
      (save-excursion
	(eclipse-goto-clause-begin)
	(setq spec (eclipse-get-current-predicate-template t)
	      vars (eclipse-get-current-predicate-args)
	      aux (split-string spec "/")
	      functor (nth 0 aux)
	      arity (string-to-number (nth 1 aux))))
      (unless (string-equal spec "/0")
	(insert (concat "\n" functor))
	(unless (zerop arity)
	  (insert "(")
	  (while (car vars)
	    (setq next (car vars)
		  vars (cdr vars))
	    (insert next)
	    (if (car vars) (insert ",")))
	  (insert ")"))
	(insert " :-\n")
	(goto-char this)
	(re-search-forward "[(:]" (point-max) t)
	(backward-char)
	(if (looking-at "(")
	    (forward-char)
	  (forward-char 3)
	  (eclipse-indent-line))))))
  
(defun eclipse-insert-clause-head-empty ()
  "Insert a new clause head of the current predicate without arguments."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil) (this (point)))
      (save-excursion
	(eclipse-goto-clause-begin)
	(setq template (eclipse-get-current-predicate-template)))
      (unless (string-equal "" template)
	(insert (concat "\n" template " :-\n"))
	(goto-char this)
	(re-search-forward "[(:]" (point-max) t)
	(backward-char)
	(if (looking-at "(")
	    (forward-char)
	  (forward-char 3)
	  (eclipse-indent-line))))))
  
(defun eclipse-anonymise-variables ()
  "Add _ to all variables in the current region."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    ;; goto first non-word after rbegin (we don't want to insert a "_" in the middle of a word!)
    (eclipse-anonymise-loop rbegin 0)
    ;; anonymise the rest of the variables until rend is reached
    (eclipse-anonymise-loop rend 1)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymous-variables ()
  "Replaces the variables in the current region with anonymous variables."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    ;; goto first non-word after rbegin (we don't want to replace the end of a word!)
    (eclipse-anonymise-loop rbegin 0)
    ;; replace the rest of the variables until rend is reached
    (eclipse-anonymise-loop rend 2)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymise-loop (pos flag)
  ;; if flag = 0, do nothing
  ;; if flag = 1, anonymise variables
  ;; if flag = 2, replace with anonymous variables ("_")
  ;; loop until first non-word at or after pos is reached
  (while (not (or (> (point) pos) (eobp)))
    (cond ((looking-at "[A-Z]")
	   (unless (zerop flag)
	     (insert "_")
	     (setq pos (+ pos 1)))
	   (when (= flag 2)
	     (while (looking-at "[_a-zA-Z0-9]")
	       (delete-char 1)
	       (setq pos (- pos 1))))
	   (re-search-forward "[^_a-zA-Z0-9]" (point-max) t)
	   (backward-char))
	  ((and (looking-at "_") (= flag 2))
	   (forward-char)
	   (while (looking-at "[_a-zA-Z0-9]")
	     (delete-char 1)
	     (setq pos (- pos 1)))
	   (re-search-forward "[^_a-zA-Z0-9]" (point-max) t)
	   (backward-char))
	  ((looking-at "'")
	   (eclipse-goto-end-of-quote))
	  ((looking-at "\"")
	   (eclipse-goto-end-of-string))
	  ((looking-at "[a-z0-9_]")
	   (re-search-forward "[^_a-zA-Z0-9']" (point-max) t)
	   (backward-char))
	  ((looking-at "%")
	   (forward-line)
	   (beginning-of-line))
	  ((looking-at "/\\*")
	   (unless (re-search-forward "\\*/" (point-max) t)
	     (goto-char (point-max))))
	  (t
	   (forward-char)
	   (if (re-search-forward "[a-zA-Z0-9_'\"%/\\]" (point-max) t)
	       (backward-char)
	     (goto-char pos)
	     (or (eobp) (forward-char)))))))

(defun eclipse-insert-comment-pred-short ()
  "Insert \":- comment(,).\" into the program text."
  (interactive)
  (unless eclipse-esp-selected
    (insert ":- comment(,).\n")
    (forward-line -1)
    (beginning-of-line)
    (search-forward "(")))

(defun eclipse-insert-comment-pred-full ()
  "Insert comment/2 call with all arguments into program text."
  (interactive)
  (unless eclipse-esp-selected
    (let (pnt spec vars functor arity aux next)
      (eclipse-jump-over-strings)
      (setq spec (eclipse-get-current-predicate-template t))
      (message spec)
      (if (string-equal spec "/0")
	  (eclipse-insert-comment-pred-short)
	(setq vars (eclipse-get-current-predicate-args)
	      aux (split-string spec "/")
	      functor (nth 0 aux)
	      arity (string-to-number (nth 1 aux)))
	(insert (concat ":- comment(" spec  ", [\n"))
	(setq pnt (point))
	(insert "        summary:,\n")
	(unless (zerop arity)
	  (insert (concat "        amode:" functor (concat "(" (make-string (- arity 1) 44) ")") ",\n"
			  "        args:[\n"))
	  (while (car vars)
	    (setq next (car vars)
		  vars (cdr vars))
	    (insert (concat "                 \"" next))
	    (if (car vars)
		(insert "\": ,\n")
	      (insert "\": \n")))
	  (insert "             ],\n"))
	(insert (concat "        desc:,\n"
			"        fail_if:,\n"
			"        resat:,\n"
			"        eg:,\n"
			"        see_also:,\n"
			"        index:]).\n\n"))
	(goto-char pnt)
	(search-forward ":")))))
  
;;
;; dabbrev support for ECLiPSe keywords
;;

(defun eclipse-dabbrev-expand ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion including arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-expand1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion without arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "[a-zA-Z][a-zA-Z_0-9]*(?"))

(defun eclipse-dabbrev-expand2 (reg)
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    ;; a bit of a hack:
    ;; we want our own keyword list searched first,
    ;; then the current buffer etc.
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-dabbrev-expand0 ()
  "Automatic expansion by dabbrev.
Checks for expansions in current buffer first, then for predefined keywords."
  (interactive)
  (let (aux1 aux2)
    (eclipse-load-dabbrev)
    (setq aux1 dabbrev-search-these-buffers-only
	  aux2 dabbrev-abbrev-char-regexp
	  dabbrev-abbrev-char-regexp nil)
    (eclipse-update-dabbrev-list (current-word) t)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp aux2
	  dabbrev-search-these-buffers-only aux1)))

(defun eclipse-dabbrev-completion ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansions including arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-completion1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansions without arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "[a-zA-Z][a-zA-Z_0-9]*(?"))

(defun eclipse-dabbrev-completion2 (reg)
  ;; automatic expansion of ECLiPSe keywords
  ;; returns list of possible expansions
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-completion nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-load-dabbrev ()
  ;; load dabbrev if needed, and load the keyword list
  (require 'dabbrev)
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (make-local-variable 'dabbrev-search-these-buffers-only))

(defun eclipse-update-dabbrev-list (keyword &optional flag)
  ;; get list of keywords from ECLiPSe, via help/1 call
  (get-buffer-create "*eclipse-keywords*")
  (unless (if dabbrev--last-abbrev-location
	      (if (numberp dabbrev--last-abbrev-location)
		  ;; dabbrev--last-abbrev-location can either be number or
		  ;; marker!?
		  (= dabbrev--last-abbrev-location (point))
		(= (marker-position dabbrev--last-abbrev-location) (point))))
    (let (help-call)
      ;; call ECLiPSe help for keyword
      (setq help-call (concat eclipse-help-call1 (downcase keyword) eclipse-help-call2))
      ;; parse output
      (save-excursion
	(set-buffer "*eclipse-keywords*")
	(goto-char (point-min))
	(delete-char (- (point-max) (point)))
	(insert (shell-command-to-string help-call))
	(goto-char (point-min))
	(if (looking-at "string stream")
	    (delete-char (- (point-max) (point-min)))
	  (while (not (eobp))
	    (cond ((looking-at "----")
		   (delete-char 4)
		   (delete-blank-lines)
		   (if (looking-at "Call")
		       (delete-char (- (point-max) (point)))
		     (let ((aux1 (point)) aux2)
		       (end-of-line)
		       (search-backward ":" aux1 t)
		       (when (looking-at ":")
			 (forward-char)
			 (skip-chars-forward " \t")
			 (setq aux1 (point))
			 (beginning-of-line)
			 (delete-char (- aux1 (point))))
		       (cond ((looking-at "lib([a-z_]+)")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     ((looking-at "[a-z]+ [a-z]+/index")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     (t (forward-line)))
		       (beginning-of-line)
		       (setq aux1 (point))
		       (search-forward "----" (eobp) t)
		       (backward-char 4)
		       (setq aux2 (point))
		       (goto-char aux1)
		       (delete-char (- aux2 aux1)))))
		  ((looking-at "[ \t]*\n")
		   (delete-blank-lines))))))))
  ;; update dabbrev buffer list
  (let ((blist (get-all-eclipse-buffers)))
    (if flag
	(setq blist (append (list (buffer-name) "*eclipse-keywords*")
			    (cdr blist)))
      (setq blist (append (list "*eclipse-keywords*") blist))
    (setq dabbrev-search-these-buffers-only blist))))

(defun get-all-eclipse-buffers ()
  ;; get list of all ECLiPSe buffers
  (let ((blist (list (buffer-name)))
	(all-buffers (cdr (buffer-list)))
	next next-name ext)
    (while (car all-buffers)
      (setq next (car all-buffers)
	    all-buffers (cdr all-buffers)
	    next-name (buffer-name next)
	    ext (nth 1 (split-string next-name "\\.")))
      (when (member ext (list "ecl" "ECL" "pl" "PL" "esp" "ESP"))
	(setq blist (append blist (list next-name)))))
    blist))

;;
;; Module loading & help functions
;;

(defun eclipse-load-all-modules ()
  "Load all project files specified in all buffers containing ECLiPSe programs,

i.e. any file inside a use_module/1, ensure_loaded/1, compile/1, or include/1
call."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-load-modules t)))

(defun eclipse-load-modules (&optional flag)
  "Load all project files specified in the current buffer,

i.e. any file inside a use_module/1, ensure_loaded/1, compile/1, or include/1
call."
  (interactive)
  (unless eclipse-esp-selected
    (let* ((blist (if flag (buffer-list) ; start with current buffer
		    (list (car (buffer-list))))) ; or all current ECLiPSe buffers
	   (all-buffers blist)
	   name ext begin end filename buffer)
      (save-excursion
	(while (car blist) ; iterate through all buffers
	  (setq name (buffer-name (car blist))
		ext (nth 1 (split-string name "\\.")))
	  (when (member ext (list "ecl" "ECL" "pl" "PL" "esp" "ESP"))
	    (set-buffer name)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (cond ((re-search-forward "\\<\\(use_module\\|ensure_loaded\\|compile\\|include\\)('?"
					(point-max) t)
		     (setq begin (point))
		     (re-search-forward "[')]" (point-max) t)
		     (setq end (- (point) 1)
			   filename (buffer-substring begin end))
		     ;; try all combinations...
		     (cond ((file-exists-p filename) t)
			   ((file-exists-p (concat filename ".ecl"))
			    (setq filename (concat filename ".ecl")))
			   ((file-exists-p (concat filename ".pl"))
			    (setq filename (concat filename ".pl")))
			   ((file-exists-p (concat filename ".ECL"))
			    (setq filename (concat filename ".ECL")))
			   ((file-exists-p (concat filename ".PL"))
			    (setq filename (concat filename ".PL")))
			   (t (setq filename nil)))
		     (unless (not filename)
		       ;; open file (if not yet open)
		       (setq buffer (find-file-noselect filename t))
		       ;; add to list of buffers (if not yet in that list)
		       (unless (member buffer all-buffers)
			 (setq blist (append blist (list buffer))
			       all-buffers (append all-buffers (list buffer))))))
		    (t (goto-char (point-max))))))
	  (setq blist (cdr blist)))))))

(defun eclipse-call-help ()
  "Call the ECLiPSe help.

You will be asked for the predicate name for which you need help.
The input will be passed on to the help/1 predicate.
The output will be presented in the buffer *eclipse-help*.
The format for the help call is Name for simple help and 
<Module:>Name/Arity for detailed help."
  (interactive)
  (eclipse-load-dabbrev)
  (let ((help-call (downcase (current-word))) aux res)
    (setq aux (read-from-minibuffer 
	       (if help-call
		   (concat "Describe predicate (default " help-call "): ")
		 "Describe predicate: ")))
    (cond ((not help-call) ; no current word: use input from mini-buffer
	   (unless (= 0 (string-width aux))
	     (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2))
	     (setq res (shell-command help-call "*eclipse-help*"))))
	  ((= 0 (string-width aux)) ; no input from mini-buffer: use current word
	   (unless (= 0 (string-width help-call))
	     (setq help-call (concat eclipse-help-call1 help-call eclipse-help-call2))
	     (setq res (shell-command help-call "*eclipse-help*"))))
	  (t ; else use input from mini-buffer
	   (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2))
	   (setq res (shell-command help-call "*eclipse-help*"))))
    (when (eq res 1)
      (message (concat "No help available for \"" aux "\"")))))

;;
;; Highlighting
;;

(defun eclipse-highlight ()
  "Highlight all occurrences of the current word in the current buffer.
Any other highlighting is removed."
  (interactive)
  (when (or (looking-at "[a-zA-Z_]")
	    (and (looking-at "[0-9_]")
		 (save-excursion
		   (re-search-backward "[^a-zA-Z0-9_]" (point-min) t)
		   (forward-char)
		   (looking-at "[a-zA-Z_][a-zA-Z0-9_]+"))))
    (let* ((beg (save-excursion
		  (re-search-backward "[^a-zA-Z0-9_]" (point-min) t)
		  (forward-char)
		  (point)))
	   (end (save-excursion
		  (re-search-forward "[^a-zA-Z0-9_]" (point-max) t)
		  (backward-char)
		  (point)))
	   (str (buffer-substring beg end)) ; current word
	   (len (length str))
	   ovl)
      (if (string-equal str eclipse-highlighted) ; the same? remove
	  (eclipse-dehighlight)
	(eclipse-dehighlight) ; remove old
	(setq eclipse-highlighted str)
	;; highlight all occurrences in current buffer
	(save-excursion
	  (goto-char (point-min))
	  (while (not (= (point) (point-max)))
	    (when (search-forward str (point-max) 1)
	      (backward-char len)
	      (setq beg (point)
		    end (+ beg len)
		    ovl (make-overlay beg end))
	      (overlay-put ovl 'face 'eclipse-highlight-face)
	      (eclipse-add-overlay ovl)
	      (forward-char len))))))))

(defun eclipse-add-overlay (ovl)
  ;; add overlay to list of overlays
  (if eclipse-overlays
      (setq eclipse-overlays (append eclipse-overlays (list ovl)))
    (setq eclipse-overlays (list ovl))))

(defun eclipse-dehighlight ()
  "Remove any highlighting from the current buffer."
  (interactive)
  (let (ovl)
    (while eclipse-overlays
      (setq ovl (car eclipse-overlays)
	    eclipse-overlays (cdr eclipse-overlays))
      (delete-overlay ovl))
    (setq eclipse-highlighted nil)))

(defun eclipse-goto-highlight-backward ()
  "Go backwards to previous occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (overlay-recenter (point))
    (let* ((ol (overlay-lists))
	   (list (append (reverse (car ol)) (cdr ol)))
	   last)
      (while (<= (overlay-start (car list)) (point))
	(setq last (car list)
	      list (cdr list)))
      (when last
	(goto-char (overlay-start last))))))

(defun eclipse-goto-highlight-forward ()
  "Go forwards to next occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (overlay-recenter (point))
    (let* ((ol (overlay-lists))
	   (list (append (reverse (car ol)) (cdr ol))))
      (while (and list (<= (overlay-start (car list)) (point)))
	(setq list (cdr list)))
      (when list
	(goto-char (overlay-start (car list)))))))

;;
;; Metrics
;;

(defun count-to-end (esp-flag fun &rest args)
  ;; count number of non-empty lines from current point to point
  ;; identified by evaluating function fun with arguments args
  (let* ((start (point))
	 (aux (save-excursion (eval (append (list fun) args))
			      (point)))
	 (end (if (and esp-flag (= aux (point)))
		  (point-max)
		aux))
	(count 0))
    (while (< (point) end)
      (unless (looking-at "^\\s-*\n")
	(setq count (+ 1 count)))
      (forward-line))
    (unless (or (= end (point)) (and esp-flag (eobp)))
      (forward-line -1))
    count))

(defun count-comments-to-end-esp (fun &rest args)
  ;; count number of comment lines from current point to point
  ;; identified by evaluating function fun with arguments args
  (let ((start (point))
	(end (save-excursion (eval (append (list fun) args))
			     (point)))
	(count 0))
    (while (< (point) end)
      (cond ((looking-at "^\\s-*%")
	     (setq count (+ 1 count)))
	    ((looking-at "/\\*")
	     ;; C-style comment
	     (setq aux (count-to-end nil 're-search-forward "\\*/"
				     (point-max) t)
		   count (+ count aux)))
	    (t t))
      (forward-line))
    (unless (= end (point))
      (forward-line -1))
    count))

(defmacro update-metrics (n min max)
  (list 'if (list '= min -1)
	(list 'setq max n min n)
    (list 'when (list '> n 0)
	  (list 'cond (list (list '> n max) (list 'setq max n))
		(list (list '< n min) (list 'setq min n))
		(list 't 't)))))

(defun float-metrics (n1 n2)
  (if (= n1 0)
      "0.00"
    (concat (number-to-string (truncate (* 1.0 n1) n2)) "."
	    (let ((n (truncate (% (/ (* 100 n1) n2) 100))))
	      (if (= n 0)
		  "00"
		(number-to-string n))))))

(defun eclipse-get-metrics (name esp-mode-flag no-msg-flag)
  ;; computes metrics for current buffer
  (let (metrics (pred_end -1) (clause_end -1) (size (buffer-size))
		(total 0) (comments 0) (pc 0))
    (cond (esp-mode-flag
	   (let ((esp_segm 0) (esp_segm_code 0) (esp_segm_min -1)
		 (esp_segm_max 0) (esp_expr 0) (esp_expr_code 0)
		 (esp_expr_min -1) (esp_expr_max 0) (esp_directives 0)
		 (esp_dir_code 0) (esp_dir_min -1) (esp_dir_max 0)
		 (html 0) aux l_esp_segm l_esp_dir l_esp_expr
		 aux_line_beg aux1)
	     (save-excursion
	       (set-buffer name)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (unless no-msg-flag
		   (setq pc (eclipse-percent-message "Computing metrics"
						     size pc)))
		 (skip-chars-forward " \t\n")
		 (cond ((eobp) t)
		       ((looking-at "<\\?esp")
			(let (esp_code esp_tag)
			  (cond ((looking-at "<\\?esp:compile")
				 (setq esp_tag 2
				       esp_directives (1+ esp_directives)))
				((looking-at "<\\?esp=")
				 (setq esp_tag 1
				       esp_expr (1+ esp_exprs)))
				(t
				 (setq esp_tag 0
				       esp_segm (1+ esp_segm))))
			  (setq aux1 (point)
				aux1 (save-excursion
				       (beginning-of-line)
				       (skip-chars-forward " \t")
				       (= aux1 (point)))
				aux_line_beg (line-beginning-position)
				aux (count-to-end t 're-search-forward
						  "\\?>" (point-max) t)
				aux (if aux1 (1- aux) aux)
				total (+ total (if aux1 aux (1- aux)))
				aux1 aux
				esp_code aux)
			  (unless (= aux_line_beg (line-beginning-position))
			    (save-excursion
			      (goto-char aux_line_beg)
			      (setq aux (count-comments-to-end-esp
					 're-search-forward
					 "\\?>" (point-max) t)
				    comments (+ comments aux)
				    esp_code (- esp_code aux)
				    aux (- aux1 aux))))
			  (cond ((= esp_tag 2)
				 (update-metrics aux esp_dir_min esp_dir_max)
				 (setq esp_dir_code (+ esp_dir_code
						       esp_code)))
				((= esp_tag 1)
				 (update-metrics aux esp_expr_min esp_expr_max)
				 (setq esp_expr_code (+ esp_expr_code
							esp_code)))
				((= esp_tag 0)
				 (update-metrics aux esp_segm_min esp_segm_max)
				 (setq esp_segm_code (+ esp_segm_code
							esp_code))))
			  (forward-line)))
		       (t ; HTML code
			(setq aux (count-to-end t 're-search-forward
						"<\\?esp" (point-max) t)
			      total (+ total aux)
			      html (+ html aux))
			(unless (or (looking-at "<\\?esp") (eobp))
			  (re-search-forward "<\\?esp" (point-max) t)
			  (backward-char 5))))))
	     (setq l_esp_segm (float-metrics esp_segm_code esp_segm)
		   l_esp_dir (float-metrics esp_dir_code esp_directives)
		   l_esp_expr (float-metrics esp_expr_code esp_expr)
		   metrics (list esp-mode-flag esp_segm esp_segm_code
				 l_esp_segm (max 0 esp_segm_min)
				 esp_segm_max esp_directives esp_dir_code
				 l_esp_dir (max 0 esp_dir_min)
				 esp_dir_max esp_expr esp_expr_code
				 l_esp_expr (max 0 esp_expr_min)
				 esp_expr_max total html comments size))))
	  (t
	   (let ((predicates 0) (clauses 0) (cl_pr 0) (cl_pr_min -1)
		 (cl_pr_max 0) (code 0) (directives 0)
		 (l_pr 0) (l_pr_min -1) (l_pr_max 0) (l_cl 0) (l_cl_min -1)
		 (l_cl_max 0) cl_pr_aux l_pr_aux l_cl_aux aux (n_clauses 0)
		 (n_code_pr 0) (n_code_cl 0))
	     (save-excursion
	       (set-buffer name)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (unless no-msg-flag
		   (setq pc (eclipse-percent-message "Computing metrics"
						     size pc)))
		 (skip-chars-forward " \t\n")
		 (cond ((looking-at ":-[ \t]*comment(")
			;; comment/2 directive: counted as comment
			(setq aux (count-to-end nil 'eclipse-goto-clause-end)
			      total (+ total aux)
			      comments (+ comments aux)))
		       ((looking-at ":-")
			;; directive
			(setq aux (count-to-end nil 'eclipse-goto-clause-end)
			      total (+ total aux)
			      directives (+ directives aux)))
		       ((looking-at "%")
			;; comment
			(setq comments (1+ comments)
			      total (1+ total)))
		       ((looking-at "/\\*")
			;; C-style comment
			(setq aux (count-to-end nil 're-search-forward
						"\\*/" (point-max) t)
			      total (+ total aux)
			      comments (+ comments aux)))
		       ((or (looking-at "\n") (eobp))
			;; empty line
			t)
		       (t ;; normal line
			(cond ((> (point) pred_end) ; next predicate
			       (unless (= predicates 0)
				 ;; update clauses per predicate (min/max)
				 ;; for previous predicate
				 (update-metrics n_clauses cl_pr_min cl_pr_max)
				 ;; update lines per predicate (min/max)
				 ;; for previous predicate
				 (update-metrics n_code_pr l_pr_min l_pr_max)
				 ;; update lines per clause (min/max)
				 ;; for previous clause
				 (update-metrics n_code_cl l_cl_min l_cl_max))
			       ;; increase predicate and clause counter
			       (setq predicates (1+ predicates)
				     clauses (1+ clauses)
				     n_clauses 1
				     n_code_pr 0
				     n_code_cl 0
				     pred_end (save-excursion
						(progn
						  (eclipse-goto-predicate-end)
						  (point)))
				     clause_end (save-excursion
						  (progn
						    (eclipse-goto-clause-end)
						    (point)))))
			      ((> (point) clause_end) ; next clause
			       ;; update lines per clause (min/max)
			       ;; for previous clause
			       (update-metrics n_code_cl l_cl_min l_cl_max)
			       ;; increase predicate and clause counter
			       (setq clauses (1+ clauses)
				     n_clauses (1+ n_clauses)
				     n_code_cl 0
				     clause_end (save-excursion
						  (progn
						    (eclipse-goto-clause-end)
						    (point)))))
			      (t t))
			(setq total (1+ total)
			      n_code_cl (1+ n_code_cl)
			      n_code_pr (1+ n_code_pr)
			      code (1+ code))))
		 (forward-line)))
	     ;; update clauses per predicate (min/max)
	     ;; for previous predicate
	     (update-metrics n_clauses cl_pr_min cl_pr_max)
	     ;; update lines per predicate (min/max)
	     ;; for previous predicate
	     (update-metrics n_code_pr l_pr_min l_pr_max)
	     ;; update lines per clause (min/max)
	     ;; for previous clause
	     (update-metrics n_code_cl l_cl_min l_cl_max)
	     (setq cl_pr (float-metrics clauses predicates)
		   l_pr (float-metrics code predicates)
		   l_cl (float-metrics code clauses)
		   metrics (list esp-mode-flag predicates clauses cl_pr
				 (max 0 cl_pr_min)
				 cl_pr_max total code comments directives l_pr
				 (max 0 l_pr_min)
				 l_pr_max l_cl
				 (max 0 l_cl_min)
				 l_cl_max size)))))
  metrics))

(defun eclipse-display-metrics (&optional flag)
  "This function computes the following metrics for the current buffer:

1. number of predicates
2. number of clauses
3. average clauses per predicate (min/max)
4. total lines, excluding empty lines
5. lines of code
6. lines of comments
7. lines of directives
8. average lines per predicate (min/max)
9. average lines per clause (min/max)
10. size

comment/2 directives are counted as comments.

Note that the line count function only looks at the beginning of a line.
The following code:

    :- comment(summary, \"Line count example\").
               % comment with leading white space

    example :-                     % This comment is not counted
        writeln(\"Hello world!\"),  /* This C-style comment
    shows that you have to be careful to avoid comment lines
    that are difficult to recognize!*/
        writeln(\"Hello again!\"),
    /* This C-style comment is counted as a comment */
        writeln(\"Goodbye!\").

gives this line count:

    Line count for example.ecl:
    -----------------------
    code lines: 6
    comments  : 3

The reason is that two lines of the first C-style comment are counted as
code lines, since the start of the comment is not at the beginning of a line.


If the current buffer is a ECLiPSe Server Pages buffer (.esp), the
following metrics are computed:

1. number of ESP segments (<?esp ... ?>)
2. number of ESP directives (<?esp:compile ... ?>)
3. number of ESP expressions (<?esp= ... ?>)
4. total lines, excluding empty lines
5. lines of ESP segment
6. average lines per segment (min/max)
7. lines of ESP directives
8. average lines per directive (min/max)
9. lines of ESP expression
10. average lines per expression (min/max)
11. lines of HTML code
12. lines of comments
13. size
"
  (interactive)
  (let* ((name (buffer-name)) metrics result-window line res)
    (unless flag
      (message "Computing metrics..."))
    (setq metrics (eclipse-get-metrics name eclipse-esp-selected flag))
    (unless flag
      (message "Computing metrics...done")
      (get-buffer-create "*eclipse-stats*"))
    (set-buffer "*eclipse-stats*")
    (unless flag
      (delete-region (point-min) (point-max)))
    (goto-char (point-max))
    (setq line (concat "Metrics for " name ":"))
    (insert-metrics line metrics)
    (unless flag
      (display-buffer "*eclipse-stats*")
      (goto-char (point-min)))
    metrics))

(defun insert-metrics (line metrics)
  (let* ((esp-mode-flag (car metrics))
	 (metrics (cdr metrics)))
    (cond (esp-mode-flag
	   (let* ((esp_segm (car metrics))
		  (metrics (cdr metrics))
		  (esp_segm_code (car metrics))
		  (metrics (cdr metrics))
		  (l_esp_segm (car metrics))
		  (metrics (cdr metrics))
		  (esp_segm_min (car metrics))
		  (metrics (cdr metrics))
		  (esp_segm_max (car metrics))
		  (metrics (cdr metrics))
		  (esp_directives (car metrics))
		  (metrics (cdr metrics))
		  (esp_dir_code (car metrics))
		  (metrics (cdr metrics))
		  (l_esp_dir (car metrics))
		  (metrics (cdr metrics))
		  (esp_dir_min (car metrics))
		  (metrics (cdr metrics))
		  (esp_dir_max (car metrics))
		  (metrics (cdr metrics))
		  (esp_expr (car metrics))
		  (metrics (cdr metrics))
		  (esp_expr_code (car metrics))
		  (metrics (cdr metrics))
		  (l_esp_expr (car metrics))
		  (metrics (cdr metrics))
		  (esp_expr_min (car metrics))
		  (metrics (cdr metrics))
		  (esp_expr_max (car metrics))
		  (metrics (cdr metrics))
		  (total (car metrics))
		  (metrics (cdr metrics))
		  (html (car metrics))
		  (metrics (cdr metrics))
		  (comments (car metrics))
		  (metrics (cdr metrics))
		  (size (car metrics)))
	     (insert (concat line "\n"
			     (make-string (string-width line) 45) "\n"
			     "ESP segments       : "
			     (number-to-string esp_segm) "\n"
			     "ESP directives     : "
			     (number-to-string esp_directives) "\n"
			     "ESP expressions    : "
			     (number-to-string esp_expr) "\n"
			     "Total lines        : "
			     (number-to-string total) "\n"
			     "ESP segm. lines    : "
			     (number-to-string esp_segm_code) "\n"
			     "Lines per segment  : " l_esp_segm " ("
			     (number-to-string esp_segm_min) "/"
			     (number-to-string esp_segm_max) ")\n"
			     "ESP direct. lines  : "
			     (number-to-string esp_dir_code) "\n"
			     "Lines per directive: " l_esp_dir " ("
			     (number-to-string esp_dir_min) "/"
			     (number-to-string esp_dir_max) ")\n"
			     "ESP expr. lines    : "
			     (number-to-string esp_expr_code) "\n"
			     "Lines per expr.    : " l_esp_expr " ("
			     (number-to-string esp_expr_min) "/"
			     (number-to-string esp_expr_max) ")\n"
			     "HTML lines         : "
			     (number-to-string html) "\n"
			     "Comment lines      : "
			     (number-to-string comments) "\n"
			     "Size               : "
			     (number-to-string size) "\n"))))
	  (t
	   (let* ((predicates (car metrics))
		  (metrics (cdr metrics))
		  (clauses (car metrics))
		  (metrics (cdr metrics))
		  (cl_pr (car metrics))
		  (metrics (cdr metrics))
		  (cl_pr_min (car metrics))
		  (metrics (cdr metrics))
		  (cl_pr_max (car metrics))
		  (metrics (cdr metrics))
		  (total (car metrics))
		  (metrics (cdr metrics))
		  (code (car metrics))
		  (metrics (cdr metrics))
		  (comments (car metrics))
		  (metrics (cdr metrics))
		  (directives (car metrics))
		  (metrics (cdr metrics))
		  (l_pr (car metrics))
		  (metrics (cdr metrics))
		  (l_pr_min (car metrics))
		  (metrics (cdr metrics))
		  (l_pr_max (car metrics))
		  (metrics (cdr metrics))
		  (l_cl (car metrics))
		  (metrics (cdr metrics))
		  (l_cl_min (car metrics))
		  (metrics (cdr metrics))
		  (l_cl_max (car metrics))
		  (metrics (cdr metrics))
		  (size (car metrics)))
	     (insert (concat line "\n"
			     (make-string (string-width line) 45) "\n"
			     "Predicates         : "
			     (number-to-string predicates) "\n"
			     "Clauses            : "
			     (number-to-string clauses) "\n"
			     "Clauses/predicate  : " cl_pr " ("
			     (number-to-string cl_pr_min) "/"
			     (number-to-string cl_pr_max) ")\n"
			     "Total lines        : "
			     (number-to-string total) "\n"
			     "Lines of code      : "
			     (number-to-string code) "\n"
			     "Comment lines      : "
			     (number-to-string comments) "\n"
			     "Directives lines   : "
			     (number-to-string directives) "\n"
			     "Lines per predicate: " l_pr " (" 
			     (number-to-string l_pr_min) "/"
			     (number-to-string l_pr_max) ")\n"
			     "Lines per clause   : " l_cl " (" 
			     (number-to-string l_cl_min) "/" 
			     (number-to-string l_cl_max) ")\n"
			     "Size               : "
			     (number-to-string size) "\n")))))))

(defmacro all-metrics-min (min new)
  (list 'cond (list (list '= min -1) new)
	(list (list '< new min) new)
	(list 't min)))

(defmacro all-metrics-max (max new)
  (list 'if (list '> new max) new max))

(defun eclipse-display-metrics-all ()
  "This function displays the metrics for all ECLiPSe buffers.

See `eclipse-display-metrics' for more details."
  (interactive)
  (let* ((blist (get-all-eclipse-buffers))
	 next line metrics all-ecl-metrics all-esp-metrics
	 (predicates 0) (clauses 0) (cl_pr 0) (cl_pr_min -1) (cl_pr_max 0)
	 (total-ecl 0) (code 0) (comments-ecl 0) (directives 0) (l_pr 0)
	 (l_pr_min -1) (l_pr_max 0) (l_cl 0) (l_cl_min -1) (l_cl_max 0)
	 (size-ecl 0) (esp_segm 0) (esp_segm_code 0) (esp_segm_min -1)
	 (esp_segm_max 0) (l_esp_segm 0) (esp_directives 0) (esp_dir_code 0)
	 (esp_dir_min -1) (esp_dir_max 0) (l_esp_dir 0) (esp_expr 0)
	 (esp_expr_code 0) (esp_expr_min -1) (esp_expr_max 0) (l_esp_expr 0)
	 (html 0) (total-esp 0) (comments-esp 0) (size-esp 0)
	 (ecl-buffers nil) (esp-buffers nil) (pc 0) (nb 0) (ab (length blist)))
    (get-buffer-create "*eclipse-stats*")
    (set-buffer "*eclipse-stats*")
    (delete-region (point-min) (point-max))
    (message "Computing metrics...")
    (while (car blist)
      (setq next (car blist)
	    blist (cdr blist))
      (save-excursion
	(set-buffer next)
	(if eclipse-esp-selected
	    (setq esp-buffers t)
	  (setq ecl-buffers t))
	(setq metrics (append metrics (list (eclipse-display-metrics t)))
	      nb (1+ nb)
	      pc (eclipse-percent-message "Computing metrics" ab pc nb)))
      (goto-char (point-max))
      (insert "\n"))
    (message "Computing metrics...done")
    (while (car metrics)
      (let ((esp-mode-flag (caar metrics)) (metr (cdar metrics)))
	(setq metrics (cdr metrics))
	(cond (esp-mode-flag
	       (setq esp_segm (+ esp_segm (car metr))
		     metr (cdr metr)
		     esp_segm_code (+ esp_segm_code (car metr))
		     metr (cddr metr)
		     l_esp_segm (float-metrics esp_segm_code esp_segm)
		     esp_segm_min (all-metrics-min esp_segm_min (car metr))
		     metr (cdr metr)
		     esp_segm_max (all-metrics-max esp_segm_max (car metr))
		     metr (cdr metr)
		     esp_directives (+ esp_directives (car metr))
		     metr (cdr metr)
		     esp_dir_code (+ esp_dir_code (car metr))
		     metr (cddr metr)
		     l_esp_dir (float-metrics esp_dir_code esp_directives)
		     esp_dir_min (all-metrics-min esp_dir_min (car metr))
		     metr (cdr metr)
		     esp_dir_max (all-metrics-max esp_dir_max (car metr))
		     metr (cdr metr)
		     esp_expr (+ esp_expr (car metr))
		     metr (cdr metr)
		     esp_expr_code (+ esp_expr_code (car metr))
		     metr (cddr metr)
		     l_esp_expr (float-metrics esp_expr_code esp_expr)
		     esp_expr_min (all-metrics-min esp_expr_min (car metr))
		     metr (cdr metr)
		     esp_expr_max (all-metrics-max esp_expr_max (car metr))
		     metr (cdr metr)
		     total-esp (+ total-esp (car metr))
		     metr (cdr metr)
		     html (+ html (car metr))
		     metr (cdr metr)
		     comments-esp (+ comments-esp (car metr))
		     metr (cdr metr)
		     size-esp (+ size-esp (car metr))))
	      (t
	       (setq predicates (+ predicates (car metr))
		     metr (cdr metr)
		     clauses (+ clauses (car metr))
		     metr (cdr metr)
		     cl_pr (float-metrics clauses predicates)
		     metr (cdr metr)
		     cl_pr_min (all-metrics-min cl_pr_min (car metr))
		     metr (cdr metr)
		     cl_pr_max (all-metrics-max cl_pr_max (car metr))
		     metr (cdr metr)
		     total-ecl (+ total-ecl (car metr))
		     metr (cdr metr)
		     code (+ code (car metr))
		     metr (cdr metr)
		     comments-ecl (+ comments-ecl (car metr))
		     metr (cdr metr)
		     directives (+ directives (car metr))
		     metr (cdr metr)
		     l_pr (float-metrics code predicates)
		     metr (cdr metr)
		     l_pr_min (all-metrics-min l_pr_min (car metr))
		     metr (cdr metr)
		     l_pr_max (all-metrics-max l_pr_max (car metr))
		     metr (cdr metr)
		     l_cl (float-metrics code clauses)
		     metr (cdr metr)
		     l_cl_min (all-metrics-min l_cl_min (car metr))
		     metr (cdr metr)
		     l_cl_max (all-metrics-max l_cl_max (car metr))
		     metr (cdr metr)
		     size-ecl (+ size-ecl (car metr)))))))
    (setq all-ecl-metrics (list nil predicates clauses cl_pr
				(max 0 cl_pr_min) cl_pr_max total-ecl code
				comments-ecl directives l_pr
				(max 0 l_pr_min) l_pr_max l_cl
				(max 0 l_cl_min) l_cl_max size-ecl)
	  all-esp-metrics (list t esp_segm esp_segm_code
				l_esp_segm (max 0 esp_segm_min)
				esp_segm_max esp_directives esp_dir_code
				l_esp_dir (max 0 esp_dir_min)
				esp_dir_max esp_expr esp_expr_code
				l_esp_expr (max 0 esp_expr_min)
				esp_expr_max total-esp html comments-esp
				size-esp))
    (when ecl-buffers
      (insert-metrics "All ECL buffers:" all-ecl-metrics)
      (when esp-buffers (insert "\n")))
    (when esp-buffers
      (insert-metrics "All ESP buffers:" all-esp-metrics))
    (display-buffer "*eclipse-stats*")
    (goto-char (point-min))))

;;
;; Inferior eclipse mode
;;

(defvar inferior-eclipse-mode-map nil)

(defun inferior-eclipse-mode ()
  "Major mode for interacting with an inferior ECLiPSe process.

The following commands are available:

\\{inferior-eclipse-mode-map}

Entry to this mode calls the value of `eclipse-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`eclipse-mode-hook' is called after `comint-mode-hook'.

Commands:
Return at end of buffer sends line as input.

\\[run-eclipse] opens inferior process buffer (if not already open) and starts ECLiPSe.

\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[stop-eclipse] or \\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-quit-subjob] sends quit signal. \\[kill-eclipse] sends quit signal and closes the process
buffer.

\\[eclipse-start-tools] starts the TkTools program.

You can send text to the inferior ECLiPSe process from other buffers using
the commands \\[eclipse-compile-buffer] and \\[eclipse-compile-region].
\\[eclipse-compile-region-and-go] sends the region to the inferior process and switches to the process
buffer. Use \\[eclipse-run-region] to send text as ECLiPSe commands.

If there is a problem with entering commands in the inferior ECLiPSe process
window, disable the line
     (define-key map \"\\r\" 'eclipse-next-line)
in the definition of function `eclipse-mode-commands' in the ECLiPSe mode file
eclipse.el."
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq major-mode 'inferior-eclipse-mode
	mode-name "Inferior Eclipse"
	comint-prompt-regexp "\\[eclipse [1-9][0-9]*\\]: ")
  (eclipse-mode-variables)
  (unless inferior-eclipse-mode-map
    (setq inferior-eclipse-mode-map (copy-keymap comint-mode-map))
    (eclipse-mode-commands inferior-eclipse-mode-map))
  ;; define menu for inferior mode
  (easy-menu-define
   inferior-eclipse-process-menu inferior-eclipse-mode-map
   "ECLiPSe menu for inferior ECLiPSe process"
   '("ECLiPSe"
     ["Run ECLiPSe" run-eclipse t]
     ["Stop ECLiPSe" stop-eclipse t]
     ["Kill ECLiPSe" kill-eclipse t]
     "--"
     ["Start TkTools" eclipse-start-tools t]))
  (easy-menu-add inferior-eclipse-process-menu)
  (use-local-map inferior-eclipse-mode-map)
  (run-hooks 'eclipse-mode-hook))

;;;###autoload
(defun run-eclipse ()
  "Run an inferior ECLiPSe process, input and output via buffer *eclipse*."
  (interactive)
  (require 'comint)
  (let (eclipse-window)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*"))
      (cond ((not eclipse-window)
	     (split-window)
	     (select-window (next-window)))
	    (t (select-window eclipse-window)))
      (add-hook 'comint-preoutput-filter-functions 'eclipse-safe-output)
      (unless (> eclipse-version 0.0)
	(add-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
      (switch-to-buffer (make-comint "eclipse" eclipse-program-call))
      (inferior-eclipse-mode))))

(defun eclipse-safe-output (output)
  "Make sure that output is always added at the end of the buffer"
  (goto-char (point-max))
  output)

(defun eclipse-get-version (output)
  "Extract the version number of ECLiPSe."
  (when (string-match "#" output)
    (setq eclipse-version (string-to-number (nth 0 (split-string (nth 1 (split-string output "Version ")) " #"))))
    (remove-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
  output)

(defun stop-eclipse ()
  "Send C-c to an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*")
	    eclipse-status (process-status "eclipse"))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (t
	     (process-send-string "eclipse" "\C-c")
	     (if (not eclipse-window)
		 (switch-to-buffer "*eclipse*")
	       (select-window eclipse-window)))))))

(defun kill-eclipse ()
  "Kill an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status exists)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*")
	    eclipse-status (process-status "eclipse"))
      (unless (not eclipse-status)
	(process-send-string "eclipse" eclipse-halt-string))
      (setq exists (get-buffer "*eclipse*"))
      (when (bufferp exists)
	(kill-buffer "*eclipse*")) 
      (setq exists (get-buffer "*tktools*"))
      (when (bufferp exists)
	(kill-buffer "*tktools*"))
      (unless (not eclipse-window)
	(delete-window eclipse-window)))))

(defun eclipse-start-tools ()
  "Start TkTools for an inferior ECLiPSe process in an external Tcl/Tk window.
The socket number shown in the ECLiPSe process buffer has to be entered
manually into the input field \"Port\" in the external TkTools window."
  (interactive)
  (let (eclipse-window eclipse-status tools-status tools-buffer)
    (save-excursion
      (setq eclipse-status (process-status "eclipse")
	    tools-buffer (get-buffer "*tktools*")
	    tools-status (process-status "tktools"))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (tools-status
	     (beep)
	     (message "TkTools already running"))
	    (t
	     ;; close buffer if open from last time tools were started
	     (unless (not tools-buffer)
	       (kill-buffer tools-buffer))
	     (setq eclipse-window (get-buffer-window "*eclipse*"))
	     (if (not eclipse-window)
		 (switch-to-buffer "*eclipse*")
	       (select-window eclipse-window))
	     (cond ((or eclipse-xemacs (< eclipse-version 5.4))
		    ;; if version < 5.4 there's no automatic start-up
		    (start-process "tktools" "*tktools*" eclipse-tktools-call)
		    (insert eclipse-53-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" eclipse-53-tktools-call)
		    (message "Enter socket number in TkTools input field \"Port\" and press \"OK\""))
		   (t
		    ;; add to preoutput filters
		    (add-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
		    (insert eclipse-54-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" eclipse-54-tktools-call))))))))

(defun eclipse-run-tktools (output)
  ;; Extracts Host and Port parameters from output, starts TkTools
  ;; with host and port parameters.
  ;; This command needs ECLiPSe 5.4 or later.
  (let (host port output-list list head tail)
    (cond ((string-match "\\[.*, .*\\]\n" output)
	   (setq output-list (split-string output ": "))
	   (cond ((> (length output-list) 1)
		  (setq head (nth 0 output-list)
			tail (nth 1 output-list)))
		 (t (setq head ""
			  tail (nth 0 output-list))))
	   (setq list (split-string tail "\\(\\[\\|\\]\\|, \\)")
		 host (nth 0 list)
		 port (nth 1 list))
	   (start-process "tktools" "*tktools*" eclipse-tktools-name "--" "-h" host "-p" port)
	   ;; TkTools started, so remove this function from preoutput filters
	   (remove-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
	   head)
	  (t output))))

(defun eclipse-run-region (compile beg end)
  "Send the region to an inferior ECLiPSe process."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (let (eclipse-window)
      (save-excursion
	(process-send-region "eclipse" beg end)
	(setq eclipse-window (get-buffer-window "*eclipse*"))
	(if (not eclipse-window)
	    (switch-to-buffer "*eclipse*")
	  (select-window eclipse-window))))))

(defun eclipse-compile-buffer ()
  "Send the entire buffer to an inferior ECLiPSe process and compile it."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (process-send-string "eclipse" eclipse-compile-string)
      (process-send-string "eclipse" (buffer-string))
      (process-send-string "eclipse" "\n")		;May be unnecessary
      (if eclipse-eof-string
	  (process-send-string "eclipse" eclipse-eof-string)
	(process-send-eof "eclipse"))))) ;Send eof to eclipse process.

(defun eclipse-compile-region (compile beg end)
  "Send the region to an inferior ECLiPSe process and compile it."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (save-excursion
      (process-send-string "eclipse" eclipse-compile-string)
      (process-send-region "eclipse" beg end)
      (process-send-string "eclipse" "\n")		;May be unnecessary
      (if eclipse-eof-string
	  (process-send-string "eclipse" eclipse-eof-string)
	(process-send-eof "eclipse"))))) ;Send eof to eclipse process.

(defun eclipse-compile-region-and-go (compile beg end)
  "Send the region to an inferior ECLiPSe process, compile it, and switch to
*eclipse* buffer."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (eclipse-compile-region compile beg end)
    (let (eclipse-window)
      (setq eclipse-window (get-buffer-window "*eclipse*"))
      (if (not eclipse-window)
	  (switch-to-buffer "*eclipse*")
	(select-window eclipse-window)))))

;;
;; ECLiPSe Outline commands
;;

;; this part has been adapted from the GNU Emacs outline.el file
;; Copyright (C) 1986, 93, 94, 95, 97, 2000, 2001, 2003
;;   Free Software Foundation, Inc.

;;; Commentary:

;; This part provides outline commands inside the ECLiPSe major mode.
;; It has been adapted from the GNU Emacs outline major mode.
;; The commands are specific for editing ECLiPSe program files.

;;; Code:

(defvar eclipse-outline-regexp "[a-z]\\|:-\\|[ \t]+("
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.")

(defvar eclipse-outline-heading-end-regexp "[.]?[ \t]*\n"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.")

(defvar eclipse-outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

(defun eclipse-outline (map)
  ;; add functions for outlining to map
  (require 'outline)
  (eclipse-outline-define-map map)
  (set (make-local-variable 'line-move-ignore-invisible) nil)
  ;; Cause use of ellipses for invisible text.
  (if eclipse-xemacs
      (setq selective-display t)
    (add-to-invisibility-spec '(outline . t))))

(defun eclipse-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line."
  (save-excursion
    (looking-at eclipse-outline-regexp)
    (if (looking-at "\\([a-z]\\|[:?]-\\)")
	0
      (- (match-end 0) (match-beginning 0)))))

(defun eclipse-outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (when (re-search-forward (concat "\n\\(" eclipse-outline-regexp "\\)") nil 'move)
    (goto-char (match-beginning 0)))
  (when (and (bolp) (not (bobp)))
    (forward-char -1)))

(defun eclipse-outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (unless eclipse-esp-selected
    (when (re-search-forward (concat "\\(/\\*\\|\n\\(" eclipse-outline-regexp "\\)\\)") nil 'move)
      (progn
	(let ((aux (1+ (match-beginning 0))))
	  (goto-char (match-beginning 0))
	  (cond ((looking-at "/\\*")
		 (unless (re-search-forward "\\*/" (point-max) t)
		   (goto-char (point-max)))
		 (eclipse-outline-next-heading))
		(t (goto-char aux))))))))

(defun eclipse-outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (unless eclipse-esp-selected
    (re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move)))

(defsubst eclipse-outline-invisible-p ()
  "Non-nil if the character after point is invisible."
  (get-char-property (point) 'invisible))

(defun eclipse-outline-visible ()
  "Obsolete.  Use `eclipse-outline-invisible-p'."
  (not (eclipse-outline-invisible-p)))

(defun eclipse-outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (eclipse-outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil t)
		(error "Before first heading"))
	    (setq found (and (or invisible-ok (eclipse-outline-visible)) (point)))))
	(goto-char found)
	found)))

(defun eclipse-outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (eclipse-outline-visible))
	 (looking-at eclipse-outline-regexp))))

(defun eclipse-outline-end-of-heading ()
  ;; go to end of current heading
  (when (re-search-forward eclipse-outline-heading-end-regexp nil 'move)
    (skip-chars-backward " \t.\n")))

(defun eclipse-outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that `eclipse-outline-regexp' matches."
  (interactive "p")
  (unless eclipse-esp-selected
    (if (< arg 0)
	(beginning-of-line)
      (end-of-line))
    (while (and (not (bobp)) (< arg 0))
      (while (and (not (bobp))
		  (re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move)
		  (not (eclipse-outline-visible))))
      (setq arg (1+ arg)))
    (while (and (not (eobp)) (> arg 0))
      (while (and (not (eobp))
		  (re-search-forward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move)
		  (not (eclipse-outline-visible))))
      (setq arg (1- arg)))
    (beginning-of-line)))

(defun eclipse-outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that `outline-regexp' matches."
  (interactive "p")
  (unless eclipse-esp-selected
    (eclipse-outline-next-visible-heading (- arg))))

(defun eclipse-outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (unless eclipse-esp-selected
    (let ((beg))
      (if (eclipse-outline-on-heading-p)
	  ;; we are already looking at a heading
	  (beginning-of-line)
	;; else go back to previous heading
	(eclipse-outline-previous-visible-heading 1))
      (setq beg (point))
      (eclipse-outline-end-of-subtree)
      (push-mark (point))
      (goto-char beg))))

(defun eclipse-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((flag1 (if eclipse-xemacs
		   (if flag ?\^M ?\n)
		 flag)))
    (if eclipse-xemacs
	;; running XEmacs, so...
	(subst-char-in-region from to (if (= flag1 ?\n) ?\^M ?\n) flag1 t)
      ;; running Emacs proper, so we can use overlays
      (save-excursion
	(goto-char from)
	(end-of-line)
	(eclipse-outline-discard-overlays (point) to 'outline)
	(when flag1
	  (let ((o (make-overlay (point) to)))
	    (overlay-put o 'invisible 'outline)
	    (overlay-put o 'isearch-open-invisible 'eclipse-outline-isearch-open-invisible))))
      (run-hooks 'eclipse-outline-view-change-hook))))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `eclipse-outline-flag-region').
(defun eclipse-outline-isearch-open-invisible (overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (show-entry))

;; Exclude from the region BEG ... END all overlays
;; which have PROP as the value of the `invisible' property.
;; Exclude them by shrinking them to exclude BEG ... END,
;; or even by splitting them if necessary.
;; Overlays without such an `invisible' property are not touched.
(defun eclipse-outline-discard-overlays (beg end prop)
  (let (o)
    (when (< end beg)
      (setq beg (prog1 end (setq end beg))))
    (save-excursion
      (eclipse-dolist
       (o (overlays-in beg end))
       (when (eq (overlay-get o 'invisible) prop)
	 ;; Either push this overlay outside beg...end
	 ;; or split it to exclude beg...end
	 ;; or delete it entirely (if it is contained in beg...end).
	 (if (< (overlay-start o) beg)
	     (if (> (overlay-end o) end)
		 (progn
		   (move-overlay (eclipse-outline-copy-overlay o) (overlay-start o) beg)
		   (move-overlay o end (overlay-end o)))
	       (move-overlay o (overlay-start o) beg))
	   (if (> (overlay-end o) end)
	       (move-overlay o end (overlay-end o))
	     (delete-overlay o))))))))

;; this macro is taken from subr.el --- basic lisp subroutines for Emacs
;; Copyright (C) 1985, 86, 92, 94, 95, 99, 2000, 2001
;;   Free Software Foundation, Inc.
;;
;; defined here because it is not available in Emacs-20
(defmacro eclipse-dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
  (let ((temp (make-symbol "--dolist-temp--")))
    (list 'let (list (list temp (nth 1 spec)) (car spec))
	  (list 'while temp
		(list 'setq (car spec) (list 'car temp))
		(cons 'progn (append body (list (list 'setq temp (list 'cdr temp))))))
	  (when (cdr (cdr spec))
	    (cons 'progn (cons (list 'setq (car spec) nil) (cdr (cdr spec))))))))

(defun eclipse-outline-copy-overlay (o)
;; Make a copy of overlay O, with the same beginning, end and properties.
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o) (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (car props) (nth 1 props))
      (setq props (cdr (cdr props))))
    o1))

(defun eclipse-hide-clause ()
  "Hide the clause directly following this heading."
  (interactive)
  (unless eclipse-esp-selected
    (unless (eclipse-check-clause-begin)
      (eclipse-goto-clause-begin))
    (eclipse-outline-end-of-heading)
    (save-excursion
      (eclipse-outline-flag-region
       (point)
       (progn (eclipse-goto-clause-end) (point)) t))))

(defun eclipse-show-clause ()
  "Show the clause directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (eclipse-outline-flag-region
       (1- (point))
       (progn (eclipse-goto-clause-end) (point)) nil))))

(defun eclipse-hide-predicate ()
  "Hide the predicate directly following this heading."
  (interactive)
  (unless eclipse-esp-selected
    (unless (eclipse-check-predicate-begin)
      (eclipse-goto-predicate-begin))
    (eclipse-outline-end-of-heading)
    (save-excursion
      (eclipse-outline-flag-region
       (point)
       (progn (eclipse-goto-predicate-end) (point)) t))))

(defun eclipse-show-predicate ()
  "Show the predicate directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (unless (eclipse-check-predicate-begin)
	(eclipse-goto-predicate-begin))
      (eclipse-outline-flag-region
       (1- (point))
       (progn (eclipse-goto-predicate-end) (point)) nil))))

(defun eclipse-hide-predicates ()
  "Hide all of buffer except predicate headings."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-hide-region-body (point-min) (point-max))))

(defun eclipse-hide-clauses ()
  "Hide all of buffer except clause headings."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-hide-region-body (point-min) (point-max) t)))

(defun eclipse-hide-region-body (start end &optional flag)
  "Hide all body lines in the region, but not headings."
  ;; Nullify the hook to avoid repeated calls to `eclipse-outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-outline'
  ;; and run the hook finally.
  (unless eclipse-esp-selected
    (let ((head (point-min)) eclipse-outline-view-change-hook
	  (length (- end start)) (last 0))
      (message "Hiding...")
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (unless (eclipse-outline-on-heading-p)
	    (eclipse-goto-clause-begin))
	  (eclipse-outline-end-of-heading)
	  (while (not (eobp))
	    (eclipse-outline-flag-region
	     (point)
	     (progn (goto-char head)
		    (if flag
			(eclipse-goto-clause-end)
		      (eclipse-goto-predicate-end))
		    (point)) t)
	    (setq last (eclipse-percent-message "Hiding" length last))
	    (unless (eobp)
	      (eclipse-outline-next-heading)
	      (setq head (point))
	      (eclipse-outline-end-of-heading))))))
    (message "Hiding...done.")
    (run-hooks 'eclipse-outline-view-change-hook)))

(defun eclipse-show-predicates ()
  "Show all of buffer."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-show-all)))

(defun eclipse-show-clauses ()
  "Show all of buffer."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-show-all)))

(defun eclipse-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-region (point-min) (point-max) nil)))

(defun eclipse-hide-block ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-subtree t)))

(defun eclipse-show-block ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-subtree nil)))

(defun eclipse-outline-flag-subtree (flag)
  ;; flag subtree
  (save-excursion
    (eclipse-outline-back-to-heading)
    (eclipse-outline-end-of-heading)
    (eclipse-outline-flag-region
     (point)
     (progn (eclipse-outline-end-of-subtree) (point))
     flag)))

(defun eclipse-outline-end-of-subtree ()
  ;; go to end of subtree
  (eclipse-outline-back-to-heading)
  (if (looking-at "[ \t]+(")
      (eclipse-outline-end-of-block)
    (let ((first t)
	  (level (eclipse-outline-level)))
      (while (and (not (eobp))
		  (or first (> (eclipse-outline-level) level)))
	(setq first nil)
	(eclipse-outline-next-heading))
      (when (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (when (bolp)
	    ;; leave blank line before heading
	    (forward-char -1)))))))

(defun eclipse-outline-end-of-block ()
  ; we are looking at "[ \t]+("
  (let (level)
    (skip-chars-forward " \t")
    (forward-char)
    (setq level 1)
    (while (not (or (eobp) (= level 0)))
      (cond ((looking-at ")")
	     (setq level (- level 1))
	     (forward-char))
	    ((looking-at "(")
	     (setq level (+ level 1))
	     (forward-char))
	    ((looking-at "'")
	     (eclipse-goto-end-of-quote))
	    ((looking-at "\"")
	     (eclipse-goto-end-of-string))
	    (t (forward-char))))
    (forward-char)))

(defun eclipse-outline-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (eclipse-outline-back-to-heading t)
  (when (eq (eclipse-outline-level) 1)
    (error "Already at top level of the outline"))
  (while (and (> (eclipse-outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (eclipse-outline-level)))
      (while (and (not (< (eclipse-outline-level) present-level))
		  (not (bobp)))
	(eclipse-outline-previous-heading))
      (setq arg (- arg 1)))))

(defun eclipse-outline-up-heading (arg)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (unless eclipse-esp-selected
    (eclipse-outline-back-to-heading)
    (when (eq (eclipse-outline-level) 1)
      (error "Already at top level of the outline"))
    (while (and (> (eclipse-outline-level) 1)
		(> arg 0)
		(not (bobp)))
      (let ((present-level (eclipse-outline-level)))
	(while (and (not (< (eclipse-outline-level) present-level))
		    (not (bobp)))
	  (eclipse-outline-previous-visible-heading 1))
	(setq arg (- arg 1))))))

(defmacro eclipse-outline-same-level (arg func str)
  (list 'unless 'eclipse-esp-selected
	(list 'eclipse-outline-back-to-heading)
	(list 'while (list '> arg 0)
	      (list 'let (list (list 'point-to-move-to
				     (list 'save-excursion (list func))))
		    (list 'if 'point-to-move-to
			  (list 'progn
				(list 'goto-char 'point-to-move-to)
				(list 'setq arg (list '1- arg)))
			  (list 'progn
				(list 'setq arg 0)
				(list 'error str)))))))
  
(defun eclipse-outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (eclipse-outline-same-level arg eclipse-outline-get-next-sibling
			      "No following same-level heading"))

(defun eclipse-outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (eclipse-outline-same-level arg eclipse-outline-get-last-sibling
			      "No previous same-level heading"))

(defmacro eclipse-outline-get-sibling (heading-func border-func)
  (list 'let (list (list 'level (list 'eclipse-outline-level)))
    (list heading-func 1)
    (list 'while (list 'and (list '> (list 'eclipse-outline-level) 'level)
		       (list 'not (list border-func)))
      (list heading-func 1))
    (list 'if (list '< (list 'eclipse-outline-level) 'level)
	  'nil
	  (list 'point))))

(defun eclipse-outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (eclipse-outline-get-sibling eclipse-outline-next-visible-heading eobp))

(defun eclipse-outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point or nil if none."
  (eclipse-outline-get-sibling eclipse-outline-previous-visible-heading bobp))

(defun eclipse-outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (unless eclipse-esp-selected
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(let ((buffer (current-buffer))
	      start end)
	  (with-temp-buffer
	    (with-current-buffer buffer
	      ;; Boundary condition: starting on heading:
	      (when (eclipse-outline-on-heading-p)
		(eclipse-outline-back-to-heading)
		(setq start (point)
		      end (progn (eclipse-outline-end-of-heading) (point)))
		(insert-buffer-substring buffer start end)
		(insert "\n\n")))
	    (let ((temp-buffer (current-buffer)))
	      (with-current-buffer buffer
		(while (eclipse-outline-next-heading)
		  (when (eclipse-outline-visible)
		    (setq start (point)
			  end (progn (eclipse-outline-end-of-heading) (point)))
		    (with-current-buffer temp-buffer
		      (insert-buffer-substring buffer start end)
		      (insert "\n\n"))))))
	    (kill-new (buffer-string))))))))

;;
;; Compile mode support & syntax checking
;;

;; well, support is a bit euphemistic...
;; as usual, the functions supplied by the compile mode were
;; insufficient to deal with ECLiPSe, so I had to do most myself
;; The following functions may contain traces of compile.el, which are
;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 1999, 2001 Free Software Foundation, Inc.

(defconst eclipse-compile-event-handlers
"my_event_handler(Nr, Where, Module) :-
        error_id(Nr, Msg),
        (
            compiled_stream(Stream)
        ->
            get_stream_info(Stream, line, Line1),
            get_stream_info(Stream, offset, Offset1),
            get_stream_info(Stream, name, Name),
            (
                Name == user
            ->
                Offset is Offset1 - 2343,
                Line is Line1 - 73
            ;
                Offset is Offset1 - 1,
                Line is Line1 - 1
            ),
            printf(\"*** %w ** %w ** %w ** %w ** %w%n%b\",
                   [Msg, Where, Name, Line, Offset])
        ;
            printf(\"*** %w ** %w in module %w%n%b\", [Msg, Where, Module])
        ).

my_compile_start_event_handler(_Nr, Where) :-
        Msg = \"Start of compilation\",
        printf(\">>> %w ** %w%n%b\", [Msg, Where]).

my_compile_end_event_handler(_Nr, Where) :-
        Msg = \"Compilation ended\",
        (
            Where = (term, _, _)
        ->
            true
        ;
            Where = (Name, Size, Time),
            printf(\"<<< %w ** %w ** %w bytes ** %w seconds%n%b\",
                   [Msg, Name, Size, Time])
        ).

:- (
       for(Nr, 1, 333)
   do
       (
           Nr == 146
       ->
           set_event_handler(Nr, my_compile_start_event_handler/2)
       ;
           Nr == 139
       ->
           set_event_handler(Nr, my_compile_end_event_handler/2)
       ;
           current_error(Nr),
           get_event_handler(Nr, Handler, _),
           memberchk(Handler,
                     [parser_error_handler/2, error_handler/2,
                      error_handler/3, error_handler/4, warning_handler/3,
                      singleton_in_loop/2, compiler_error_handler/2,
                      call_handler/4, redef_other_file_handler/2,
                      undef_array_handler/3, undef_record_handler/2,
                      dynamic_handler/3, undef_dynamic_handler/3,
                      declaration_warning_handler/3,
                      locked_access_handler/2, no_lookup_module_handler/4,
                      ambiguous_import_resolve/3, ambiguous_import_warn/3,
                      macro_handler/3])
       ->
           set_event_handler(Nr, my_event_handler/3)
       ;
           true
       )
   ),
   set_stream(warning_output, output)."
"ECLiPSe predicates to reformat compile time warning and error messages.
These predicates are used so that compiling ECLiPSe code will result in
standardised output that can be easily parsed by the Emacs compile mode
functions.")

(defun eclipse-parse-errors (&optional arg1 arg2) ; ignore arguments, just reparse
  ;; parses compile errors and warnings
  (setq compilation-error-list nil)
  (let ((compilation-num-errors-found 0)
	(compilation-current-file nil)
	(compilation-current-buffer nil)
	(compilation-current-offset nil)
	start aux marker line col clause predicate error)
    ;; Parse messages.
    (goto-char (point-min))
    (while (not (eobp))
      ;; search beginning of next message
      (re-search-forward "\\(\\(\\*\\*\\* \\)?WARNING: Unrecognized structure\\|\\*\\*\\*\\|[>][>][>]\\|[<][<][<]\\)"
			 (point-max) t)
      (forward-char -3)
      (cond ((looking-at "\\*\\*\\*") ; error or warning
	     (setq marker (point-marker))
	     (forward-char 4)
	     (cond ((looking-at "Warning: Singleton variables in clause") ; singleton variables
		    (search-forward "clause ")
		    (setq start (point))
		    (search-forward " ")
		    (setq clause (string-to-number (buffer-substring start (point)))) ; get clause number
		    (search-forward "of ")
		    (setq start (point))
		    (search-forward ":")
		    ;; find appropriate clause to point to
		    (setq predicate (buffer-substring start (1- (point)))
			  aux (eclipse-compile-find-clause (car compilation-current-buffer) predicate clause)
			  error (cons marker aux)))
		   (t ; other error or warning
		    (setq start (point))
		    (end-of-line)
		    (setq aux (split-string (buffer-substring start (point)) " \\*\\* "))
		    (cond ((= (length aux) 2) ; short message: nothing to point to
			   (setq error nil))
			  (t ; find appropriate point in buffer to point to
			   (setq aux (string-to-number (car (cddr (cddr aux)))))
			   (save-excursion
			     (set-buffer (car compilation-current-buffer))
			     (goto-char (+ aux (car compilation-current-offset)))
			     (setq error (cons marker (point-marker))))))))
	     (unless (not error)
	       (setq compilation-num-errors-found (1+ compilation-num-errors-found)
		     compilation-error-list (cons error compilation-error-list)))
	     (forward-line))
	    ((looking-at "[>][>][>]") ; new file opened
	     (search-forward "**")
	     (forward-char)
	     (setq start (point))
	     (end-of-line)
	     (setq filename (buffer-substring start (point)))
	     (cond ((string-equal filename "user")
		    ;; top level: get filename of eclipse-compile-buffer, put info on stack
		    (setq compilation-current-file (cons (buffer-file-name (get-buffer eclipse-compile-buffer)) compilation-current-file)
			  compilation-current-buffer (cons eclipse-compile-buffer compilation-current-buffer)
			  compilation-current-offset (cons eclipse-compile-offset compilation-current-offset)))
		   (t ; else get filename and open file if necessary, put info on stack
		    (setq compilation-current-file (cons filename compilation-current-file)
			  compilation-current-buffer (cons (buffer-name (find-file-noselect filename t)) compilation-current-buffer)
			  compilation-current-offset (cons 0 compilation-current-offset))))
	     (forward-line))
	    ((looking-at "[<][<][<]") ; file finished: pop stack
	     (setq compilation-current-file (cdr compilation-current-file)
		   compilation-current-buffer (cdr compilation-current-buffer)
		   compilation-current-offset (cdr compilation-current-offset))
	     (forward-line))
	    ((looking-at "ure") ; unrecognized structure
	     (beginning-of-line)
	     (setq marker (point-marker))
	     (unless (looking-at "\\*") ; make it look like other messages
	       (insert "*** "))
	     (search-forward "in ")
	     (cond ((looking-at "module") ; new style
		    (if (looking-at "module \\([^\n]\\)+ in ")
			(search-forward "in ")
		      (end-of-line)
		      (delete-char 1)
		      (insert " "))
		    (setq start (point))
		    (re-search-forward "\\({\\| with\\)")
		    (backward-char)
		    (if (looking-at "{")
			(forward-char)
		      (backward-char 3)))
		   (t  ; old style
		    (forward-char)
		    (setq start (point))
		    (search-forward " ")))
	     (backward-char)
	     (setq aux (buffer-substring start (point)))
	     (save-excursion ; find appropriate point in buffer to point to
	       (set-buffer (car compilation-current-buffer))
	       (goto-char (point-min))
	       (re-search-forward (concat aux "\\({\\|[ \t]+with\\)") (point-max) t)
	       (goto-char (match-beginning 0))
	       (setq aux (point-marker)))
	     (setq error (cons marker aux)
		   compilation-num-errors-found (1+ compilation-num-errors-found)
		   compilation-error-list (cons error compilation-error-list))
	     (forward-line))
	    (t (goto-char (point-max)))) ; else do nothing
      (message
       "Parsing error messages...%d found. %.0f%% of buffer seen."
       compilation-num-errors-found
       (/ (* 100.0 (point)) (point-max))))
    (set-marker compilation-parsing-end (point))
    (setq compilation-error-list (nreverse compilation-error-list))
    (when eclipse-compile-flag
      (setq compilation-old-error-list compilation-error-list
	    eclipse-compile-flag nil))
    ;; make errors clickable
    (let ((inhibit-read-only t)
	  (buffer-undo-list t)
	  deactivate-mark
	  (error-list compilation-error-list))
      (while error-list
	(save-excursion
	  (add-text-properties (goto-char (caar error-list))
			       (progn (end-of-line) (point))
			       '(mouse-face highlight help-echo "\
mouse-2: visit this file and line")))
	(setq error-list (cdr error-list))))))

(defun eclipse-compile-find-clause (buffer predicate clause)
  ;; find the appropriate clause of predicate
  (save-excursion
    (let ((i 1))
      (set-buffer buffer)
      (goto-char (point-min))
      (eclipse-goto-clause-begin)
      (unless (string-equal predicate (eclipse-get-current-predicate-template t))
 	(while (not (string-equal predicate (eclipse-get-current-predicate-template t)))
 	  (eclipse-goto-predicate-end)
	  (eclipse-jump-over-strings t)))
      (while (< i clause)
	(eclipse-goto-clause-end)
	(eclipse-jump-over-strings t)
 	(setq i (1+ i)))
      (point-marker))))

(defun eclipse-call-compile (code)
  "Compiles ECLiPSe code using the Emacs compile mode."
  (interactive)
  (require 'compile)
  (require 'comint)
  (let (start end messages keywords (buf (buffer-name)))
    (save-excursion
      (message "Start compiling...")
      ;; make compilation buffer
      (set-buffer (make-comint "eclipse-compile" eclipse-program-call))
      ;; redefine event handlers first!
      (process-send-string "eclipse-compile" eclipse-compile-string)
      (process-send-string "eclipse-compile" eclipse-compile-event-handlers)
      (process-send-string "eclipse-compile" "\n")
      (if eclipse-eof-string
	  (process-send-string "eclipse-compile" eclipse-eof-string)
	(process-send-eof "eclipse-compile")) ;Send eof to eclipse process.
      ;; now send code to eclipse
      (process-send-string "eclipse-compile" eclipse-compile-string)
      (process-send-string "eclipse-compile" code)
      (process-send-string "eclipse-compile" "\n")
      (if eclipse-eof-string
	  (process-send-string "eclipse-compile" eclipse-eof-string)
	(process-send-eof "eclipse-compile")) ;Send eof to eclipse process.
      ;; send halt command to eclipse, then wait until eclipse is done compiling
      (process-send-string "eclipse-compile" eclipse-halt-string)
      (while (eq (process-status "eclipse-compile") 'run)
	(sleep-for 0.1))
      ;; get the compilation output
      (search-backward "[eclipse 2]: ")
      (forward-char 13)
      (setq start (point))
      (search-forward "[eclipse")
      (forward-line -2)
      (end-of-line)
      (setq end (point)
	    messages (buffer-substring start end))
      ;; kill the compilation buffer
      (when (bufferp (get-buffer "*eclipse-compile*"))
	(kill-buffer "*eclipse-compile*"))
      ;; split the output in single lines, delete leading whitespace
      (let ((auxmsg (split-string messages "\n")) aux msg)
	(setq messages nil)
	(while (car auxmsg)
	  (setq aux (string-to-list (car auxmsg))
		auxmsg (cdr auxmsg)
		msg nil)
	  (while (char-equal (car aux) 32)
	    (setq aux (cdr aux)))
	  (while (car aux)
	    (setq msg (concat msg (char-to-string (car aux)))
		  aux (cdr aux)))
	  (setq messages (concat messages "\n" msg))))
      (message "Compiling done")
      (let (eclipse-compile-window)
	;; switch to compile (output) buffer
	(setq eclipse-compile-window (get-buffer-window "*eclipse-compilation*"))
	(cond ((not eclipse-compile-window)
	       (split-window)
	       (select-window (next-window)))
	      (t (select-window eclipse-compile-window)))
	(switch-to-buffer (get-buffer-create "*eclipse-compilation*"))
	;; delete any content (in case such a buffer already existed)
	(delete-region (point-min) (point-max))
	;; switch to compilation mode, and set the variables
	(compilation-mode)
	(setq compilation-auto-highlight t
	      compilation-error-screen-columns nil
	      compilation-read-command nil
	      compilation-parse-errors-function 'eclipse-parse-errors
	      compile-command nil)
	(set (make-local-variable 'eclipse-compile-flag) t)
	(set (make-local-variable 'eclipse-compile-buffer) buf)
	(set (make-local-variable 'eclipse-compile-offset) 0)
	(set (make-local-variable 'compilation-eclipse-font-lock-keywords)
	     (list
	      '("\\*\\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([0-9]+\\) \\*\\* \\([0-9]+\\)" (1 font-lock-warning-face) (2 font-lock-variable-name-face) (3 font-lock-type-face) (4 font-lock-function-name-face) (5 font-lock-function-name-face))
	      '("\\*\\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) in module \\(.+\\)" (1 font-lock-warning-face) (2 font-lock-function-name-face) (3 font-lock-type-face))
	      '("\\*\\*\\* \\(Warning: Singleton variables\\) in clause \\([0-9]+\\) of \\(.+/[0-9]+\\): \\([A-Z][a-zA-Z0-9]*\\)\\(, \\([A-Z][a-zA-Z0-9]*\\)\\)+" (1 font-lock-warning-face) (2 font-lock-type-face) (3 font-lock-function-name-face) (4 font-lock-variable-name-face) (6 font-lock-variable-name-face))
	      '("\\*\\*\\* \\(Warning: Singleton variables\\) in clause \\([0-9]+\\) of \\(.+/[0-9]+\\): \\([A-Z][a-zA-Z0-9]*\\)" (1 font-lock-warning-face) (2 font-lock-type-face) (3 font-lock-function-name-face) (4 font-lock-variable-name-face))
	     '("\\\(WARNING: Unrecognized structure name\\) \\'\\(.+\\) with [[].+[]]\\' in module \\(.+\\)" (1 font-lock-warning-face) (2 font-lock-function-name-face) (3 font-lock-type-face))
	     '("\\(WARNING: Unrecognized structure name\\) in module \\(.+\\) in \\(.+\\)\\({.*}\\| with\\)" (1 font-lock-warning-face) (2 font-lock-type-face) (3 font-lock-function-name-face))))
	(setq font-lock-defaults '(compilation-eclipse-font-lock-keywords t))
	;; insert the compilation output and parse it
	(insert messages)
	(eclipse-parse-errors)))))

(defun eclipse-check-buffer ()
  "Send the entire buffer to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (setq eclipse-compile-offset 0)
      (eclipse-call-compile (buffer-substring (point-min) (point-max))))))

(defun eclipse-check-region ()
  "Send the current region to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (setq eclipse-compile-offset (region-beginning))
      (eclipse-call-compile (buffer-substring (region-beginning) (region-end))))))

(defun eclipse-check-predicate ()
  "Send the current predicate to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (let ((point1 (prog2
			(eclipse-goto-predicate-begin)
			(point)))
	    (point2 (prog2
			(eclipse-goto-predicate-end)
			(point))))
	(setq eclipse-compile-offset point1)
	(eclipse-call-compile (buffer-substring point1 point2))))))

(defun eclipse-check-clause ()
  "Send the current clause to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (let ((point1 (prog2
			(eclipse-goto-clause-begin)
			(point)))
	    (point2 (prog2
			(eclipse-goto-clause-end)
			(point))))
	(setq eclipse-compile-offset point1)
	(eclipse-call-compile (buffer-substring point1 point2))))))

(provide 'eclipse)

;;; eclipse.el ends here
