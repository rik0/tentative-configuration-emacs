;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;Dice ad Emacs dove cercare i plugin
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-nav/")

;;Utilizzo nav, navigatore di directory
(require 'nav)
(nav)

;;Supporto per scheme
(require 'quack)

      
;; Kill default buffer without the extra emacs questions 
(defun kill-buffer-no-questions ()   
   (interactive) 
   (kill-buffer (buffer-name)))



;;SYSTEM DEPENDENT CONFIGURATIONS
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))

(defun macosx-lisp-mode-hook ()
  (local-set-key (kbd "C-c C-r") 'slime-eval-region))

;;TIPS: C-h k visualizza il comando triggerato da uno shortcut o menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MAC-OS SPECIFIC SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fixpath) ;;setta variabili d'ambiente su MAC-OS
(if macosx-p
  (progn
    (if window-system (tool-bar-mode 1)) ;Se ho la GUI abilito la toolbar
    (setq inhibit-splash-screen t) ;Toglie lo splashscreen
    (setq visible-bell t) ;Mai piu BEEP
    (setq mac-option-modifier nil)
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (global-set-key (kbd "M-w") 'kill-buffer-no-questions)
    (global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
    (global-set-key (kbd "M-x") 'clipboard-kill-region) ;CUT
    (global-set-key (kbd "M-c") 'clipboard-kill-ring-save) ;COPY
    (global-set-key (kbd "M-v") 'clipboard-yank) ;PASTE
    (global-set-key (kbd "M-z") 'undo) ;UNDO
    (global-set-key (kbd "M-y") 'repeat) ;REPEAT
    (global-set-key (kbd "M-o") 'find-file) ;OPEN
    (global-set-key (kbd "M-s") 'save-buffer) ;SAVE                                      
    (global-set-key (kbd "<f1>") 'execute-extended-command) ;;M-x
    (global-set-key (kbd "M-a") 'mark-whole-buffer)                                                             
    (global-set-key (kbd "<f2>") 'ns-toggle-fullscreen)
    (add-hook 'lisp-mode-hook 'macosx-lisp-mode-hook)
    (setq shell-file-name "/bin/zsh")
    (add-hook 'emacs-startup-hook 'mac-read-environment-vars-from-shell)))

;LINUX SPECIFIC SETTINGS
(if linux-p
  (progn
    (tool-bar-mode 1)
    (setq inhibit-splash-screen t) ;Toglie lo splashscreen
    (setq visible-bell t) ;Mai piu BEEP
    (global-set-key (kbd "C-w") 'kill-buffer-no-questions)
    (global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
    (global-set-key (kbd "C-x") 'clipboard-kill-region) ;CUT
    (global-set-key (kbd "C-c") 'clipboard-kill-ring-save) ;COPY
    (global-set-key (kbd "C-v") 'clipboard-yank) ;PASTE
    (global-set-key (kbd "C-z") 'undo) ;UNDO
    (global-set-key (kbd "C-y") 'repeat) ;REPEAT
    (global-set-key (kbd "C-o") 'find-file) ;OPEN
    (global-set-key (kbd "C-s") 'save-buffer) ;SAVE                                      
    (global-set-key (kbd "<f1>") 'execute-extended-command) ;;M-x
    (global-set-key (kbd "C-a") 'mark-whole-buffer) ;SELECT ALL                                                          
    (global-set-key (kbd "<f2>") 'my-toggle-fullscreen))) 
	
;;Toggle fullscreen LINUX
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))

;;Per dire a Emacs di ricordare gli aperti di recente
(recentf-mode 1)

;;Linum mostra i numeri di riga
(require 'linum+)

;;Mi permette di avere le tab per ogni file aperto.
(require 'tabbar)
(tabbar-mode)

(global-set-key (kbd "C-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<left>") 'tabbar-backward-tab)

;; add a buffer modification state indicator in the tab label,
;; and place a space around the label to make it looks less crowd
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
		(if (and (buffer-modified-p (tabbar-tab-value tab))
				 (buffer-file-name (tabbar-tab-value tab)))
			(concat " + " (concat ad-return-value " "))
			(concat " " (concat ad-return-value " ")))))
			
;; called each time the modification state of the buffer changed
(defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))
   
;; first-change-hook is called BEFORE the change is made
(defun ztl-on-buffer-modification ()
   (set-buffer-modified-p t)
   (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
;; this doesn't work for revert, I don't know
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

;;Separo per gruppi le varie tab
(defvar *emacs-notifications* '("*Messages*" "*Completions*"))
(defvar *navigations* '("*nav*"))
(defvar *slime-stuff* '("*inferior-lisp*" "*slime-events*" "*slime-repl sbcl*" "*sldb sbcl/1*"))

(setq tabbar-buffer-groups-function
  (lambda ()
    (cond ((member (buffer-name) *slime-stuff*) (list "#slime"))
          ((member (buffer-name) *emacs-notifications*) (list "#emacs-notifications"))
          ((member (buffer-name) *navigations*) (list (buffer-name)))
          (t (list "All")))))

;(setq tabbar-buffer-groups-function (lambda () (list "All")))

;;Disabilita il backup automatico dei file
(setq backup-inhibited t)

;;Fa partire slime
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/elpa/slime-20100404")  ; your SLIME directory
(require 'slime)
(slime-setup)

;;Fa splittare la finestra quando parte slime
(add-hook 'slime-repl-mode-hook 'split-window-vertically)

;;Rainbow-mode per parentesi
(setq hl-paren-colors
      '("red"
        "orange" "yellow" "greenyellow" "green"
        "springgreen" "cyan" "slateblue" "magenta" "purple"))

(add-hook 'lisp-mode-hook 
	  (lambda () 
	    (highlight-parentheses-mode t)))

;; Mi avverte se supero gli 80 caratteri
(require 'highlight-80+)
(add-hook 'lisp-mode-hook (lambda () (highlight-80+-mode t)))

;; Serve per indentare bene con C e simili
(add-hook 'c-mode-common-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'lisp-mode-hook '(lambda ()
			(local-set-key (kbd "RET") 'newline-and-indent)))


;; Serve per avere l'evidenziazione sintassi con SLIME
;;(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; Mi fa vedere il numero di riga a sx
(global-linum-mode 1)

;; Gestione dei colori, do-re-mi mi permette di ciclare
;; fra i themes per selezionare quello che mi piace

(load-file "~/.emacs.d/themes/color-theme-sunburst.el")
(load-file "~/.emacs.d/themes/color-theme-tango.el")
(load-file "~/.emacs.d/themes/gentooish.el")
(color-theme-tm)

;; Serve per intentare con tutti spazi in python
(setq python-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))

;; Serve per intentare con tutti spazi in HASKELL
(setq haskell-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))


;; Serve per yasnippet
(add-to-list 'load-path
                  "~/.emacs.d/plugins/yasnippet-0.6.1c")
    (require 'yasnippet) ;; not yasnippet-bundle
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; Serve per haskell mode, caratteri sagaci
(setq haskell-font-lock-symbols 'unicode)

;; Serve per AutoComplete.el
(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-modes 'lisp-mode)
(ac-config-default)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
