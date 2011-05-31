;******************************************************
;***                 Server mode                    ***
;******************************************************

(server-start)

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defun macosx-p ()
  (string-match "darwin" (symbol-name system-type)))
(defun linux-p ()
  (string-match "gnu/linux" (symbol-name system-type)))
(defun win-p ()
  (string-match "windows-nt" (symbol-name system-type)))




;; ;;TIPS: C-h k visualizza il comando triggerato da uno shortcut o menu.
;; ;;MAC-OS SPECIFIC SETTINGS
;; (if macosx-p
;;   (progn
;;     (tool-bar-mode 1)
;;     (setq inhibit-splash-screen t) ;Toglie lo splashscreen
;;     (setq visible-bell t) ;Mai piu BEEP
;;     (setq mac-option-modifier nil)
;;     (setq mac-option-key-is-meta nil)
;;     (setq mac-command-key-is-meta t)
;;     (setq mac-command-modifier 'meta)
;;     (global-set-key (kbd "M-w") 'kill-buffer-no-questions)
;;     (global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
;;     (global-set-key (kbd "M-x") 'clipboard-kill-region) ;CUT
;;     (global-set-key (kbd "M-c") 'clipboard-kill-ring-save) ;COPY
;;     (global-set-key (kbd "M-v") 'clipboard-yank) ;PASTE
;;     (global-set-key (kbd "M-z") 'undo) ;UNDO
;;     (global-set-key (kbd "M-y") 'repeat) ;REPEAT
;;     (global-set-key (kbd "M-o") 'find-file) ;OPEN
;;     (global-set-key (kbd "M-s") 'save-buffer) ;SAVE
;;     (global-set-key (kbd "<f1>") 'execute-extended-command) ;;M-x
;;     (global-set-key (kbd "M-a") 'mark-whole-buffer)
;;     (global-set-key (kbd "<f2>") 'ns-toggle-fullscreen)))

;; ;LINUX SPECIFIC SETTINGS
;; (if linux-p
;;   (progn
;;     (tool-bar-mode 1)
;;     (setq inhibit-splash-screen t) ;Toglie lo splashscreen
;;     (setq visible-bell t) ;Mai piu BEEP
;;     (global-set-key (kbd "C-w") 'kill-buffer-no-questions)
;;     (global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
;;     (global-set-key (kbd "C-x") 'clipboard-kill-region) ;CUT
;;     (global-set-key (kbd "C-c") 'clipboard-kill-ring-save) ;COPY
;;     (global-set-key (kbd "C-v") 'clipboard-yank) ;PASTE
;;     (global-set-key (kbd "C-z") 'undo) ;UNDO
;;     (global-set-key (kbd "C-y") 'repeat) ;REPEAT
;;     (global-set-key (kbd "C-o") 'find-file) ;OPEN
;;     (global-set-key (kbd "C-s") 'save-buffer) ;SAVE
;;     (global-set-key (kbd "<f1>") 'execute-extended-command) ;;M-x
;;     (global-set-key (kbd "C-a") 'mark-whole-buffer) ;SELECT ALL
;;     (global-set-key (kbd "<f2>") 'my-toggle-fullscreen))) ;FULLSCREEN


; ** ERGO EMACS
; Optional
; ; (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; for Dvorak keyboard
;
; ; Optional. Swap “Ctrl+x” and “Ctrl+t”, so it's easier to type on Dvorak layout
; ; (keyboard-translate ?\C-t ?\C-x) (keyboard-translate ?\C-x ?\C-t)
;
; ; Options for Mac.
; ; (setq mac-command-modifier 'meta)
; ; (setq mac-option-modifier 'hyper)
;
;; (load-file "~/.elisp/ergoemacs_1_7_1/site-lisp/site-start.el")


;;;; FROM CHARLES

;;Per dire a Emacs di ricordare gli aperti di recente
(recentf-mode 1)








;******************************************************
;***       Misc graphic configurations stuff        ***
;******************************************************
(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-insert-mode t
      column-number-mode t
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-mode t
      indent-tabs-mode nil
      partial-completion-mode t
      scroll-conservatively 20000000
      scroll-margin 1
      scroll-step 1
      show-paren-mode t
      tab-always-indent t
      tab-width 4
      text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))
      transient-mark-mode t)

(mouse-wheel-mode t)
(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving
(setq show-trailing-whitespace t)    ; Trailing white space, yes!
(setq backup-inhibited t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;iswitchb makes buffer switching more cool
(iswitchb-mode)
;Reload .emacs on the fly
(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs")
  (message ".emacs reloaded successfully"))

;;Place all backup copies of files in a common location
(defconst use-backup-dir t)   
(setq backup-directory-alist (quote ((".*" . "~/emacs-meta/backups/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.


(setq-default indent-tabs-mode nil)


; Colors
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
      "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;Make the prompt read only
(setq comint-prompt-read-only t)


;******************************************************
;***       Some Key-binding personalizations        ***
;******************************************************

; (global-set-key (kbd "C-z") 'kill-region)
; (global-set-key (kbd "C-x S-3") 'comment-region)
; (global-set-key (kbd "M-/") 'comment-region)
; (global-set-key (kbd "S-M-/") 'uncomment-region)

(global-set-key [C-A-right] 'next-buffer)
(global-set-key [C-A-left] 'previous-buffer)














;******************************************************
;***                Fullscreen mode                 ***
;******************************************************
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil
                                                            'fullscreen) nil
                                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)









;******************************************************
;***          Encodings and lang mode               ***
;******************************************************

;(set-face-background 'hl-line "#930")
(set-language-environment "English")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;; (set-file-name-coding-system 'utf-8m) ; already set
(prefer-coding-system 'utf-8-unix)







;******************************************************
;***                C++ mode stuff                  ***
;******************************************************
;; This stuff is here since its default everywhere
;; and having a usable .emacs is useful


(setq c-default-style "stroustrup")
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(setq auto-mode-alist
      (append
       '(("\\.h" . c++-mode))
       auto-mode-alist))








;******************************************************
;***          Python default stuff                  ***
;******************************************************
;(define-key inferior-python-mode-map "\C-c\t" 'python-complete-symbol)







;******************************************************
;***     Adding to path external libraries          ***
;******************************************************


(setq load-path (cons  "~/.emacs.d/" load-path))

(when (macosx-p)
  (load-library "apple-cus"))
(load-library "more-libraries")


;******************************************************
;***               Color and faces                  ***
;******************************************************

(defun my-color ()
  (load-library "color-theme")
  (color-theme-initialize)
  (color-theme-classic)
  )

(defun my-gui-preferences ()
  (my-color)
  (hl-line-mode)
  (if (boundp 'riko-preferred-font)
      (set-face-font
       'default
       riko-preferred-font)))

(if window-system
    (my-gui-preferences))





;; TODO: Emacs 23 has this integrated in whitespace-mode.
;; Add lines and lines-tail to whitespace-style, and 
;; customize whitespace-line-column

;; Mi avverte se supero gli 80 caratteri
;; (require 'highlight-80+)
;; (add-hook 'emacs-startup-hook (lambda () (highlight-80+-mode))) ;;Non so se e' necessario ma prima non funzionava senza hook





;******************************************************
;***     Custom set variables                       ***
;******************************************************
;; Custom variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex --synctex=1")
 '(TeX-shell "/bin/zsh")
 '(column-number-mode t)
 '(display-time-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(mac-pass-command-to-system nil)
 '(mouse-wheel-progressive-speed nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(pc-selection-mode nil nil (pc-select))
 '(save-place nil nil (saveplace))
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar)))



(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )



