;; File: .emacs
;; Creation: 2013
;; Author: Samuel El-Borai aka dgellow <samuel.elborai@gmail.com>
;; Website: https://github.com/dgellow/home-bootstrapping
;; Description: My personal emacs configuration



;;                   Table of contents
;;                   0. Meta
;;                   1. Editor
;;                   1.1 Theme
;;                   1.2 Navigation
;;                   1.3 Fullscreen mode
;;                   2. Programming
;;                   2.1 Lisp
;;                   2.2 Ruby
;;                   2.3 Python
;;                   2.4 Clojure
;;                   2.5 Web
;;                   3. Custom sets
;;                   Table of contents


;; To generate the table of contents : `cat .emacs | grep "^;;   " | sort -u`



;;————————————————————————————————————————————————————————————————————
;;                   0. Meta

(require 'cl)

;; Package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)


;;————————————————————————————————————————————————————————————————————
;;                   1. Editor

;; Backup files
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;; Save minibuffer history
(savehist-mode 1)

;; Scroll up & down without moving the cursor
(global-set-key [(meta p)] 'scroll-up-line)
(global-set-key [(meta n)] 'scroll-down-line)

;; Move cursor to the top & bottom of the current window body
(global-set-key [(meta P)] '(lambda ()(interactive)(goto-char (window-start))))
(global-set-key [(meta N)] '(lambda ()(interactive)(goto-char (- (window-end)1))))

;; Smooth scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(progn
	(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
	(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
	(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
	(setq scroll-step 1)) ;; keyboard scroll one line at a time

;; MultiTerm mode
(require 'multi-term)


;;--------------------------------------------------------------------
;;                   1.1 Theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq cursor-type '(bar . 1))

(require 'powerline)
(powerline-default-theme)

(require 'rainbow-delimiters)


;;--------------------------------------------------------------------
;;                   1.2 Navigation

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)


;;--------------------------------------------------------------------
;;                   1.3 Fullscreen mode

(defun toggle-minimal-mode (fs)
  (interactive "P")
  (defun fullscreen-margins nil
    (if (and (window-full-width-p) (not (minibufferp)))
	(set-window-margins nil (/ (- (frame-width) 120) 2) (/ (- (frame-width) 120) 2))
      (mapcar (lambda (window) (set-window-margins window nil nil)) (window-list))))

  (cond (menu-bar-mode
	 (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
	 (set-frame-height nil (+ (frame-height) 4))
	 (if fs (progn (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
					      '(1 "_NET_WM_STATE_FULLSCREEN" 0))
		       (add-hook 'window-configuration-change-hook 'fullscreen-margins))))
	(t (menu-bar-mode 1) (scroll-bar-mode 1)
	   (when (frame-parameter nil 'fullscreen)
	     (remove-hook 'window-configuration-change-hook 'fullscreen-margins)
	     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
				    '(0 "_NET_WM_STATE_FULLSCREEN" 0))
	     (set-window-buffer (selected-window) (current-buffer)))
	   (set-frame-width nil (assoc-default 'width default-frame-alist)))))

(global-set-key [f11] 'toggle-minimal-mode)




;;————————————————————————————————————————————————————————————————————
;;                   2. Programming

(require 'auto-complete)
(require 'auto-complete-config)
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode)
;; Start auto-completion after 1 char of a word
(setq ac-auto-start 1)
;; Ignore case
(setq ac-ignore-case nil)

;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Symbol highlighting
(defun  highlight-symbol-at-point ()
  "Highlight the SYMBOL at the cursor position."
  (interactive)
  (highlight-at-point 'symbol))

(defun highlight-at-point (THING)
  "Highlight the THING at the current cursor position."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (setq thing-as-str (buffer-substring-no-properties
                        (car bounds) (cdr bounds)))
    (add-to-history 'regexp-history thing-as-str)
    (hi-lock-face-buffer (regexp-quote thing-as-str) 'hi-pink)))

(defun highlight-reset ()
  "Undo the whole regexp highlight history."
  (interactive)
  (mapc (lambda (r)(hi-lock-unface-buffer r))
        regexp-history))


;;--------------------------------------------------------------------
;;                   2.1 Lisp

;; Slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/ccl")
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup '(slime-fancy))


;;--------------------------------------------------------------------
;;                   2.2 Ruby

;; ruby completion, navigation and documentation lookup
(add-hook 'ruby-mode-hook 'robe-mode)

;; disable show-trailing-whitespace
(add-hook 'ruby-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))


;;--------------------------------------------------------------------
;;                   2.3 Python

(add-to-list 'load-path "~/.emacs.d/python-mode")
(setq py-install-directory "~/.emacs.d/python-mode")
(require 'python-mode)


;;--------------------------------------------------------------------
;;                   2.4 Clojure

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)


;;--------------------------------------------------------------------
;;                   2.5 Web

;; HAML
(require 'haml-mode)

;; CoffeScript
(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))




;;————————————————————————————————————————————————————————————————————
;;                   3. Custom sets

(defun startup-theme ()
  (if window-system '(octopussy)
    '(faceless)))

(custom-set-variables
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (startup-theme))
 '(custom-safe-themes
   '("eb048b4126bb42ea1a2207afcafa43937764b39e83fe30b406514e9abd7834a5"
     "34d26daceb2ca8e1cdd00fa7b96a6e9161f74d992b701454075898a7a237d4ef"
     default))
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-rainbow-delimiters-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format " %7i ")
 '(menu-bar-mode nil)
 '(projectile-global-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil))

;; Font
(custom-set-faces
 '(default
    ((t (:slant normal
                :weight normal
                :height 113
                :width normal
                :foundry "unknown"
                :family "Droid Sans Mono")))))
