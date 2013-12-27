(require 'cl)
(require 'misc)

;; Custom functions
(add-to-list 'load-path "~/.emacs.d/functions/")

;; Packages repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;;--------------------------------------------------------------------
;; Floobits
(load "~/.emacs.d/floobits/floobits.el")

;;--------------------------------------------------------------------
;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq cursor-type '(bar . 1))

;;--------------------------------------------------------------------
;; Ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;;--------------------------------------------------------------------
;; Flx-ido
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;--------------------------------------------------------------------
;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup '(slime-fancy))

;;--------------------------------------------------------------------
;; Rinari
(require 'rinari)

;;--------------------------------------------------------------------
;; HAML
(require 'haml-mode)

;;--------------------------------------------------------------------
;; CoffeScript
(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;;--------------------------------------------------------------------
;; Save minibuffer history
(savehist-mode 1)

;;--------------------------------------------------------------------
;; Gmail configuration
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)

;;--------------------------------------------------------------------
;; Ruby

;; automatic indentation
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; ruby completion, navigation and documentation lookup
(add-hook 'ruby-mode-hook 'robe-mode)

;; ruby repl in an emacs buffer
(require 'inf-ruby)

;; to insert text automatically after:
;; - a correctly indented "end" when you write "class" or "def".
;; - the right parenthesis when you write a left parenthesis.
;; - an end-quote when you enter a quote.
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; disable show-trailing-whitespace
(add-hook 'ruby-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

;;--------------------------------------------------------------------
;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20130724.1750") 
(require 'auto-complete-config)
(ac-config-default)

;;--------------------------------------------------------------------
;; Emacs Multimedia System
(add-to-list 'load-path "~/.emacs.d/emms/lisp")
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;;--------------------------------------------------------------------
;; Powerline
;;(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(powerline-default-theme)
;; (setq powerline-arriow-shape 'arrow)   ;; default
;; (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts

;;--------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'rainbow-delimiters)

;;--------------------------------------------------------------------
;; Fullscreen mode
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


;;--------------------------------------------------------------------
;; Smooth scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(progn
	(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
	(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
	(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
	(setq scroll-step 1)) ;; keyboard scroll one line at a time

;;--------------------------------------------------------------------
;; Scroll up & down without moving the cursor

(global-set-key [(meta p)] 'scroll-up-line)
(global-set-key [(meta n)] 'scroll-down-line)

;;--------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (faceless)))
 '(custom-safe-themes (quote ("34d26daceb2ca8e1cdd00fa7b96a6e9161f74d992b701454075898a7a237d4ef" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-rainbow-delimiters-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format " %7i ")
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-right-side nil)
 '(tab-width 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#202020" :foreground "#cacaca" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

