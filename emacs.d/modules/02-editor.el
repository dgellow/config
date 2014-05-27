;;; editor.el --- dgellow's editor configuration.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping/
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not port of GNU Emacs.

;;; Commentary:

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:
;; Backup files
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;; Tab width
(setq tab-width 2)

;; Indent on C-m
(electric-indent-mode t)

;; Create parentheses by pair
(electric-pair-mode t)

;; Save minibuffer history
(savehist-mode 1)

;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Do not use tabs
(setq-default indent-tabs-mode nil)

;; Better clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; Extend search paths for apropos
(setq apropos-do-all t)

;; Load auto-complete
(use-package auto-complete
  :init (global-auto-complete-mode t)
  :config (progn
            ;; Use dictionaries by default
;;            (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
            ;; Start auto-completion after 1 char of a word
            (setq ac-auto-start 1)
            ;; Ignore case
            (setq ac-ignore-case nil)))

;; Load global-flycheck-mode
(defun dg-init-flycheck-mode ()
  "If `global-flycheck-mode' exists, load it."
  (when (fboundp 'global-flycheck-mode)
    (global-flycheck-mode t)))

(add-hook 'after-init-hook #'dg-init-flycheck-mode)

(provide 'editor)
;;; editor.el ends here
