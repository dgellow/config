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

;; Save minibuffer history
(savehist-mode 1)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Scroll up & down without changing point position
(global-set-key [(meta p)] 'scroll-up-line)
(global-set-key [(meta n)] 'scroll-down-line)

;; Move point to the top & bottom of the current window body
(defun goto-window-beginning ()
  "Move the point to the beginning of current window.

Bound to M-P."
  (interactive)
  (goto-char (window-start)))

(defun goto-window-end ()
  "Move the point to the end of current window.

Bound to M-N."
  (interactive)
  (goto-char (- (window-end) 1)))
(global-set-key [(meta P)] 'goto-window-beginning)
(global-set-key [(meta N)] 'goto-window-end)

;; Load global-flycheck-mode
(defun dg-init-flycheck-mode ()
  "If `global-flycheck-mode' exists, load it."
  (when (fboundp 'global-flycheck-mode)
    (global-flycheck-mode t)))

(add-hook 'after-init-hook #'dg-init-flycheck-mode)

(provide 'editor)
;;; editor.el ends here
