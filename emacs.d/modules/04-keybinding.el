;;; keybinding.el --- dgellow's keybindings.
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

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Switch bindings for search by regexp or string functions
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(provide 'keybinding)
;;; keybinding.el ends here
