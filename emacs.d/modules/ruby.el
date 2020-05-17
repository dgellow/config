;;; ruby.el --- ruby configuration.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <sam@elborai.me>
;; URL: https://github.com/dgellow/config/
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

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
;; Use pry in inf-ruby if available
(defvar dg-pry-path
  (let ((pry-path (shell-command-to-string "which pry"))
        (regex "not found"))
    (unless (string-match regex pry-path)
      pry-path))
  "Path of pry executable.")

(defun dg-set-ruby-default-as-pry ()
  "If pry is installed, use is as `inf-ruby-default-implementation'."
  (when dg-pry-path
    (setq inf-ruby-default-implementation "pry")))

;; toggle do ... end block to curly braces or the inverse.
(defvar dg-ruby-block-do-re
  "\\<do\\(\\s-*\\|$\\)"
  "Regexp matching the 'do' part of a 'do ... end' block.")

(defun dg-goto-block-beginning ()
  "Move point to the beginning of current block.
It can be either 'do' or '{' keyword."
  (ruby-backward-sexp)
  (if (= (point) 1)
      nil
    (if (or (looking-at "{")
            (looking-at "\\<do\\(\\s-*\\|$\\)"))
        (point)
      (dg-goto-block-beginning))))

(defun dg-ruby-toggle-block ()
  "Toggle between do ... end block and curly braces.

The difference with `ruby-toggle-block' is that it can be used within a block."
  (interactive)
  (let ((pos-origin (point))
        (pos-begin-block (dg-goto-block-beginning)))
    (when pos-begin-block
        (ruby-toggle-block))
    (goto-char pos-origin)))

;; disable show-trailing-whitespace
(defun dg-hide-trailing-whitespace ()
  "Do not show trailing whitespaces."
  (setq show-trailing-whitespace nil))

;; Config ruby-mode
(use-package ruby-mode
  :bind (("C-c r b" . dg-ruby-toggle-block)
         ("C-c r r" . inf-ruby)
         ("C-c r l" . ruby-load-file))
  ;; ruby completion, navigation and documentation lookup
  :init (progn
          (dg-set-ruby-default-as-pry)
          (add-hook 'ruby-mode-hook 'robe-mode)
          (add-hook 'ruby-mode-hook #'dg-hide-trailing-whitespace)))

(provide 'dg-ruby)
;;; ruby.el ends here
