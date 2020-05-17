;;; surgeon.el --- Manipulate small parts of text with the precision of a surgeon.
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
;;
;; (use-package thingatpt)
;;
;; (defun select-current-word ()
;;   "Select as a region the word at point."
;;   (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;     (push-mark bounds)
;;     ()))
;;
;; (defun select-current-sexp ()
;;  "Select as a region the s-expression (or what your current language mode considers as one) at point."
;;  (interactive)
;;  (let (sum-run start-position end-position)
;;    (skip-chars-backward "^\(")
;;    (setq start-position (- (point) 1))
;;    ;; (skip-chars-forward "^\)")
;;    ;; (setq end-position (point))
;;
;;    ;; Loop on each chars
;;    ;; -1 if ')'
;;    ;; +1 if '('
;;    ;; stop loop if 0
;;    (while (not (= sum-run 1))
;;      ;; TODO
;;      )
;;    (goto-char start-position)))
;;
;;(defun delete-current-word ()
;;  "Delete the word at point."
;;  ())
;;
;;(defun delete-current-sexp ()
;;  "Delete the sexp at point."
;;  ())
;;
;;(provide 'surgeon)
;;; surgeon.el ends here
