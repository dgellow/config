#!emacs --script

;;; bootstrap.el --- dgellow's config bootstrapping entry point.
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
(defun dg-match-first (REGEXP STRING)
  "Return the first group matched by applying REGEXP on STRING."
  (string-match REGEXP STRING)
  (match-string 1 STRING))

(defun dg-message (STRING)
  "Print a message with `load-file-name' as context and STRING as value.
If `load-file-name' is empty, use the value of `current-buffer'."
  (let* ((filename (or load-file-name buffer-file-name))
         (regexp-filename "\\(\\w+\\.el$\\)")
         (context (dg-match-first regexp-filename filename)))
    (if context
      (message "[%s]: %s" context STRING))))

;; Home directory
(defvar dg-user-home-dir
  (substitute-in-file-name "$HOME")
  "User's home directory.")

;; Current directory
(defvar load-file-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of current file.")

;; Current operating system
(defvar linux-p nil
  "'t if value of `system-type' is `gnu/linux'.  nil otherwise.")
(defvar mac-p nil
  "'t if value of `system-type' is `darwin'.  nil otherwise.")

(pcase system-type
  (`gnu/linux (setq linux-p 't))
  (`darwin (setq mac-p 't)))

;; Throw exception if not a supported platform
(unless (or linux-p mac-p)
  (dg-message "Only Linux and Mac OS X are supported for now.")
  (throw 'unsupported-platform 't))

;; Load modules and utils
(defun dg-load-modules (DIRECTORY)
  "Load every *.el files in the given DIRECTORY."
  (when (file-exists-p DIRECTORY)
    (mapc 'load (directory-files DIRECTORY 't "^[^#].*\\.el$"))))

;; Modules directory
(defvar dg-bootstrap-dir
  (expand-file-name "bootstrap" load-file-dir))

;; Load modules
(dg-message "Modules:")
(dg-load-modules dg-bootstrap-dir)


;;; bootstrap.el ends here
