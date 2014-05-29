;;; symlink-dot-files.el --- symlinks config files in $HOME directory.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping
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
(require 'cl) ; needed for (reduce ...)

(defun dg-symlink (target name)
  "Create a symbolic link to TARGET with the name NAME."
  (let ((command (format "ln -s %s %s" target name)))
    (shell-command-to-string command)))

(defvar dg-blacklist-dot-files
  '("bootstrap"
    "vagrantfile"
    "readme")
  "List of regexp to exclude files from the symlinking process.")

(defun dg-exclude-blacklisted-files (list-files)
  "Exclude files from LIST-FILES which match a regexp contained in `dg-blacklist-dot-files'."
  (delq nil (mapcar (lambda (file) (and (not (dg-blacklisted-p file)) file)) list-files)))

(defun dg-blacklisted-p (file)
  "Check if FILE is in `dg-blacklist-dot-files'."
  (reduce (lambda (x y) (or x y))
          (mapcar (lambda (regexp)
            (string-match-p regexp (file-name-nondirectory file)))
          dg-blacklist-dot-files)))

(defvar dg-list-dot-files
  (dg-exclude-blacklisted-files
   (directory-files load-file-dir 't "^[^\\.].*$"))
  "Files to symlink.")

;; Create symlinks
(defun dg-create-symlink (file)
  "Create a hidden symlink for FILE in $HOME.
If the hidden file already exists a confirmation to overwrite it is prompted."
  (let* ((dest-file-name
         (expand-file-name (format ".%s" (file-name-nondirectory file))
                           dg-user-home-dir))
         (msg-file-exists
          (format "File %s already exists." dest-file-name))
         (overwrite-p (and (file-exists-p dest-file-name)
                           (progn
                             (dg-message msg-file-exists)
                             (y-or-n-p "Overwrite?")))))
    (when overwrite-p
      (if (file-directory-p dest-file-name)
          (delete-directory dest-file-name t)
          (delete-file dest-file-name)))
    (unless (file-exists-p dest-file-name)
      (dg-symlink file dest-file-name))))

(mapc 'dg-create-symlink dg-list-dot-files)

(provide 'dg-symlink-dot-files)
;;; symlink-dot-files.el ends here
