;;; install-cask.el --- Install cask if it is not already.
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
(dg-message "Begin ... ")

;; Install from git
(defvar dg-cask-git-repo
  "https://github.com/cask/cask.git"
  "Cask's git repository.")

(defvar dg-cask-dir
  (expand-file-name ".cask" dg-user-home-dir)
  "Cask's install directory.")

(defvar dg-cask-issue-tracker
  "https://github.com/cask/cask/issues"
  "Cask's issue tracker.")

(defvar dg-cask-bin-path
  (expand-file-name "bin/cask" dg-cask-dir)
  "Path of Cask executable.")

(defun dg-cask-bootstrap ()
  "Bootstrap cask."
  (let* ((params "upgrade-cask")
         (command (format "%s %s" dg-cask-bin-path params)))
    (shell-command-to-string command)))

(defun dg-cask-install-from-git ()
  "Install cask by cloning the git repository.

Clone from `dg-cask-git-repo' to `dg-cask-dir'."
  (dg-git-clone dg-cask-git-repo dg-cask-dir))

(defun dg-cask-linux-installed-p ()
  "Check if a .cask file already exists."
  (file-exists-p dg-cask-dir))

(defun dg-cask-linux-install ()
  "Make some check then install cask from git."
  (let ((msg-file-exists
         (format "Directory %s exists. Is Cask already installed?"
                   dg-cask-dir))
        (msg-not-linux "You are not running GNU/Linux.")
        (msg-success "Successfully installed Cask!  Now, add the cask binary to your $PATH."))
    (cond
     ((not linux-p) (dg-message msg-not-linux))
     ((dg-cask-linux-installed-p) (dg-message msg-file-exists))
     ( t (dg-cask-install-from-git)
         (dg-cask-bootstrap)
         (dg-message msg-success)))))

;; Install from homebrew
(defvar dg-cask-brew-path nil "Path of brew executable.")

(defun dg-cask-set-brew-path ()
  "Set `dg-cask-brew-path'."
  (let ((brew-path (shell-command-to-string "which brew"))
        (regex "not found"))
    (unless (string-match regex brew-path)
      (setq dg-cask-brew-path brew-path))))

(defvar dg-cask-brew-params
  "install cask"
  "Parameters to use when running brew.")

(defun dg-cask-install-from-brew ()
  "Install cask with the package manager homebrew.

Homebrew is only available on Mac OS X."
  (unless mac-p
    (throw 'only-with-macosx 't))
  (let ((brew-command
         (format "%s %s" dg-cask-brew-path dg-cask-brew-params)))
    (dg-message "Install from brew ... ")
    (shell-command-to-string brew-command)))

(defun dg-cask-mac-installed-p ()
  "Check if cask is already installed."
  (let ((cask-path (shell-command-to-string "which cask"))
        (regex "not found"))
    (not (string-match regex cask-path))))

(defun dg-cask-mac-install ()
  "Make some check then install cask from brew."
  (let ((msg-exists "Cask is already installed. Check with `which cask'.")
        (msg-not-mac "You are not running Mac OS X.")
        (msg-success "Successfully installed Cask!  Now, add the cask binary to your $PATH."))
    (cond
     ((not mac-p) (dg-message msg-not-mac))
     ((dg-cask-mac-installed-p) (dg-message msg-exists))
     (t (dg-cask-install-from-brew)
        (dg-message msg-success)))))


;; Run the install
(cond (linux-p (dg-cask-linux-install))
      (mac-p (dg-cask-mac-install)))

(dg-message "Done.")

(provide 'install-cask)
;;; install-cask.el ends here
