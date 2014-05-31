;;; install-linux-packages.el --- install a set of linux packages.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping
;; Version: 1.0.0
;; Keywords:convenience

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
(require 'cl) ; for caddr

(defvar dg-linux-packages
  '(("ruby" . "ruby1.9.3")
    ("python" . "python2.7")
    ("zsh" . nil)
    ("curl" . nil)
    ("pip" . "python-pip")))

(defun dg-set-archlinux-packages ()
  ""
  (let ((arch-packages
         '(("ruby" . nil)
           ("python" . "python2"))))
    (mapc (lambda (x) (add-to-list 'dg-linux-packages x))
          arch-packages)))

(defvar dg-linux-distrib-alist
  '(("archlinux" . ("pacman" "-Sy --noconfirm" "-S --noconfirm %s"))
    ("debian" . ("aptitude" "update" "install -y %s"))
    ("ubuntu" . ("aptitude" "update" "install -y %s"))
    ("gentoo" . ("emerge" "--sync" "%s")))
  "A list of cons with the following format :
\(DISTRIB . (PACKAGE_MANAGER ARGS_UPDATE ARGS_INSTALL))")

;; Define current linux system
(defvar dg-archlinux-p nil)
(defvar dg-debian-p nil)
(defvar dg-ubuntu-p nil)
(defvar dg-gentoo-p nil)
(defvar dg-linux-unknown-p nil)

(defvar dg-current-linux
  (let* ((etc-issue (with-temp-buffer
                      (insert-file-contents "/etc/issue")
                      (buffer-string))))
    (cond ((string-match "arch linux" etc-issue)
           (setq dg-archlinux-p t)
           (assoc "archlinux" dg-linux-distrib-alist))
          ((string-match "debian" etc-issue)
           (setq dg-debian-p t)
           (assoc "debian" dg-linux-distrib-alist))
          ((string-match "ubuntu" etc-issue)
           (setq dg-ubuntu-p t)
           (assoc "ubuntu" dg-linux-distrib-alist))
          ((string-match "gentoo" etc-issue)
           (setq dg-gentoo-p t)
           (assoc "gentoo" dg-linux-distrib-alist))
          (t (setq dg-linux-unknown-p t)
             nil))))

(defun dg-sudo-shell-command (command)
  "Run COMMAND with sudo."
  (with-temp-buffer
    (cd "/sudo::/")
    (shell-command-to-string command)))

(defun dg-linux-packages-install ()
  "Update sources and install packages."
  (let* ((commands (cdr dg-current-linux))
         (pkg-man (car commands))
         (update-command (concat pkg-man " "
                                 (cadr commands)))
         (install-command (concat pkg-man " "
                                  (caddr commands))))
    (dg-message update-command)
    (dg-sudo-shell-command update-command)
    (mapc (lambda (pkg)
            (dg-message (or (cdr pkg)
                            (car pkg)))
            (dg-sudo-shell-command
             (format install-command (or (cdr pkg)
                                         (car pkg)))))
          dg-linux-packages)))

;; Patch list of packages
(cond
 (dg-archlinux-p (dg-set-archlinux-packages)))

;; Install
(if dg-current-linux
  (dg-linux-packages-install)
  (error "Current linux system cannot be identified"))

(provide 'dg-install-linux-packages)
;;; install-linux-packages.el ends here
