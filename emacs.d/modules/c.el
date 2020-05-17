;;; c-mode.el --- C/C++ configuration.
;;
;; Copyright (c) 2016 Samuel El-Borai
;;
;; Author: Samuel El-Borai <sam@elborai.me>
;; URL: https://github.com/dgellow/
;; Version: 1.0.0
;; Keywords: c language

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

;; Disabled for now as irony-mode throws errors when using derived
;; modes (glsl-mode in my case).
;;
;; (use-package c-mode
;;   :init (add-hook 'c-mode-hook 'irony-mode))
;;
;; (use-package c++-mode
;;   :init (add-hook 'c++-mode-hook 'irony-mode))

(provide 'dg-c)
;;; c-mode.el ends here
