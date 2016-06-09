;;; clojure.el --- clojure configuration..
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping/
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
(require 'clojure-mode)

(defvar dg-list-om-dom-tags
  '(a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    marquee
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr

    ;; other, from https://github.com/swannodette/om/blob/f9cd0ce959b17b903a78f5b9c79300291eba7b00/src/om/dom.cljs
    input
    textarea
    option)
  "Based on the list from om/dom.clj.

Consulted version: https://github.com/swannodette/om/blob/4e4aa4ee560ad3a3ce7600ffbeb8a9ede144c333/src/om/dom.clj")

(defun dg-set-om-indent-rules ()
  "Fix the way functions from om.dom and om.next are indented."
  (define-clojure-indent
    (defui 1)
    (render 1)
    (add-root! 1)
    (query 1)
    (ident 1)
    (transact! 1))
  ;; DOM tags indentation
  (mapc (lambda (x) (put-clojure-indent x 'defun)) dg-list-om-dom-tags))

(defvar dg-list-clj-core-fn-indent-as-defun
  '(apply))

(defun dg-set-coreclj-indent-rules ()
  (mapc (lambda (x) (put-clojure-indent x 'defun))
        dg-list-clj-core-fn-indent-as-defun))

(use-package clojure-mode
  :init (progn
          (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
          (add-hook 'cider-repl-mode-hook 'pretty-mode)
          (setq cider-repl-use-clojure-font-lock t)
          (setq cider-repl-display-help-banner nil)
          (dg-set-om-indent-rules)
          (dg-set-coreclj-indent-rules)))

(provide 'dg-clojure)
;;; clojure.el ends here
