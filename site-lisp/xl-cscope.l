; -*- mode: lisp; package: xl-cscope -*-

;;; xl-cscope.l --- jump to xyzzy source.
;;
;; Copyright (c) 2008 MIYAMUKO Katsuyuki.
;;
;; Author: MIYAMUKO Katsuyuki <miyamuko@gmail.com>
;; Version: 0.3
;; Keywords: development
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; * xyzzy lisp の Built-in 関数・変数から対応する C++ の関数・変数に
;;   jump します。
;;
;; * tag jump に cscope を使っているので、NetInstaller から cscope をインストールしてください。
;;   2008.03.18 版以上が必要です。
;;
;; * 使い方
;;
;;   1. cscope -b を実行して xyzzy ソースの DB (cscope.out) を作っておく。
;;   2. *xl-cscope-xyzzy-source-directory* に xyzzy ソースのディレクトリを設定する。
;;   3. 実行
;;      - M-x xl-cscope-jump-to-xyzzy-source でミニバッファにシンボルを入力してソースを表示
;;      - Ctrl-F4 でカーソル下のシンボルのソースを表示
;;


;;; Code:

(in-package :lisp)

(require "cscope")
(defpackage :xl-cscope
  (:use
   :lisp :editor
   :cscope
   ))
(use-package :xl-cscope :user)


(in-package :xl-cscope)

(export '(*xl-cscope-xyzzy-source-directory*
          xl-cscope-jump-to-xyzzy-source
          xl-cscope-jump-to-xyzzy-source-current-word
          ))


(defvar *xl-cscope-xyzzy-source-directory*
  (merge-pathnames "xyzzy-src" (si:system-root)))

(defparameter *lisp->c-mapping*
  '(;; DEFUN2
    (list* . "list_star")
    (string= . "string_equal")
    (string-equal . "string_equalp")
    (string/= . "string_not_equal")
    (string-not-equal . "string_not_equalp")
    (string< . "string_less")
    (string> . "string_greater")
    (string<= . "string_not_greater")
    (string>= . "string_not_less")
    (= . "number_eql")
    (/= . "number_not_eql")
    (< . "number_less")
    (> . "number_greater")
    (<= . "number_not_greater")
    (>= . "number_not_less")
    (+ . "add")
    (- . "subtract")
    (* . "multiply")
    (/ . "divide")
    (char= . "char_eql")
    (char/= . "char_not_eql")
    (char< . "char_less")
    (char> . "char_greater")
    (char<= . "char_not_greater")
    (char>= . "char_not_less")
    (set-syntax-end-c++-comment . "set-syntax-end-cplusplus-comment")
    (set-syntax-start-c++-comment . "set-syntax-start-cplusplus-comment")
    (syntax-end-c++-comment-p . "syntax-end-cplusplus-comment-p")
    (syntax-c++-comment-p . "syntax-cplusplus-comment-p")
    ;; DEFVAR
    (*ime-does-not-process-C-\\* . "ime_does_not_process_control_backslach")
    (*enable-D&D-edit* . "enable_DnD_edit")
    ))

(defmacro with-set-default-directory (new-dir &body body)
  (let (($buffer (gensym))
        ($org-dir (gensym))
        ($new-dir (gensym)))
    `(let* ((,$buffer (selected-buffer))
            (,$org-dir (default-directory ,$buffer))
            (,$new-dir ,new-dir))
       (set-default-directory ,$new-dir ,$buffer)
       (unwind-protect
           (progn ,@body)
         (set-default-directory ,$org-dir ,$buffer)))))
(setf (get 'with-set-default-directory 'ed::lisp-indent-hook) 1)

(defun xl-cscope-jump-to-xyzzy-source (sym)
  (interactive "SBuild-in symbol: ")
  (multiple-value-bind (cname type)
      (xyzzy-lisp-symbol->c-name sym)
    (with-set-default-directory *xl-cscope-xyzzy-source-directory*
      (funcall (if (eq type :function)
                   'cscope-find-global-definition
                 'cscope-find-c-symbol)
               cname *xl-cscope-xyzzy-source-directory*))
    (message "~S => ~S" sym cname)
    ))

(defun xl-cscope-jump-to-xyzzy-source-current-word ()
  (interactive)
  (macrolet ((at (&body body)
               `(save-excursion ,@body (point))))
    (let ((s (at (skip-syntax-spec-backward "w_\\")))
          (e (at (skip-syntax-spec-forward "w_\\"))))
      (when (< s e)
        (xl-cscope-jump-to-xyzzy-source
         (read-from-string (buffer-substring s e)))))))
(global-set-key #\C-F4 'xl-cscope-jump-to-xyzzy-source-current-word)


(defun xyzzy-lisp-symbol->c-name (sym)
  (funcall (cond ((member sym '(t nil)) 'make-c-name-from-symbol)
                 ((subtypep sym 'condition) 'make-c-name-from-condition)
                 ((keywordp sym) 'make-c-name-from-keyword)
                 ((fboundp sym) 'make-c-name-from-function)
                 ((boundp sym) 'make-c-name-from-variable)
                 (t 'make-c-name-from-symbol))
           sym))

(defun make-c-name-from-symbol (sym)
  (values (make-c-name "Q" sym)
          :symbol))

(defun make-c-name-from-keyword (sym)
  (values (make-c-name "K" sym)
          :keyword))

(defun make-c-name-from-condition (sym)
  (values (make-c-name "FE" sym)
          :condition))

(defun make-c-name-from-function (sym)
  (unless (si:*builtin-function-p (symbol-function sym))
    (plain-error "~S は builtin function ではありません" sym))
  (values (make-c-name (if (systemp sym) "Fsi_" "F") sym)
          :function))

(defun make-c-name-from-variable (sym)
  (values (make-c-name (if (systemp sym) "Vsi_" "V") sym)
          :variable))

(defun make-c-name (prefix sym)
  (let ((str (or (cdr (assoc sym *lisp->c-mapping*))
                 (symbol-name sym))))
    (concat prefix (remove #\* (substitute #\_ #\- str)))))


(defun systemp (sym)
  (string-equal "system" (package-name (symbol-package sym))))


#|
テスト

car
(cdr '())
'set-text-attribute
#'set-text-attribute

si:system-root
si::system-root
si:*simple-array-p

si:*accept-kill-xyzzy*
si::*accept-kill-xyzzy*

*kill-xyzzy-hook*
ed:*kill-xyzzy-hook*
ed::*kill-xyzzy-hook*

simple-error type-error network-error

:empty :ibeam :arrow :strike-out :extend

list* * / + - string<
*ime-does-not-process-C-\\*
*enable-D&D-edit*

t
nil
|#


(provide "xl-cscope")

;;; xl-cscope.l ends here
