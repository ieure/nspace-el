;;; nspace.el --- Pseudo-namespaces for Emacs Lisp

;; Copyright (C) 2013, 2015 Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 0.1.0
;; Keywords: lisp, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar *ns* nil
  "The current namespace")

(defconst nspace-separator "/"
  "The string which separates the namespace prefix from the symbol.")

(defun nspace-normalize (nsspec)
  "Turn NSSPEC into a namespace string."
  (cond ((stringp nsspec) nsspec)
        ((symbolp nsspec) (symbol-name nsspec))
        (t (error (format "Unknown nssspec `%s'" nsspec)))))

(defun nspace-sym-in-scopep (nsspec sym)
  "Is SYM in the scope of namespace NSSPEC?"
  (string-match-p (concat "^" nsspec nspace-separator)
                  (nspace-normalize sym)))

(defun nspace-symbols (nsspec)
  "Return a list of pseudo-namespace symbols."
  (let ((syms)
        (prefix (nspace-normalize nsspec)))
    (mapatoms
     (lambda (sym)
       (when (nspace-sym-in-scopep prefix sym)
         (push sym syms))))
    syms))

(defun nspace-make-function-thunk (method)
  "Make a thunk macro for namespace NS method METHOD."
  (let* ((name (symbol-name method))
         (ns (car (split-string name nspace-separator)))
         (bare (intern (substring name (1+ (length ns))))))
    `(,bare (&rest args) ,(list 'backquote (list method ',@args)))))

(defun nspace-make-symbol-thunk (sym)
  "Make a symbol-macrolet definition for namespace NS symbol SYM."
  (let* ((name (symbol-name sym))
         (ns (car (split-string name nspace-separator)))
         (bare (intern (substring name (1+ (length ns))))))
    (list bare sym)))

(defun nspace-make-thunks (ns)
  "Return namespace function / symbol thunks.

   Returns a list of (SYM-THUNKS FUN-THUNKS)."
  (let ((syms (nspace-symbols ns)))
    (list (mapcar 'nspace-make-symbol-thunk syms)
          (mapcar 'nspace-make-function-thunk
                  (remove-if-not 'fboundp syms)))))

(defmacro with-ns (ns &rest body)
  "Evaluate BODY in the context of namespace NS.

   Any function with a prefix of `NS/' will be aliased to its non
   nspace-qualified name while BODY is evaluated."
  (let* ((symfuncs (nspace-make-thunks ns))
         (syms (car symfuncs))
         (funcs (cadr symfuncs)))
    `(let ((*ns* ',ns)
           (max-lisp-eval-depth (* 10 max-lisp-eval-depth)))
       (macrolet ,funcs
         (symbol-macrolet ,syms
           ,@body)))))

(put 'with-ns 'lisp-indent-function 1)

(provide 'nspace)
;;; nspace.el ends here
