;;; jsdoc.el --- Provides an easy way to insert JSDoc comments with the power of tree-sitter   -*- lexical-binding: t -*-

;; Copyright (C) 2021 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamert@protonmail.com>
;; Version: 0.1
;; URL: https://github.com/isamert/jsdoc.el
;; Package-Requires: ((emacs "25.1") (dash "2.11.0") (s "1.12.0") (tsc "0.13.1"))

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

;; Insert JSDoc function comments and arbitrary typedefs easily.

;;; Code:

;; Some important files:
;; https://github.com/tree-sitter/tree-sitter-javascript/blob/master/src/grammar.json
;; https://github.com/tree-sitter/tree-sitter-javascript/blob/master/src/node-types.json

(defcustom jsdoc-append-dash t
  "Wheter to append \" - \" after @param, @returns etc. to enhance readability.")

;;;###autoload
(defun jsdoc ()
  (interactive)
  (let* ((meta (jsdoc-generate))
         (col (current-indentation))
         (params (plist-get meta :params))
         (returns (plist-get meta :returns))
         (throws (plist-get meta :throws)))
    (jsdoc--insert-line col 'beg nil)
    (jsdoc--insert-line col 'empty nil)
    (--each params (jsdoc--insert-line col 'mid 'param it))
    (when throws
      (jsdoc--insert-line col 'mid 'throws throws))
    (when returns
      (jsdoc--insert-line col 'mid 'returns returns))
    (jsdoc--insert-line col 'end nil)))

(defun jsdoc--insert-line (col-no w tag &optional it)
  (let* ((col (s-repeat col-no " "))
         (tag-text (case tag
                     ('param (jsdoc--format-param it))
                     ('throws (format "@throws {%s} " it))
                     ('returns (format "@returns {%s} " it))
                     (otherwise nil)))
         (tag-text-fixed (if (and jsdoc-append-dash tag-text)
                             (s-concat tag-text "- ")
                           tag-text))
         (start (case w
                  ('beg "/**" )
                  ('end " */")
                  ('empty " * ")
                  (otherwise " * "))))
    (move-beginning-of-line nil)
    (insert (format "%s%s%s\n" (or col "") (or start "") (or tag-text-fixed "")))))

(defun jsdoc--format-param (it)
  (format
   "@param {%s} %s "
   (plist-get it :type)
   (if (plist-get it :default)
       (format "[%s=%s]" (plist-get it :name) (plist-get it :default))
     (plist-get it :name))))

(defun jsdoc-generate ()
  (let* ((curr-node (tsc-get-parent (tree-sitter-node-at-point)))
         (curr-node-type (tsc-node-type curr-node)))
    (case curr-node-type
      ('lexical_declaration (jsdoc-parse-lexical-declaration curr-node))
      ('method_definition (jsdoc-parse-method-definition curr-node))
      ('function_declaration (jsdoc-parse-function-declaration curr-node)))))

(defun jsdoc-parse-lexical-declaration (node)
  (let* ((fn-def (tsc-get-nth-named-child node 0))
         (name (tsc-child-text fn-def :name))
         (fn (tsc-get-nth-named-child fn-def 1))
         (fn-type (tsc-node-type fn)))
    (case fn-type
      ('arrow_function (jsdoc-parse-arrow-function fn name)))))

(defun jsdoc-parse-method-definition (node)
  (let* ((name (tsc-child-text node :name)))
    (jsdoc-parse-arrow-function node name)))


(defun jsdoc-parse-arrow-function (fn name)
  (let* ((params (or (tsc-get-child-by-field fn :parameters) (tsc-get-nth-child fn 0))))
    (list
     :name name
     :returns (jsdoc-get-return-type fn)
     :throws (jsdoc-get-throw-type fn)
     :params (--map (jsdoc-parse-param it) (or (tsc-named-children params) (list params))))))

(defun jsdoc-parse-param (param)
  "Parse PARAM and return it's name with type and the default value if it exists."
  (case (tsc-node-type param)
    ('identifier
     (list
      :name (tsc-node-text param)
      :type "any"))
    ('shorthand_property_identifier
     (list
      :name (tsc-node-text param)
      :type "any"))
    ('assignment_pattern
     (list
      :name (plist-get (jsdoc-parse-param (tsc-get-child-by-field param :left)) :name)
      :default (tsc-node-text (tsc-get-child-by-field param :right))
      :type (jsdoc-infer-type (tsc-get-child-by-field param :right))))
    ('array_pattern
     (list
      :name 'unnamed-param
      :type (--reduce (s-concat acc "," it) (--map (plist-get (jsdoc-parse-param it) :type) (tsc-named-children param)))))
    ('object_pattern
     (list
      :name 'unnamed-param
      :type (--reduce (s-concat acc "," it) (--map (plist-get (jsdoc-parse-param it) :type) (tsc-named-children param)))))
    ('rest_parameter
     (list
      :name (tsc-node-text (tsc-get-nth-named-child param 0))
      :type "...any"))))

(defun jsdoc-infer-type (node)
  (case (tsc-node-type node)
    ('identifier (jsdoc-infer-identifier node))
    ('true "boolean")
    ('false "boolean")
    ('number "number")
    ('string "string")
    ('array "any[]")
    ('object "object")
    ('new_expression (jsdoc-infer-type (tsc-get-nth-named-child node 0)))
    ('call_expression (jsdoc-infer-type (tsc-get-nth-named-child node 0)))
    ('binary_expression (jsdoc-infer-binary-expression node))
    (otherwise "any")))

(defun jsdoc-infer-binary-expression (node)
  (format "TODO"))

(defun jsdoc-infer-identifier (node)
  "Return given identifier NODE type.  `X' if `X()', otherwise `any'."
  (let* ((next-sibling (tsc-get-next-named-sibling node)))
    (if (and next-sibling
             (equal (tsc-node-type next-sibling) 'arguments)
             (s-uppercase? (substring (tsc-node-text node) 0 1)))
        (tsc-node-text node)
      "any")))

(defun jsdoc-get-return-type (node)
  "Return the return type of given NODE."
  (-if-let (return-type (jsdoc-get-returned-type-of-statement node 'return_statement))
      (pcase (tsc-node-text (tsc-get-nth-child node 0))
        ("async" (format "Promise<%s>" return-type))
        (otherwise return-type))
    (progn
      (jsdoc-infer-type (tsc-get-child-by-field node :body)))))

(defun jsdoc-get-throw-type (node)
  "Retun throw type of given NODE if it throws anythng.
Otherwise return nil."
  (--> (jsdoc-get-returned-type-of-statement node 'throw_statement)
    (if (and it (s-contains? "|" it))
        (format "(%s)" it)
      it)))

(defun jsdoc-get-returned-type-of-statement (node stmt)
  "Find the STMT somewhere under NODE and return the type."
  (-->
   (tsc-find-descendants-with-type node stmt)
   (--map (jsdoc-infer-type (tsc-get-nth-child it 1)) it)
   (-distinct it)
   (when it
     (--reduce (format "%s | %s" acc it) it))))


;;
;; tsc utils
;;

(defun tsc-child-text (node prop)
  (tsc-node-text (tsc-get-child-by-field node prop)))

(defun tsc-children (node)
  (--map (tsc-get-nth-child node it) (number-sequence 0 (1- (tsc-count-children node)))))

(defun tsc-named-children (node)
  (--map (tsc-get-nth-named-child node it) (number-sequence 0 (1- (tsc-count-named-children node)))))

(defun tsc-find-descendants-with-type (node type)
  (-flatten (--map (if (equal type (tsc-node-type it))
                       it
                     (tsc-find-descendants-with-type it type))
                   (tsc-children node))))

;; debg

(defun tsc-children-types (node)
  (--map (tsc-node-type it) (tsc-children node)))
