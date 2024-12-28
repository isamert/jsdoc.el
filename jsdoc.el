;;; jsdoc.el --- Insert JSDoc comments -*- lexical-binding: t -*-

;; Copyright (C) 2021 Isa Mert Gurbuz
;; Copyright (C) 2023 Demis Balbach, Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.4.0
;; URL: https://github.com/isamert/jsdoc.el
;; Package-Requires: ((emacs "29.1") (dash "2.11.0") (s "1.12.0"))

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

;; This package provides an easy way to insert JSDoc function comments
;; and typedefs.  It leverages a variety of techniques to generate
;; documentation as much as possible without human interaction,
;; including type inference and other similar approaches.

;;; Code:

(require 's)
(require 'dash)
(require 'treesit)

(defgroup jsdoc nil
  "JSDoc helper for Emacs."
  :group 'tools)

(defcustom jsdoc-append-dash t
  "Whether to append \" - \" after @param, @returns etc. to enhance readability."
  :type 'boolean
  :group 'jsdoc)

;;;###autoload
(defun jsdoc ()
  "Insert a JSDoc function comment or a typedef for an object."
  (interactive)
  (let* ((meta (jsdoc--generate))
         (col (current-indentation)))
    (pcase (plist-get meta :doc-kind)
      ('function (jsdoc--print-function-doc meta col))
      ('typedef (jsdoc--print-typedef-doc meta col)))))

(defun jsdoc--print-function-doc (meta col)
  (let* ((params (plist-get meta :params))
         (type-params (plist-get meta :type-params))
         (returns (plist-get meta :returns))
         (throws (plist-get meta :throws)))
    (jsdoc--insert-line col 'beg nil)
    (jsdoc--insert-line col 'empty nil)
    (jsdoc--insert-line col 'skip nil)
    (-each params
      (lambda (param)
        (jsdoc--insert-line col 'mid 'param param)
        (when-let* ((children (plist-get param :children)))
          (--each children
            (jsdoc--insert-line col 'mid 'param `(:name ,(format "%s.%s" (plist-get param :name) (plist-get it :name)) ,@it))))))
    (when type-params
      (--each type-params (jsdoc--insert-line col 'mid 'typeParam it)))
    (when returns
      (jsdoc--insert-line col 'mid 'returns returns))
    (when throws
      (jsdoc--insert-line col 'empty nil)
      (when (jsdoc--typescript?)
        (jsdoc--insert-line col 'mid 'throws throws)
        (jsdoc--insert-line col 'empty nil)))
    (jsdoc--insert-line col 'end nil)) )

;; TODO: This only prints out one-liner types, like {hasPower:
;; boolean, hasWisdom: boolean}, but I would also like to have the
;; following form:
;;
;; /**
;;  *
;;  * @typedef {Object}  -
;;  * @property {boolean} hasPower -
;;  * @property {boolean} hasWisdom -
;;  */
;;
;; See https://jsdoc.app/tags-typedef
;;
;; We are already doing something similar with object params in
;; `jsdoc--print-function-doc'.
(defun jsdoc--print-typedef-doc (meta col)
  (let* ((type (plist-get meta :type)))
    (jsdoc--insert-line col 'beg nil)
    (jsdoc--insert-line col 'empty nil)
    (when type
      (jsdoc--insert-line col 'mid 'typedef type))
    (jsdoc--insert-line col 'end nil)) )

;; Some important resources:
;; https://github.com/tree-sitter/tree-sitter-javascript/blob/master/src/grammar.json
;; https://github.com/tree-sitter/tree-sitter-javascript/blob/master/src/node-types.json

(defun jsdoc--typescript? ()
  "Does current buffer contain TypeScript code?"
  (-contains? '(tsx-ts-mode typescript-ts-mode) major-mode))

(defun jsdoc--insert-line (col-no w tag &optional it)
  (let* ((col (s-repeat col-no " "))
         (tag-text (pcase tag
                     ('param (if (jsdoc--typescript?)
                                 (jsdoc--format-param-ts it)
                               (jsdoc--format-param-js it)))
                     ('throws (if (jsdoc--typescript?)
                                  (format "@throws {@link %s} " it)
                                (format "@throws {%s} " it)))
                     ('returns (if (jsdoc--typescript?)
                                   (format "@returns ")
                                 (format "@returns {%s} " it)))
                     ('typedef (format "@typedef {%s} " it))
                     ('typeParam (format "@typeParam %s " it))))
         (tag-text-fixed (if (and jsdoc-append-dash tag-text)
                             (s-concat tag-text "- ")
                           tag-text))
         (start (pcase w
                  ('beg "/**" )
                  ('end " */")
                  ('empty " * ")
                  ('skip " *")
                  (_ " * "))))
    (move-beginning-of-line nil)
    (insert (format "%s%s%s\n" (or col "") (or start "") (or tag-text-fixed "")))))

(defun jsdoc--format-param-ts (it)
  (format
   "@param %s "
   (plist-get it :name)))

(defun jsdoc--format-param-js (it)
  (format
   "@param {%s} %s "
   (plist-get it :type)
   (if (plist-get it :default)
       (format "[%s=%s]" (plist-get it :name) (plist-get it :default))
     (plist-get it :name))))

(defun jsdoc--generate ()
  (let* ((curr-node (treesit-node-parent (treesit-node-at (point))))
         (curr-node-type (treesit-node-type curr-node)))
    (pcase curr-node-type
      ("lexical_declaration"
       (jsdoc--parse-lexical-declaration curr-node))
      ((or "function_declaration" "method_definition" "function" "arrow_function" "function_expression")
       (jsdoc--parse-generic-function-declaration curr-node)))))

(defun jsdoc--parse-lexical-declaration (node)
  (let* ((def (treesit-node-child node 0 t))
         (name (jsdoc--tsc-child-text def "name"))
         (value (treesit-node-child (treesit-node-child node 0 t) 1 t))
         (value-type (treesit-node-type (treesit-node-child (treesit-node-child node 0 t) 1 t))))
    (pcase value-type
      ((or "arrow_function" "function") (jsdoc--parse-generic-function value name))
      ("object" (jsdoc--parse-object value name))
      (other (user-error "Can't document object of type: %s" other)))))

(defun jsdoc--parse-generic-function-declaration (node)
  (let* ((name (jsdoc--tsc-child-text node "name")))
    (jsdoc--parse-generic-function node name)))

(defun jsdoc--parse-generic-function (fn name)
  (let* ((params (or (treesit-node-child-by-field-name fn "parameters") (treesit-node-child fn 0))))
    (list
     :name name
     :returns (jsdoc--get-return-type fn)
     :doc-kind 'function
     ;; TODO This should be a list, a function can throw multiple
     ;; exceptions
     :throws (jsdoc--get-throw-type fn)
     :params (-map #'jsdoc--parse-param (or (jsdoc--tsc-named-children params) (list params)))
     ;; TypeScript only
     :type-params (-map
                   #'treesit-node-text
                   (ignore-errors
                     (-some->> "type_parameters"
                       (treesit-node-child-by-field-name fn)
                       treesit-node-children
                       (-drop 1)
                       (-drop-last 1)))))))

(defun jsdoc--parse-object (node name)
  (list
   :doc-kind 'typedef
   :type (jsdoc--infer-type node)))

(defun jsdoc--parse-param (param)
  "Parse PARAM and return it's name with type and the default value if it exists."
  (pcase (treesit-node-type param)
    ("identifier"
     (list
      :name (treesit-node-text param)
      :type "*"))
    ("shorthand_property_identifier_pattern"
     (list
      :name (treesit-node-text param)
      :type "*"))
    ("assignment_pattern"
     (list
      :name (plist-get (jsdoc--parse-param (treesit-node-child-by-field-name param "left")) :name)
      :default (treesit-node-text (treesit-node-child-by-field-name param "right"))
      :type (jsdoc--infer-type (treesit-node-child-by-field-name param "right"))))
    ("array_pattern"
     (list
      :name 'x
      :type (jsdoc--infer-array-type (jsdoc--tsc-named-children param))))
    ("object_assignment_pattern"
     (list
      :name (treesit-node-text (treesit-node-child-by-field-name param "left"))
      :default (treesit-node-text (treesit-node-child-by-field-name param "right"))
      :type (jsdoc--infer-type (treesit-node-child-by-field-name param "right"))))
    ("object_pattern"
     (list
      :name 'x
      :type "Object"
      :children (--map
                 (jsdoc--parse-param it)
                 (treesit-node-children param t))))
    ("rest_pattern"
     (list
      :name (treesit-node-text (treesit-node-child param 0 t))
      :type "...*"))
    ;; TypeScript parameter types. :type is not needed but I just add it anyway
    ((or "required_parameter" "optional_parameter")
     (let ((x (jsdoc--parse-param (treesit-node-child param 0 t))))
       (list
        :name (plist-get x :name)
        :children (plist-get x :children)
        :type (treesit-node-text
               (treesit-node-child
                (treesit-node-child-by-field-name param "type") 0 t)))))))

(defun jsdoc--infer-type (node)
  (pcase (treesit-node-type node)
    ("identifier" (jsdoc--infer-identifier node))
    ("true" "boolean")
    ("false" "boolean")
    ("number" "number")
    ("string" "string")
    ("array" "*[]")
    ("object" (jsdoc--infer-object node))
    ("pair" (jsdoc--infer-type (treesit-node-child-by-field-name node "value")))
    ("new_expression" (jsdoc--infer-type (treesit-node-child node 0 t)))
    ("call_expression" (jsdoc--infer-type (treesit-node-child node 0 t)))
    ("arrow_function" (jsdoc--infer-closure-type node))
    ("binary_expression" (jsdoc--infer-binary-expression node))
    ("unary_expression" (jsdoc--infer-unary-expression node))
    (_ "*")))

(defun jsdoc--infer-closure-type (node)
  "Return the inferred type of the given closure NODE."
  (let* ((fn (jsdoc--parse-generic-function node ""))
         (params (s-join ", " (--map (plist-get it :type) (plist-get fn :params))))
         (returns (plist-get fn :returns)))
    (concat "function(" params "): " returns)))

(defun jsdoc--infer-array-type (node)
  "Return the inferred type of the given array NODE."
  (let* ((params (--reduce
                  (if (eq acc it)
                      acc
                    (s-concat acc "|" it))
                  (--map (plist-get (jsdoc--parse-param it) :type) node))))
    (if (string-match-p (regexp-quote "|") params)
        (concat "(" params ")" "[]")
      (concat params "[]"))))

;; TODO: Expand the inference
;; https://stackoverflow.com/questions/12122293/list-of-all-binary-operators-in-javascript
(defun jsdoc--infer-binary-expression (node)
  (pcase (treesit-node-text (treesit-node-child-by-field-name node "operator"))
    ((or ">" "<" "==" "===") "boolean")
    ("-" "number")
    ("+" (jsdoc--infer-type (treesit-node-child-by-field-name node "left")))
    (_ "*")))

;; TODO: Expand the inference
;; https://www.digitalocean.com/community/tutorials/javascript-unary-operators-simple-and-useful#summary-of-all-unary-operators
(defun jsdoc--infer-unary-expression (node)
  (pcase (treesit-node-text (treesit-node-child-by-field-name node "operator"))
    ("!" "boolean")
    (_ "*")))

(defun jsdoc--infer-identifier (node)
  "Return given identifier NODE type.
`X' if `X()', otherwise `*'."
  (let* ((next-sibling (treesit-node-next-sibling node t)))
    (if (and next-sibling
             (equal (treesit-node-type next-sibling) "arguments")
             (s-uppercase? (substring (treesit-node-text node) 0 1)))
        (treesit-node-text node)
      "*")))

(defun jsdoc--infer-object (node)
  (let ((pairs (--map
                (list
                 :key
                 (substring-no-properties (treesit-node-text (treesit-node-child-by-field-name it "key")))
                 :type
                 (jsdoc--infer-type it))
                (treesit-node-children node :collect-named))))
    ;; TODO: if all pair types are same, then convert into {Object.<string, TYPE>}
    ;; TODO: check if buffer has already a @param or @typedef definition for this object?
    (concat
     "{ "
     (s-join ", " (--map (concat (plist-get it :key) ": " (plist-get it :type)) pairs))
     " }")))

(defun jsdoc--get-return-type (node)
  "Return the return type of given NODE."
  (-if-let (return-type (jsdoc--get-returned-type-of-statement node "return_statement"))
      (pcase (treesit-node-text (treesit-node-child node 0))
        ("async" (format "Promise<%s>" return-type))
        (_ return-type))
    (progn
      (jsdoc--infer-type (treesit-node-child-by-field-name node "body")))))

(defun jsdoc--get-throw-type (node)
  "Retun throw type of given NODE if it throws anythng.
Otherwise return nil."
  (--> (jsdoc--get-returned-type-of-statement node "throw_statement")
    (if (and it (s-contains? "|" it))
        (format "(%s)" it)
      it)))

(defun jsdoc--get-returned-type-of-statement (node stmt)
  "Find the STMT somewhere under NODE and return the type."
  (-->
   (jsdoc--tsc-find-descendants-with-type node stmt)
   (--map (jsdoc--infer-type (treesit-node-child it 1)) it)
   (-distinct it)
   (when it
     (--reduce (format "%s | %s" acc it) it))))


;;
;; tsc utils
;;

(defun jsdoc--tsc-child-text (node prop)
  (treesit-node-text (treesit-node-child-by-field-name node prop)))

(defun jsdoc--tsc-children (node)
  (--map (treesit-node-child node it) (number-sequence 0 (1- (treesit-node-child-count node)))))

(defun jsdoc--tsc-named-children (node)
  (--map (treesit-node-child node it t) (number-sequence 0 (1- (treesit-node-child-count node t)))))

(defun jsdoc--tsc-find-descendants-with-type (node type)
  (-flatten (--map (if (equal type (treesit-node-type it))
                       it
                     (jsdoc--tsc-find-descendants-with-type it type))
                   (jsdoc--tsc-children node))))

(provide 'jsdoc)
;;; jsdoc.el ends here
