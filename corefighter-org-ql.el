;;; corefighter-org-ql.el --- Retrieve org headings using org-ql -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (corefighter "1.0") (dash "2.12") (org-ql "1.0"))
;; URL: https://github.com/akirak/corefighter-extras/

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a module for displaying headings
;; in Org files via corefighter interfaces.

;;; Code:

(require 'org-ql)
(require 'corefighter)
(require 'dash)

(defclass corefighter-org-ql (corefighter-module)
  ((title :initform "org-ql")
   (files :initarg :files
          :initform (lambda () (org-agenda-files))
          :type (or list
                    function
                    string
                    symbol)
          :documentation "Files to search entries in.")
   (q :initarg :q
      :documentation "How to filter items.
Accepts the same type as the second argument in `org-ql' (or more
precisely, `org-ql--query'.")
   (sort :initarg :sort
         :initform nil
         :documentation "How to sort items.
Accepts the same type as \":sort\" in `org-ql'.")
   (due :initarg :due
        :initform 'earlier
        :type (member scheduled
                      deadline
                      earlier)
        :documentation "Which property you use as the due date/time?"))
  "Core Fighter module that retrieve headings.")

(defun corefighter-org-ql--expand-files (files)
  "Expand FILES as \":files\" argument."
  (cl-etypecase files
    (function (funcall files))
    (symbol (if (boundp files)
                (symbol-value files)
              (error "Symbol %s is unbound" (symbol-name files))))
    (list (eval files))
    (string files)))

(defmethod corefighter-module-items ((obj corefighter-org-ql)
                                     &optional _refresh)
  (eval (macroexpand
         `(org-ql (quote ,(corefighter-org-ql--expand-files (oref obj files)))
            ,(oref obj q)
            :action-fn
            (lambda (element)
              (let* ((marker (make-marker))
                     (pos (org-element-property :begin element))
                     (title (org-element-property :raw-value element))
                     (scheduled (org-element-property :scheduled element))
                     (deadline (org-element-property :deadline element))
                     (due (cl-case ,(oref obj due)
                            ('scheduled scheduled)
                            ('deadline deadline)
                            ('earlier (cond
                                       ((and scheduled deadline)
                                        (-min-by (-on #'time-less-p
                                                      #'org-timestamp-to-time)
                                                 (list scheduled deadline)))
                                       (t (or scheduled deadline)))))))
                (set-marker marker pos)
                (make-corefighter-item
                 ;; FIXME: Make the title format configurable
                 :title (concat title
                                (if due
                                    (concat " " (org-element-property :raw-value due))
                                  ""))
                 :action `(org-goto-marker-or-bmk ,marker)
                 :due (when due
                        (corefighter-encode-time
                         (float-time (org-timestamp-to-time due)))))))
            :sort ,(oref obj sort)))))

(provide 'corefighter-org-ql)
;;; corefighter-org-ql.el ends here
