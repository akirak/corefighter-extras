;;; corefighter-repom.el --- Repository statuses in core fighter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (corefighter "1.0") (repom "1.0") (dash "2.10"))
;; URL: https://github.com/akirak/corefighter-extras

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

;; This is an extra module for Core Fighter which displays a list
;; of dirty Git repositories. The repository list is fetched by
;; repom.el <https://github.com/akirak/repom.el>, so you have to
;; install it as well.

;; An example configuration is as follows:
;; (setq corefighter-modules
;;       '((corefighter-repom-dirty)
;;         (corefighter-org-agenda)))

;;; Code:

(require 'corefighter)
(require 'repom)
(require 'dash)

(defclass corefighter-repom-dirty (corefighter-module)
  ((title :initform "Git repositories in dirty states")
   (fields :initarg :fields
           :initform '(dirty untracked))))

(cl-defmethod corefighter-module-items ((_obj corefighter-repom-dirty)
                                        &optional _refresh)
  (cl-loop for (repo . sums) in (repom-git-statuses (oref _obj fields))
           collect (make-corefighter-item
                    :title (abbreviate-file-name repo)
                    :description
                    (format "%s contains %d dirty files"
                            (abbreviate-file-name repo)
                            (-sum (mapcar #'cdr sums)))
                    :action-window 'other-window
                    :action `(magit-status ,repo))))

(provide 'corefighter-repom)
;;; corefighter-repom.el ends here
