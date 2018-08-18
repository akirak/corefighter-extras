;;; corefighter-git-statuses.el --- Repository statuses in core fighter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (corefighter "0.2") (repom "1.0") (dash "2.10"))
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
;;       '((corefighter-git-statuses)
;;         (corefighter-org-agenda)))

;;; Code:

(require 'corefighter)
(require 'repom)
(require 'dash)

(defclass corefighter-git-statuses (corefighter-module)
  ;; TODO: Allow restricting target repositories
  ((title :initform "Dirty Git repositories")
   (navigate-action
    :initform (corefighter-make-action
               (lambda (repo)
                 (let ((magit-display-buffer-function
                        #'magit-display-buffer-same-window-except-diff-v1))
                   (magit-status repo)))))
   (fields :initarg :fields
           :initform '(dirty untracked)))
  "Core Fighter module to check dirty states of local Git
repositories.  See `repom-git-statuses` for definitions of the
options.")

(cl-defmethod corefighter-module-items ((obj corefighter-git-statuses)
                                        &optional _refresh)
  (cl-loop for (repo . sums) in (repom-git-statuses (oref obj fields))
           collect (make-corefighter-item
                    :title (abbreviate-file-name repo)
                    :description
                    (mapconcat #'repom-git-status-summary
                               sums
                               "\n")
                    :payload repo)))

(provide 'corefighter-git-statuses)
;;; corefighter-git-statuses.el ends here
