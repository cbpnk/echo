;;; ob-echo-db.el --- Babel Functions for Echo Database -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Nick Savage <nick@nicksavage.ca>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; copied from ob-sqlite.el of org-mode

;;; Code:

(require 'sql)

;;;###autoload (with-eval-after-load 'org-src (add-to-list 'org-src-lang-modes '("echo-db" . sql)))

;;;###autoload
(defun org-babel-execute:echo-db (body _params)
  "Execute a block of Sqlite code in echo-db"
  (with-echo-db-transaction conn
    (sqlite-select conn body nil 'full)))

;;;###autoload
(defun org-babel-prep-session:echo-db (_session _params)
  "Raise an error because support for echo-db sessions isn't implemented."
  (error "SQLite sessions not yet implemented"))

;;;###autoload
(defun org-babel-edit-prep:echo-db (_info)
  (sql-set-product 'sqlite))

(provide 'ob-echo-db)
;;; ob-echo-db.el ends here
