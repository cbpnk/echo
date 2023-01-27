;;; echo.el --- Org-mode properties cache  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>, Michael Albinus <michael.albinus@gmx.de>, Eric Schulte
;; URL: https://github.com/cbpnk/echo
;; Keywords: Org
;; Package-Requires: ((emacs "28.1") (org "9.0") (magit-section "3.0.0"))
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-attach)

(defcustom echo-directory (expand-file-name org-directory)
  "Default path to Echo files.
Should be absolute path
All Org files, at any level of nesting, are considered."
  :type 'directory
  :group 'echo)

(defcustom echo-file-exclude-regexp (regexp-quote org-attach-id-dir)
  "Directory matching this regular expression are excluded from Echo."
  :type '(choice
          (string :tag "Regular expression matching directories to ignore")
          (const :tag "Include everything" nil))
  :group 'echo)

(defcustom echo-collect-keywords '("TITLE")
  "collect file properties"
  :group 'echo
  :type '(repeat string))

(defconst echo--file-name-nodirectory-match-regexp "^[^.#]\\(.*[^~]\\)?$\\|^[#].*[^#~]$")

;;; Library

(defun echo--file-name-nondirectory-p (filename)
  (and
   (let ((mode (assoc-default filename auto-mode-alist 'string-match-p)))
     (and (symbolp mode)
          (provided-mode-derived-p mode 'org-mode)))
   (string-match-p echo--file-name-nodirectory-match-regexp filename)))

;;;###autoload
(defun echo-file-p (abs-path)
  "Return t if ABS-PATH is an Echo file, nil otherwise.

ABS-PATH is an Echo file if:
- It's located somewhere under `echo-directory'
- It doesn't match excluded regexp (`echo-file-exclude-regexp')"
  (and
   (string-prefix-p
    (file-name-as-directory echo-directory)
    abs-path)
   (not
    (string-match-p
     echo-file-exclude-regexp
     (file-relative-name abs-path echo-directory)))
   (or
    (directory-name-p abs-path)
    (echo--file-name-nondirectory-p (file-name-nondirectory abs-path)))))

;;;###autoload
(defun echo-buffer-p (&optional buffer)
  "Return t if BUFFER is an Echo file.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (not (buffer-base-buffer))
           (echo-file-p (buffer-file-name))))))

;;;###autoload
(defun echo-buffer-list ()
  "Return a list of buffers that are under `echo-directory'"
  (seq-filter 'echo-buffer-p (buffer-list)))

(provide 'echo)
;;; echo.el ends here
