;;; echo-db.el --- Echo database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides the underlying database API to Echo.
;; copied from org-roam-db.el of Org-Roam

;;; Code:

(defcustom echo-db-location nil
  "The path to file where the Echo database is stored."
  :type 'string
  :group 'echo)

(defcustom echo-db-extra-links-elements '(node-property keyword)
  "The list of Org element types to include for parsing by Echo.
By default, when parsing Org's AST, links within keywords and
property drawers are not parsed as links. Sometimes however, it
is desirable to parse and cache these links (e.g. hiding links in
a property drawer)."
  :group 'echo
  :type '(set (const :tag "keywords" keyword)
              (const :tag "property drawers" node-property)))

(defvar echo-db--connection nil)

;;;###autoload
(defun echo-db ()
  "Entrypoint to the Echo sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless echo-db--connection
    (let ((conn (sqlite-open echo-db-location)))
      (echo-db--init conn)
      (setq echo-db--connection conn)))
  echo-db--connection)

(defun echo-db--init (conn)
  (sqlite-pragma conn "foreign_keys = true")

  (with-sqlite-transaction conn
    (sqlite-execute
     conn
     "
CREATE TABLE IF NOT EXISTS file (
  file_id INTEGER PRIMARY KEY AUTOINCREMENT,
  path TEXT UNIQUE,
  hash TEXT NOT NULL,
  mtime INTEGER NOT NULL,
  properties JSON
)")
    (sqlite-execute
     conn
     "
CREATE TABLE IF NOT EXISTS node (
  node_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER REFERENCES file(file_id) ON DELETE CASCADE,
  point INTEGER,
  level INTEGER,
  outline_path JSON,
  properties JSON
)")
    (sqlite-execute
     conn
"
CREATE TABLE IF NOT EXISTS link (
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  property TEXT,
  point INTEGER,
  type TEXT NOT NULL,
  path TEXT NOT NULL
)")
    (sqlite-execute
     conn
     "
CREATE TEMP TABLE IF NOT EXISTS file (
  file_id INTEGER PRIMARY KEY AUTOINCREMENT,
  path TEXT UNIQUE,
  modified_tick INTEGER,
  properties JSON
)")
    (sqlite-execute
     conn
"
CREATE TEMP TABLE IF NOT EXISTS node (
  node_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER REFERENCES file(file_id) ON DELETE CASCADE,
  point INTEGER,
  level INTEGER,
  outline_path JSON,
  properties JSON
)
")
    (sqlite-execute
     conn
"
CREATE TEMP TABLE IF NOT EXISTS link (
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  property TEXT,
  point INTEGER,
  type TEXT NOT NULL,
  path TEXT NOT NULL
)")))

;;;###autoload
(defun echo-db-clear-all ()
  (interactive)
  (when echo-db--connection
    (sqlite-close echo-db--connection)
    (when echo-db-location
      (delete-file echo-db-location))
    (setq echo-db--connection nil)))

(defun echo-db--file-hash (file-path)
  "Compute the hash of FILE-PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'sha256 (current-buffer))))

(defun echo-db--get-special-properties (pom)
  (let (props
        (element (org-element-at-point)))
    (when-let ((category (org-entry-get-with-inheritance "CATEGORY")))
      (push (cons "CATEGORY" category) props))
    (when (org-entry-blocked-p)
      (push (cons "BLOCKED" 1) props))
    (when-let ((tags (org-get-tags nil t)))
      (push (cons "TAGS" tags) props))
    (when-let ((tags (org-get-tags)))
      (push (cons "ALLTAGS" tags) props))
    (when-let ((todo (org-element-property :todo-keyword element)))
      (push (cons "TODO" todo) props))
    (when-let ((priority (org-element-property :priority element)))
      (push (cons "PRIORITY" (char-to-string priority)) props))
    (dolist (property '("SCHEDULED" "DEADLINE" "CLOSED"))
      (when-let ((time (org-entry-get pom property)))
        (push (cons property (org-time-string-to-seconds time)) props)))
    props))

(defun echo-db--insert-file (conn)
  (when-let
      ((result
        (sqlite-select
         conn
         "
INSERT INTO main.file(path, hash, mtime, properties)
VALUES (?, ?, ?, ?)
ON CONFLICT
DO UPDATE
SET hash = EXCLUDED.hash
  , mtime = EXCLUDED.mtime
  , properties = EXCLUDED.properties
WHERE hash != EXCLUDED.hash
RETURNING file_id"
         (list
          (file-relative-name buffer-file-name echo-directory)
          (echo-db--file-hash buffer-file-name)
          (car
           (time-convert
            (file-attribute-modification-time (file-attributes buffer-file-name))
            1000000000))
          (when-let ((props (org-collect-keywords echo-collect-keywords echo-collect-keywords)))
            (json-encode props))))))
    (caar result)))

(defun echo-db--insert-buffer (conn)
  (when-let
      ((result
        (sqlite-select
         conn
         "
INSERT INTO temp.file(path, modified_tick, properties)
VALUES (?, ?, ?)
ON CONFLICT
DO UPDATE
SET modified_tick = EXCLUDED.modified_tick
  , properties = EXCLUDED.properties
WHERE modified_tick < EXCLUDED.modified_tick
RETURNING file_id"
         (list
          (file-relative-name buffer-file-name echo-directory)
          (buffer-chars-modified-tick)
          (when-let ((props (org-collect-keywords echo-collect-keywords echo-collect-keywords)))
            (json-encode props))))))
    (caar result)))

(defun echo-db--insert-node (conn database file-id pom)
  (let ((props (append
                (echo-db--get-special-properties pom)
                (assoc-delete-all "CATEGORY" (org-entry-properties pom 'standard))))
        (olp (unless (org-before-first-heading-p) (org-get-outline-path t nil))))
    (sqlite-execute
     conn
     (format "INSERT INTO %s.node(file_id, point, level, outline_path, properties) VALUES (?, ?, ?, ?, ?)" database)
     (list
      file-id
      pom
      (org-outline-level)
      (when olp (json-encode olp))
      (when props (json-encode props))))))

(defun echo-db--insert-link (conn database file-id element)
  (let* ((prop
          (when (not (eq 'link (org-element-type element)))
            (org-element-property :key element)))
         (link
          (if prop
              (save-match-data (org-element-link-parser))
            element)))
    (sqlite-execute
     conn
     (format "
INSERT INTO %s.link(node_id, property, point, type, path)
SELECT node_id, ?, ?, ?, ?
FROM %s.node
WHERE file_id = ?
AND point < ?
ORDER BY point DESC
LIMIT 1
" database database)
     (list
      prop
      (org-element-property :begin link)
      (org-element-property :type element)
      (org-element-property :path element)
      file-id
      (org-element-property :begin link)))))

(defun echo-db--map-links (fun)
  "Call FUN for every links in current buffer."
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil :no-error)
    (goto-char (match-beginning 0))
    (save-match-data
      (let* ((element (org-element-context))
             (type (org-element-type element))
             (link
              (cond
               ((eq type 'link)
                element)
               ;; Links in property drawers and lines starting with #+. Recall that, as for Org Mode v9.4.4, the
               ;; org-element-type of links within properties drawers is "node-property" and for lines starting with
               ;; #+ is "keyword".
               ((member type echo-db-extra-links-elements)
                (org-element-link-parser)))))
        (funcall fun link)))
    (goto-char (match-end 0))))

(defun echo-db--update-nodes-and-links (conn database file-id)
  (sqlite-execute
   conn
   (format "DELETE FROM %s.node WHERE file_id = ?" database)
   (list file-id))
  (org-map-region
   (lambda ()
     (echo-db--insert-node conn database file-id (point)))
   (point-min) (point-max))
  (echo-db--map-links
   (lambda (link)
     (echo-db--insert-link conn database file-id link))))

(defun echo-db--update-file (conn abs-path)
  "Update echo cache for ABS-PATH."
  (with-temp-buffer
    (let ((buffer-file-name abs-path)
          (default-directory (file-name-directory abs-path)))
      (insert-file-contents abs-path)
      (delay-mode-hooks (org-mode))
      (org-with-wide-buffer
       (when-let ((file-id (echo-db--insert-file conn)))
         (echo-db--update-nodes-and-links conn "main" file-id))))))

(defun echo-db--add-directory (conn abs-path)
  "Add files under ABS-PATH to echo cache"
  (when (echo-file-p abs-path)
    (pcase-dolist
        (`(,filename ,type . _)
         (directory-files-and-attributes abs-path t echo--file-name-nodirectory-match-regexp t))
      (cond
       ((eq type 't)
        (echo-db--add-directory conn (file-name-as-directory filename)))
       ((not (stringp type))
        (when (echo-file-p filename)
          (echo-db--update-file conn filename)))))))

(defun echo-db--delete-file (conn abs-path)
  (sqlite-execute
   conn
   "DELETE FROM file WHERE path = ?"
   (list (file-relative-name abs-path echo-directory))))

(defun echo-db--rename-file (conn old-abs-path new-abs-path)
  (let* ((old-path (file-relative-name old-abs-path echo-directory))
         (old-length (length old-path))
         (new-path (file-relative-name new-abs-path echo-directory)))
    (sqlite-execute
     conn
     "UPDATE file SET path = ? || substr(path, ? + 1) WHERE substr(path, 1, ?) = ?"
     (list new-path old-length old-length old-path))))

;;;###autoload
(defun echo-db-add-file (abs-path)
  (let ((conn (echo-db)))
    (with-sqlite-transaction conn
      (if (directory-name-p abs-path)
          (echo-db--add-directory conn abs-path)
        (echo-db--update-file conn abs-path)))))

;;;###autoload
(defun echo-db-update-file (abs-path)
  (let ((conn (echo-db)))
    (with-sqlite-transaction conn
      (echo-db--update-file conn abs-path))))

;;;###autoload
(defun echo-db-delete-file (abs-path)
  (let ((conn (echo-db)))
    (with-sqlite-transaction conn
      (echo-db--delete-file conn abs-path))))

;;;###autoload
(defun echo-db-rename-file (old-abs-path new-abs-path)
  (let ((conn (echo-db)))
    (with-sqlite-transaction conn
      (echo-db--rename-file conn old-abs-path new-abs-path))))

;;;###autoload
(defun echo-db-refresh (&optional file-list)
  (interactive)
  (let* ((abs-path-list
          (seq-filter
           'echo-file-p
           (seq-uniq (mapcar 'expand-file-name file-list))))
         (buffer-list
          (if file-list
              (seq-filter 'identity (mapcar 'find-buffer-visiting abs-path-list))
            (echo-buffer-list)))
         (buffer-file-list
          (mapcar
           'buffer-file-name
           (seq-filter
            (lambda (buffer)
              (and (buffer-modified-p buffer)
                   (local-variable-p 'echo-db--buffer-visited buffer)))
            buffer-list)))
         (conn (echo-db)))
    (with-sqlite-transaction conn
      (let ((default-directory echo-directory))
        (if file-list
            (when-let ((killed-list (seq-difference abs-path-list buffer-file-list)))
              (sqlite-execute
               conn
               "DELETE FROM temp.file WHERE path IN (SELECT value FROM json_each(?))"
               (list
                (json-encode
                 (mapcar 'file-relative-name killed-list)))))
          (if buffer-file-list
              (sqlite-execute
               conn
               "DELETE FROM temp.file WHERE path NOT IN (SELECT value FROM json_each(?))"
               (list
                (json-encode
                 (mapcar 'file-relative-name buffer-file-list))))
            (sqlite-execute
             conn
             "DELETE FROM temp.file"))))
      (dolist (buffer buffer-list)
        (when (buffer-modified-p buffer)
          (with-current-buffer buffer
            (make-local-variable 'echo-db--buffer-visited)
            (org-with-wide-buffer
             (when-let ((file-id (echo-db--insert-buffer conn)))
               (echo-db--update-nodes-and-links conn "temp" file-id)))))))))

;;;###autoload
(defun echo-db-sync ()
  "Synchronize the cache state with the current Org files on-disk.

Do nothing if file watch mode enabled"
  (interactive)
  (unless (bound-and-true-p echo-filewatch-mode)
    (echo-db-add-file (file-name-as-directory echo-directory))))

(defun echo-db--on-file-change (event abs-path &optional abs-path1)
  (let ((conn (echo-db)))
    (with-sqlite-transaction conn
      (pcase-exhaustive event
        ('modified
         (echo-db--update-file conn abs-path))
        ('deleted
         (echo-db--delete-file conn abs-path))
        ('renamed
         (echo-db--rename-file conn abs-path abs-path1))))))

(defun echo-db--on-watch-toggled ()
  (when echo-filewatch-mode
    (when-let
        ((files
          (seq-filter
           (lambda (x)
             (not (directory-name-p x)))
           (mapcar
            (lambda (x)
              (file-relative-name (cdr x) echo-directory))
            echo-filewatch--desc-alist))))
      (let ((conn (echo-db)))
        (with-sqlite-transaction conn
          (sqlite-execute
           conn
           "DELETE FROM file WHERE path NOT IN (SELECT value FROM json_each(?))"
           (list (json-encode files))))))))

(add-hook 'echo-filewatch-hook 'echo-db--on-file-change)

(add-hook 'echo-filewatch-mode-hook 'echo-db--on-watch-toggled)

(provide 'echo-db)
;;; echo-db.el ends here
