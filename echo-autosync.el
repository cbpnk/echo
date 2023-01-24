;;; echo-autosync.el -- sync changes on file-save -*- coding: utf-8; lexical-binding: t; -*-

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

;; copied from org-roam-db.el of Org-Roam

;;; Code:


;;;###autoload
(define-minor-mode echo-autosync-mode
  "Global minor mode to keep your Echo session automatically synchronized.
Through the session this will continue to setup your buffers,
keep track of the related changes, maintain cache consistency and
incrementally update the currently active database.

If you need to manually trigger resync of the currently active
database, see `echo-db-sync' command."
  :group 'echo
  :global t
  :init-value nil
  (cond
   (echo-autosync-mode
    (add-hook 'find-file-hook 'echo-autosync--setup-file-h)
    (advice-add 'rename-file :after 'echo-autosync--rename-file-a)
    (advice-add 'delete-file :before 'echo-autosync--delete-file-a)
    (dolist (buf (echo-buffer-list))
      (with-current-buffer buf
        (add-hook 'after-save-hook 'echo-autosync--update-on-save-h nil t)))
    (echo-db-sync))
   (t
    (advice-remove 'delete-file 'echo-autosync--delete-file-a)
    (advice-remove 'rename-file 'echo-autosync--rename-file-a)
    (remove-hook 'find-file-hook 'echo-autosync--setup-file-h)
    (dolist (buf (echo-buffer-list))
      (with-current-buffer buf
        (remove-hook 'after-save-hook 'echo-autosync--update-on-save-h t))))))

(defun echo-autosync--delete-file-a (file &optional _trash)
  "Maintain cache consistency when file deletes."
  (let ((abs-path (expand-file-name file)))
    (when (echo-file-p abs-path)
      (echo-db-delete-file abs-path))))

(defun echo-autosync--rename-file-a (old-file new-file-or-dir &rest _args)
  "Maintain cache consistency of file rename."
  (let* ((old-abs-path
          (expand-file-name
           (if (file-directory-p old-file)
               (file-name-as-directory (directory-file-name old-file))
             (directory-file-name old-file))))
         (old-file-name
          (if (directory-name-p old-abs-path)
              (file-name-as-directory (file-name-nondirectory (directory-file-name old-abs-path)))
            (file-name-nondirectory (directory-file-name old-abs-path))))
         (new-abs-path
          (expand-file-name
           (if (directory-name-p new-file-or-dir)
               (file-name-concat new-file-or-dir old-file-name)
             new-file-or-dir))))
    (if (echo-file-p old-abs-path)
        (if (echo-file-p new-abs-path)
            (echo-db-rename-file old-abs-path new-abs-path)
          (echo-db-delete-file old-abs-path))
      (when (echo-file-p new-abs-path)
        (echo-db-add-file new-abs-path)))))

(defun echo-autosync--setup-file-h ()
  "Setup the current buffer if it visits an Org file under echo-directory"
  (when (echo-buffer-p)
    (add-hook 'after-save-hook 'echo-autosync--update-on-save-h nil t)))

(defun echo-autosync--update-on-save-h ()
  "Update the database for the current file after saving buffer."
  (echo-db-update-file))

(provide 'echo-autosync)
;;; echo-autosync.el ends here
