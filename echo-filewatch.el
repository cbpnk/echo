;;; echo-filewatch.el --- watch file changes for echo  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;; copied from filenotify.el of GNU Emacs

;;; Code:

(require 'echo)

(defvar echo-filewatch-hook nil)

(defvar echo-filewatch--desc-alist nil)

(defvar echo-filewatch--pending-move-alist nil)

(defun echo-filewatch--get-move-desc (cookie)
  (when-let ((desc (cdr (assoc cookie echo-filewatch--pending-move-alist))))
    (setq echo-filewatch--pending-move-alist
          (assoc-delete-all cookie echo-filewatch--pending-move-alist))
    desc))

(defun echo-filewatch--get-move-cookie (desc)
  (when-let ((cookie
              (car (rassoc desc echo-filewatch--pending-move-alist))))
    (setq echo-filewatch--pending-move-alist
          (assoc-delete-all cookie echo-filewatch--pending-move-alist))
    cookie))

(defun echo-filewatch--get-desc (filename)
  (car (rassoc filename echo-filewatch--desc-alist)))

(defun echo-filewatch--inotify-callback (event)
  (let* ((desc (car event))
         (actions (cadr event))
         (filename
          (file-name-concat
           (file-name-directory (cdr (assoc desc echo-filewatch--desc-alist)))
           (file-name-nondirectory (caddr event))))
         (cookie (cadddr event)))
    (pcase-exhaustive actions
      ('(modify)
       (run-hook-with-args 'echo-filewatch-hook 'modified filename))
      ('(isdir create)
       (echo-filewatch--add-directory (file-name-as-directory filename)))
      ('(create)
       (echo-filewatch--add-file filename))
      ('(isdir delete-self)
       (echo-filewatch--remove-directory desc))
      ('(delete-self)
       (echo-filewatch--remove-file desc))
      ('(isdir move-self)
       (when (echo-filewatch--get-move-cookie desc)
         (echo-filewatch--remove-directory desc t)))
      ('(move-self)
       (when (echo-filewatch--get-move-cookie desc)
         (echo-filewatch--remove-file desc)))
      ('(isdir moved-from)
       (when-let ((desc (echo-filewatch--get-desc (file-name-as-directory filename))))
         (push (cons cookie desc) echo-filewatch--pending-move-alist)))
      ('(moved-from)
       (when-let ((desc (echo-filewatch--get-desc filename)))
         (push (cons cookie desc) echo-filewatch--pending-move-alist)))
      ('(isdir moved-to)
       (echo-filewatch--rename-directory (echo-filewatch--get-move-desc cookie) (file-name-as-directory filename)))
      ('(moved-to)
       (echo-filewatch--rename-file (echo-filewatch--get-move-desc cookie) filename)))))

(defun echo-filewatch--watch-file (filename)
  (inotify-add-watch
   filename
   '(delete-self modify move-self)
   'echo-filewatch--inotify-callback))

(defun echo-filewatch--watch-directory (filename)
  (inotify-add-watch
   filename
   '(create delete-self move-self move onlydir)
   'echo-filewatch--inotify-callback))

(defun echo-filewatch--add-file (filename)
  (when (echo-file-p filename)
    (let ((desc (echo-filewatch--watch-file filename)))
      (push (cons desc filename) echo-filewatch--desc-alist)
      (run-hook-with-args 'echo-filewatch-hook 'modified filename))))

(defun echo-filewatch--add-directory (dirname)
  (when (echo-file-p dirname)
    (let ((desc (echo-filewatch--watch-directory dirname)))
      (push (cons desc dirname) echo-filewatch--desc-alist)
      (pcase-dolist
          (`(,filename ,type . _)
           (directory-files-and-attributes dirname t echo--file-name-nodirectory-match-regexp t))
        (cond
         ((eq type 't)
          (echo-filewatch--add-directory (file-name-as-directory filename)))
         ((not (stringp type))
          (echo-filewatch--add-file filename)))))))

(defun echo-filewatch--rename-file (desc filename)
  (if desc
      (let ((pair (assoc desc echo-filewatch--desc-alist)))
        (if (not (echo-file-p filename))
            (echo-filewatch--remove-file desc)
          (run-hook-with-args 'echo-filewatch-hook 'renamed (cdr pair) filename)
          (setcdr pair filename)))
    (echo-filewatch--add-file filename)))

(defun echo-filewatch--rename-directory (desc filename)
  (if desc
      (let ((dirname (file-name-as-directory filename))
            (srcname (cdr (assoc desc echo-filewatch--desc-alist))))
        (if (not (echo-file-p dirname))
            (echo-filewatch--remove-directory desc t)
          (run-hook-with-args 'echo-filewatch-hook 'renamed srcname dirname)
          (setq echo-filewatch--desc-alist
                (mapcar
                 (lambda (pair)
                   (if (not (string-prefix-p srcname (cdr pair)))
                       pair
                     (cons
                      (car pair)
                      (file-name-concat
                       dirname
                       (string-remove-prefix srcname (cdr pair))))))
                 echo-filewatch--desc-alist))))
    (echo-filewatch--add-directory filename)))

(defun echo-filewatch--remove-file (desc)
  (when-let ((filename (cdr (assoc desc echo-filewatch--desc-alist))))
    (run-hook-with-args 'echo-filewatch-hook 'deleted filename)
    (setq echo-filewatch--desc-alist
          (assoc-delete-all desc echo-filewatch--desc-alist))
    (inotify-rm-watch desc)))

(defun echo-filewatch--remove-directory (desc &optional recursive)
  (cond
   (recursive
    (let ((dirname (cdr (assoc desc echo-filewatch--desc-alist))))
      (pcase-dolist (`(,desc . ,filename) echo-filewatch--desc-alist)
        (when (string-prefix-p dirname filename)
          (inotify-rm-watch desc)))
      (setq
       echo-filewatch--desc-alist
       (seq-remove
        (lambda (pair)
          (string-prefix-p dirname (cdr pair)))
        echo-filewatch--desc-alist))))
   (t
    (setq echo-filewatch--desc-alist
          (assoc-delete-all desc echo-filewatch--desc-alist))
    (inotify-rm-watch desc))))

(defun echo-filewatch--remove-all ()
  (pcase-dolist (`(,desc . ,_) echo-filewatch--desc-alist)
    (inotify-rm-watch desc)
    (setq echo-filewatch--desc-alist nil)))

;;;###autoload
(define-minor-mode echo-filewatch-mode
  "Global minor mode"
  :group 'echo
  :lighter " echo"
  :global t
  :init-value nil
  (cond
   (echo-filewatch-mode
    (echo-filewatch--add-directory (file-name-as-directory echo-directory)))
   (t
    (echo-filewatch--remove-all))))

(provide 'echo-filewatch)
;;; echo-filewatch.el ends here
