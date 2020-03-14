;;; maple-note.el ---  maple note configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-note

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; note mode configuration.
;;

;;; Code:
(require 'tabulated-list)
(require 's)

(defgroup maple-note nil
  "Major mode for writing note"
  :prefix "maple-note-"
  :group 'external)

(defcustom maple-note-buffer-name "*Note*"
  "Note major buffer name."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-header [("DATE"     16 t)
                              ("TITLE"    36 t)
                              ("PUBLISH"  16 t)
                              ("CATEGORY" 16 t)
                              ("TAGS"     0  nil)]
  "Note Header."
  :group 'maple-note
  :type 'vector)

(defcustom maple-note-sort-key '("DATE" . t)
  "Note sort key."
  :group 'maple-note
  :type '(alist :key-type (string) :value-type (boolean)))

(defcustom maple-note-filter-keyword nil
  "Global filter keywords."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-base-directory nil
  "Note base directory."
  :group 'maple-note
  :type 'list)

(defcustom maple-note-draft-directory "draft/"
  "Note draft directory."
  :group 'maple-note
  :type 'list)

(defcustom maple-note-publishing-directory
  '((org-mode . "org/")
    (markdow-mode . "markdown/"))
  "Note publishing directory."
  :group 'maple-note
  :type '(alist :key-type (symbol) :value-type (string)))

(defcustom maple-note-file-match "^[^.]*\\.\\(org\\|md\\|markdown\\)$"
  "Note scan file."
  :group 'maple-note
  :type 'string)

(defvar maple-note-md-template
  '("Title: %s"
    "Author: honmaple"
    "Date: %s"
    "Modified: %s"
    "Category:"
    "Tags:"
    "Slug: %s"
    "Summary: "))

(defvar maple-note-org-template
  '("#+TITLE: %s"
    "#+AUTHOR: honmaple"
    "#+DATE: %s"
    "#+CATEGORY:"
    "#+PROPERTY: MODIFIED %s"
    "#+PROPERTY: TAGS"
    "#+PROPERTY: SLUG %s"
    "#+PROPERTY: SUMMARY "))

(defcustom maple-note-template-alist
  `((org-mode . ,maple-note-org-template)
    (markdown-mode . ,maple-note-md-template))
  "Note template list."
  :group 'maple-note
  :type '(alist :key-type (symbol) :value-type (list)))

(defvar maple-note-md-meta
  '(:category
    "^Category:\\(.*\\)$"
    :title "^Title:\\(.*\\)$"
    :tags "^Tags:\\(.*\\)$"
    :date "^Date:\\(.*\\)$"))

(defvar maple-note-org-meta
  '(:category
    "^#\\+CATEGORY:\\(.*\\)$"
    :title "^#\\+TITLE:\\(.*\\)$"
    :tags "^#\\+PROPERTY: TAGS\\(.*\\)$"
    :date "^#\\+DATE:\\(.*\\)$"))

(defcustom maple-note-meta-alist
  `((org-mode . ,maple-note-org-meta)
    (markdown-mode . ,maple-note-md-meta))
  "Note meta list."
  :group 'maple-note
  :type '(alist :key-type (symbol) :value-type (list)))

(defcustom maple-note-read-alist nil
  "Note scan file."
  :group 'maple-note
  :type '(alist :key-type (symbol) :value-type (symbol)))

(defcustom maple-note-draft-alist
  '((t . maple-note-draft))
  "Note scan file."
  :group 'maple-note
  :type '(alist :key-type (symbol) :value-type (symbol)))

(defface maple-note-face
  '((t (:underline nil :inherit font-lock-keyword-face)))
  "Note text face.")

(defmacro maple-note-with (&rest body)
  "Execute the forms in BODY with maple note buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create maple-note-buffer-name) ,@body))

(defun maple-note-alist (file alist)
  "FILE ALIST."
  (let ((key (assoc-default file auto-mode-alist 'string-match)))
    (when key (cdr (or (and key (assq key alist)) (assq t alist))))))

(defun maple-note-abspath (path)
  "Return full absolute path base on PATH."
  (if (file-name-absolute-p path) path
    (expand-file-name path maple-note-base-directory)))

(defun maple-note-draft (file)
  "Check FILE is in drafts."
  (string-prefix-p
   (maple-note-abspath maple-note-draft-directory) file))

(defun maple-note--match(regex string)
  "Match REGEX STRING."
  (let ((match (s-match regex string)))
    (if match (s-trim (cadr match)) "")))

(defun maple-note--date (datestring &optional fstring)
  "Format DATESTRING FSTRING."
  (let ((date (apply 'encode-time (cl-loop for item in (parse-time-string (or datestring ""))
                                           for i from 1 to 6
                                           collect (or item 0)))))
    (if fstring (format-time-string fstring date) date)))

(defun maple-note--read (file)
  "Read FILE metadata."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string))
          (draft (maple-note-alist file maple-note-draft-alist)))
      (cl-destructuring-bind (&key category title tags date)
          (maple-note-alist file maple-note-meta-alist)
        (list :category (maple-note--match category content)
              :title (maple-note--match title content)
              :draft (when draft (funcall draft file))
              :tags  (maple-note--match tags content)
              :date  (maple-note--date (maple-note--match date content))
              :file  file)))))

(defun maple-note-read (file)
  "Read info of pelican FILE."
  (let ((read (maple-note-alist file maple-note-read-alist)))
    (if read (funcall read file)
      (maple-note--read file))))

(defun maple-note-entries (file)
  "Get entries of FILE."
  (let* ((info (maple-note-read file))
         (date (plist-get info :date))
         (tags (plist-get info :tags))
         (title (plist-get info :title))
         (draft (plist-get info :draft))
         (category (plist-get info :category)))
    (list file (vector
                `(,(format-time-string "%Y-%m-%d %H:%M" date)
                  face font-lock-comment-face
                  follow-link t
                  action (lambda(&optional _) (find-file (tabulated-list-get-id))))
                `(,title
                  face maple-note-face
                  follow-link t
                  action (lambda(&optional _) (find-file (tabulated-list-get-id))))
                `(,(if draft "YES" "NO")
                  face ,(if draft 'maple-note-face 'font-lock-comment-face)
                  action (lambda(&optional _) (maple-note-publish-or-unpublish)))
                `(,category
                  face maple-note-face
                  action (lambda(&optional _) (maple-note-refresh ,category)))
                `(,tags face maple-note-face)))))

(defun maple-note-file-list ()
  "Scan all files."
  (cl-loop
   for dir in (append (list (maple-note-abspath maple-note-draft-directory))
                      (cl-loop for dir in maple-note-publishing-directory
                               collect (maple-note-abspath (cdr dir))))
   append (directory-files (maple-note-abspath dir) t maple-note-file-match)))

(defun maple-note-publish-or-unpublish ()
  "Switch between publish and draft."
  (interactive)
  (let* ((point (point))
         (file  (tabulated-list-get-id))
         (draft (maple-note-alist file maple-note-draft-alist))
         (dest-path (if (funcall draft file)
                        (maple-note-alist file maple-note-publishing-directory)
                      (maple-note-abspath maple-note-draft-directory))))
    (when (file-exists-p file)
      (rename-file file (expand-file-name
                         (file-name-nondirectory file)
                         (maple-note-abspath dest-path)))
      (let ((buffer (find-buffer-visiting file)))
        (when buffer (kill-buffer buffer))))
    (maple-note-refresh) (goto-char point)))

(defun maple-note-create (filename)
  "New note with FILENAME."
  (interactive "sNote's filename(new-note.org, new-note.md etc):")
  (let ((temp (maple-note-alist filename maple-note-template-alist))
        (time (format-time-string "%F %T" (current-time))))
    (if (not (string-match maple-note-file-match filename))
        (message "Unsupported file extension!")
      (find-file (expand-file-name filename (maple-note-abspath maple-note-draft-directory)))
      (when temp (insert (format (string-join temp "\n") (file-name-base filename) time time (file-name-base filename)))))))

(defun maple-note-delete ()
  "Delete note."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (when (y-or-n-p (format "Do you really want to delete %s ? " file))
      (delete-file file t)
      (let ((buffer (find-buffer-visiting file)))
        (when buffer (kill-buffer buffer)))
      ;; remove asset directory if exist
      ;; (let ((dir-path (file-name-sans-extension file)))
      ;;   (if (file-exists-p dir-path)
      ;;       (delete-directory dir-path t)))
      (maple-note-refresh))))

(defun maple-note-refresh(&optional keyword remember-pos update)
  "Refresh with &optional KEYWORD REMEMBER-POS UPDATE."
  (interactive)
  (maple-note-with
    (let ((notes (mapcar 'maple-note-entries (maple-note-file-list)))
          (filter-keyword (or keyword maple-note-filter-keyword)))
      (when filter-keyword
        (setq notes (cl-remove-if-not
                     (lambda (x)
                       (let* ((row (append (car (cdr x)) nil))
                              (title (car (nth 1 row)))
                              (category (car (nth 3 row))))
                         (s-contains? keyword (mapconcat #'identity (list title category) "|") t)))
                     notes)))
      (setq tabulated-list-sort-key maple-note-sort-key)
      (setq tabulated-list-entries notes)
      (tabulated-list-print remember-pos update))))

(defun maple-note-filter()
  "Filter notes."
  (interactive)
  (maple-note-refresh (read-from-minibuffer "Search filter: ")))

(defvar maple-note-mode-map
  (let ((map (make-sparse-keymap))
        (keymap '(("q" . quit-window)
                  ("r" . maple-note-refresh)
                  ("s" . maple-note-publish-or-unpublish)
                  ("D" . maple-note-delete)
                  ("w" . maple-note-create)
                  ("f" . maple-note-filter)
                  ("G" . end-of-buffer)
                  ("l" . forward-char)
                  ("h" . backward-char)
                  ("A" . end-of-line)
                  ("I" . beginning-of-line)
                  ("gg" . beginning-of-buffer))))
    (set-keymap-parent map tabulated-list-mode-map)
    (dolist (key keymap) (define-key map (car key) (cdr key))) map)
  "Maple note mode map.")

(define-derived-mode maple-note-mode tabulated-list-mode "NOTE"
  "Major mode for writing note.
\\<maple-note-mode-map>
\\{maple-note-mode-map}"
  (maple-note-with
    (setq tabulated-list-format maple-note-header)
    (tabulated-list-init-header))
  (maple-note-refresh))

;;;###autoload
(defun maple-note()
  "Start note."
  (interactive)
  (maple-note-with (maple-note-mode))
  (switch-to-buffer maple-note-buffer-name))

(provide 'maple-note)
;;; maple-note.el ends here
