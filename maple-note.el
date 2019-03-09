;;; maple-note-mode.el ---  note mode configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

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
  :type 'list)

(defcustom maple-note-filter-keyword nil
  "Global filter keywords."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-root-path nil
  "Note root path."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-draft-path nil
  "Note draft path."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-path nil
  "Note path."
  :group 'maple-note
  :type 'list)

(defcustom maple-note-file "^[^.]*\\.\\(org\\|md\\|markdown\\)$"
  "Note scan file."
  :group 'maple-note
  :type 'string)

(defcustom maple-note-alist
  '(("markdown"
     :path "content/markdown"
     :read maple-note--read-md
     :template "Title: %s
Author: honmaple
Date: %s
Modified: %s
Category:
Tags:
Slug: %s
Summary: ")
    ("org-mode"
     :path "content/org"
     :read maple-note--read-org
     :template "#+TITLE: %s
#+AUTHOR: honmaple
#+DATE: %s
#+CATEGORY:
#+PROPERTY: MODIFIED %s
#+PROPERTY: TAGS
#+PROPERTY: SLUG %s
#+PROPERTY: SUMMARY "))
  "Note template list."
  :group 'maple-note
  :type '(list))

(defface maple-note-face
  '((t (:underline nil :inherit font-lock-keyword-face)))
  "Note text face.")

(defmacro maple-note--with-buffer (&rest body)
  "Execute the forms in BODY with maple note buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (get-buffer-create maple-note-buffer-name)))
     (with-current-buffer buffer
       ,@body)))

(defun maple-note--read-md (file)
  "Read info of markdown FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((title (s-chop-prefix "Title:" (car (s-match "^Title:.*?\n" (buffer-string)))))
          (category (s-chop-prefix "Category: " (car (s-match "^Category:.*?\n" (buffer-string)))))
          (tags (s-chop-prefix "Tags:" (car (s-match "^Tags:.*?\n" (buffer-string)))))
          (date (s-chop-prefix "Date:" (car (s-match "^Date:.*?\n" (buffer-string))))))
      (list :title (s-trim title)
            :category (s-trim category)
            :tags (s-trim tags)
            :date (maple-note--format-datetime (s-trim date))))))

(defun maple-note--read-org (file)
  "Read info of org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((title (s-chop-prefix "#+TITLE:" (car (s-match "^#\\+TITLE:.*?\n" (buffer-string)))))
          (category (s-chop-prefix "#+CATEGORY:" (car (s-match "^#\\+CATEGORY:.*?\n" (buffer-string)))))
          (tags (s-chop-prefix "#+PROPERTY: TAGS" (car (s-match "^#\\+PROPERTY: TAGS.*?\n" (buffer-string)))))
          (date (s-chop-prefix "#+DATE:" (car (s-match "^#\\+DATE:.*?\n" (buffer-string))))))
      (list :title (s-trim title)
            :category (s-trim category)
            :tags (s-trim tags)
            :date (maple-note--format-datetime (s-trim date))))))

(defun maple-note--read (filename)
  "Read info of pelican FILENAME."
  (let* ((args (maple-note--info filename))
         (info (funcall (plist-get args :read) filename)))
    (if (maple-note--is-draft filename)
        (plist-put info :publish "NO")
      (plist-put info :publish "YES"))))

(defun maple-note--info(file)
  "Check FILE path."
  (let ((e (file-name-extension file)))
    (cond ((string= e "md")
           (cdr (assoc "markdown" maple-note-alist)))
          ((string= e "org")
           (cdr (assoc "org-mode" maple-note-alist)))
          (t nil))))

(defun maple-note--entry (file)
  "Get info of FILE."
  (let* ((info (maple-note--read file))
         (title (plist-get info :title))
         (publish (plist-get info :publish))
         (category (plist-get info :category))
         (tags (plist-get info :tags))
         (date (plist-get info :date)))
    (list file (vector
                `(,date
                  face font-lock-comment-face
                  follow-link t
                  action (lambda(&optional _) (find-file (tabulated-list-get-id))))
                `(,title
                  face maple-note-face
                  follow-link t
                  action (lambda(&optional _) (find-file (tabulated-list-get-id))))
                `(,publish
                  face ,(if (string= publish "YES")
                            'font-lock-comment-face
                          'maple-note-face)
                  action (lambda(&optional _) (maple-note-publish-or-unpublish)))
                `(,category
                  face maple-note-face
                  action (lambda(&optional _) (maple-note-refresh ,category)))
                `(,tags
                  face maple-note-face)))))

(defun maple-note--files (&optional path)
  "Scan files of PATH."
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (maple-note--abspath append-path) t maple-note-file))
          (or path (append maple-note-path (list maple-note-draft-path))))))

(defun maple-note--format-datetime (datetime)
  "Format DATETIME."
  (let* ((datetime-in-plist
          (when (not (stringp datetime))
            (plist-get (plist-get (car datetime) 'timestamp) :raw-value)))
         (datetime-str (cond
                        ((eq datetime nil) "") ;; nil
                        ((stringp datetime-in-plist) datetime-in-plist)
                        ((stringp datetime) datetime)
                        (t (car datetime))))
         (l (parse-time-string datetime-str)))
    (if (equal l '(nil nil nil nil nil nil nil nil nil)) ;; can't parse
        "Can't Parse"
      (format-time-string "%Y-%m-%d %H:%M"
                          (encode-time 0 (or (nth 1 l) 0) (or (nth 2 l) 0) (nth 3 l) (nth 4 l) (nth 5 l))))))

(defun maple-note--is-draft (path)
  "Check PATH is in drafts."
  (s-starts-with?
   (maple-note--abspath maple-note-draft-path) path))

(defun maple-note--abspath (path)
  "Return full absolute path base on PATH."
  (expand-file-name
   (concat (file-name-as-directory maple-note-root-path) path)))

(defun maple-note-publish-or-unpublish ()
  "Switch between publish and draft."
  (interactive)
  (let* ((point (point))
         (path (tabulated-list-get-id))
         (info (maple-note--info path))
         (dest-path (if (maple-note--is-draft path)
                        (maple-note--abspath (plist-get info :path))
                      maple-note-draft-path)))
    (when (file-exists-p path)
      (rename-file path (expand-file-name
                         (file-name-nondirectory path)
                         (maple-note--abspath dest-path)))
      (let ((buffer (find-buffer-visiting path)))
        (when buffer (kill-buffer buffer))))
    (maple-note-refresh) (goto-char point)))

(defun maple-note-create (filename)
  "New note with FILENAME."
  (interactive "sNote's filename(new-note.org, new-note.md etc):")
  (let ((info (maple-note--info filename))
        (time (format-time-string "%F %T" (current-time))))
    (if (not info) (message "Unsupported file extension!")
      (find-file (expand-file-name filename (maple-note--abspath maple-note-draft-path)))
      (insert (format (plist-get info :template)
                      (file-name-base filename) time time
                      (file-name-base filename))))))

(defun maple-note-delete ()
  "Delete note."
  (interactive)
  (let ((file-path (tabulated-list-get-id)))
    (when (y-or-n-p (format "Do you really want to delete %s ? " file-path))
      (delete-file file-path t)
      (let ((buffer (find-buffer-visiting file-path)))
        (when buffer (kill-buffer buffer)))
      ;; remove asset directory if exist
      (let ((dir-path (file-name-sans-extension file-path)))
        (if (file-exists-p dir-path)
            (delete-directory dir-path t)))
      (maple-note-refresh))))

(defun maple-note-refresh(&optional keyword remember-pos update)
  "Refresh with &optional KEYWORD REMEMBER-POS UPDATE."
  (interactive)
  (maple-note--with-buffer
    (let ((notes (mapcar 'maple-note--entry (maple-note--files)))
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

(defun maple-note-init()
  "Init notes."
  (maple-note--with-buffer
    (dolist (arg maple-note-alist)
      (add-to-list 'maple-note-path (plist-get (cdr arg) :path)))
    (setq tabulated-list-format maple-note-header)
    (tabulated-list-init-header)))

(defvar maple-note-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'maple-note-refresh)
    (define-key map "s" 'maple-note-publish-or-unpublish)
    (define-key map "D" 'maple-note-delete)
    (define-key map "w" 'maple-note-create)
    (define-key map "f" 'maple-note-filter)
    (define-key map "gg" 'beginning-of-buffer)
    (define-key map "G" 'end-of-buffer)
    (define-key map "l" 'forward-char)
    (define-key map "h" 'backward-char)
    (define-key map "A" 'end-of-line)
    (define-key map "I" 'beginning-of-line)
    map)
  "Maple note mode map.")

(define-derived-mode maple-note-mode tabulated-list-mode "NOTE"
  "Major mode for writing note.
\\<maple-note-mode-map>
\\{maple-note-mode-map}"
  (maple-note-init)
  (maple-note-refresh))

;;;###autoload
(defun maple-note()
  "Start note."
  (interactive)
  (maple-note--with-buffer (maple-note-mode))
  (switch-to-buffer maple-note-buffer-name))

(provide 'maple-note)
;;; maple-note.el ends here
