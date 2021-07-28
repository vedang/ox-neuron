;;; ox-neuron.el --- Neuron Zettel Markdown back-end for Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Vedang Manerikar

;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Keywords: org, wp, markdown, zettelkasten
;; Package-Requires: ((emacs "24.4") (org "9.0") (ox-hugo "0.7"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ox-neuron implements a Markdown back-end for Org exporter.
;; The exported Markdown is compatible with the Neuron Zettelkasten
;; note-taking static site generator (https://neuron.zettel.page/).
;; This exporter also handles `org-brain' relationships in notes
;; (https://github.com/Kungsgeten/org-brain).

;; This code builds on top of the `ox-hugo' exporter for the Hugo
;; static site generator (https://ox-hugo.scripter.co) and uses a lot
;; of it's code as-is.

;;; Code:

(require 'ox-hugo)

(defgroup org-export-neuron nil
  "Options for exporting Org mode files to Neuron."
  :tag "Org Export Neuron"
  :group 'org-export
  :version "25.2")

(defcustom org-neuron-base-dir nil
  "Base directory for Neuron content.

All Neuron markdown content will be generated into this
directory. Set either this value, or the NEURON_BASE_DIR global
property for export."
  :group 'org-export-neuron
  :type 'directory)
;;;###autoload (put 'org-neuron-base-dir 'safe-local-variable 'stringp)

(setq org-hugo-front-matter-format "yaml"
      org-hugo-allow-spaces-in-tags nil
      org-hugo-date-format "%Y-%m-%dT%T")

;;neuron < hugo < blackfriday < md < html
(org-export-define-derived-backend 'neuron 'hugo
  :menu-entry
  '(?N "Export to Neuron-compatible Markdown"
       ((?N "Subtree to Neuron Md files"
            (lambda (_a _s v _b)
              (org-neuron-export-wim-to-md v)))))
  :translate-alist
  '((link . org-neuron-link))
  :options-alist
  '((:neuron-base-dir "NEURON_BASE_DIR" nil org-neuron-base-dir)))


(defun org-neuron-link (link desc info)
  "Convert LINK to Neuron Markdown format.

This function defers to `org-hugo-link' for everything other than
`id', `custom_id' and brain links (`brain' `brain-child'
`brain-parent', `brain-friend').

DESC is the link's description.
INFO is a plist used as a communication channel."
  (let* ((type (org-element-property :type link)))
    (if (member type '("custom-id" "id" "brain" "brain-child" "brain-parent" "brain-friend"))
        (let ((destination (org-element-property :path link)))
          (pcase (org-element-property :type link)
            ("brain-child"
             (if desc
                 (format "[[%s|%s]]#" destination desc)
               (format "[[%s]]#" destination)))
            ("brain-parent"
             (if desc
                 (format "#[[%s|%s]]" destination desc)
               (format "#[[%s]]" destination)))
            (_
             (if desc
                 (format "[[%s|%s]]" destination desc)
               (format "[[%s]]" destination)))))
      (org-hugo-link link desc info))))

;; (org-element-link-parser)
;; (link (:type "brain-parent" :path
;;  "4e18cf0b-0952-4074-9d3f-4f2497aab1e9" :format bracket :raw-link
;;  "brain-parent:4e18cf0b-0952-4074-9d3f-4f2497aab1e9" :application
;;  nil :search-option nil :begin 1 :end 112 :contents-begin 54
;;  :contents-end 110 :post-blank 0))

;; (defvar *testlink2 (org-element-link-parser))
;; *testlink2
;; (org-neuron-link *testlink2 nil nil)
;; "#[[4e18cf0b-0952-4074-9d3f-4f2497aab1e9]]"
;; (org-neuron-link *testlink2 "Explore something" nil)
;; "#[[4e18cf0b-0952-4074-9d3f-4f2497aab1e9|Explore something]]"

(defun org-neuron--get-post-name (entry &optional title)
  "Return the file-name for ENTRY Neuron post.

If the optional arg TITLE is non-nil, return the :title property
of this ENTRY. This is useful to give meaningful directory names
when exporting posts."
  (or (when title
        (org-string-nw-p (org-element-property :title entry)))
      (org-string-nw-p (org-element-property :EXPORT_FILE_NAME entry))
      (org-element-property :ID entry)))

(defun org-neuron--get-valid-subtree ()
  "Return the Org element for a valid Neuron post subtree.
The condition to check validity is that the ID property is
defined for the subtree element.

As this function is intended to be called inside a valid Hugo
post subtree, doing so also moves the point to the beginning of
the heading of that subtree.

Return nil if a valid Hugo post subtree is not found.  The point
will be moved in this case too."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (valid (org-element-property :ID entry))
             (fname (org-neuron--get-post-name entry))
             level)
        (when (and valid fname)
          (throw 'break entry))
        ;; Keep on jumping to the parent heading if the current entry
        ;; does not have an ID property.
        (setq level (org-up-heading-safe))
        ;; If no more parent heading exists, break out of the loop
        ;; and return nil
        (unless level
          (throw 'break nil))))))

;; * This is a test element
;; :PROPERTIES:
;; :ID:       541a96fc-56ca-4011-8ca0-1baa3cd755bb
;; :END:
;; - [[brain-parent:4e18cf0b-0952-4074-9d3f-4f2497aab1e9][export org-brain notes to the neuron zettlekasten format]]
;; (org-element-property :ID (org-element-at-point))
;; "541a96fc-56ca-4011-8ca0-1baa3cd755bb"

(defun org-neuron--get-entry-path (info)
  "Return the dir structure under which this Neuron post lives.

As this function is intended to be called inside a valid Neuron
post subtree, doing so also moves the point to the beginning of
the heading of that subtree.

Return nil if a valid post subtree is not found. The point will
be moved in this case too.

INFO is the communication channel."
  (let* ((base-dir (if (plist-get info :neuron-base-dir)
                       (file-name-as-directory
                        (plist-get info :neuron-base-dir))
                     (user-error "It is mandatory to set the NEURON_BASE_DIR
property or the `org-neuron-base-dir' local variable")))
         (level (org-up-heading-safe))
         (dir-path base-dir))
    (if level
        (catch 'break
          (while :infinite
            (let* ((entry (org-neuron--get-valid-subtree))
                   (fname (org-neuron--get-post-name entry :title)))
              (when (not entry)
                (throw 'break dir-path))
              (setq dir-path (concat dir-path fname "/"))
              ;; Keep on jumping to the parent heading if the current entry
              ;; does not have an EXPORT_FILE_NAME property or ID property.
              (setq level (org-up-heading-safe))
              ;; If all levels are exhausted, break
              (unless level
                (throw 'break dir-path)))))
      dir-path)))

;;;; Publication Directory
(defun org-neuron--get-pub-dir (info)
  "Return the post publication directory path.

The publication directory is created if it does not exist.

INFO is a plist used as a communication channel."
  (let* ((entry-path (org-neuron--get-entry-path info)))
    ;; Create the directory if it does not exist
    (make-directory entry-path :parents)
    (file-truename entry-path)))

(provide 'ox-neuron)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-neuron.el ends here
