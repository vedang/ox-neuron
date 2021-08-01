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
  '((:neuron-base-dir "NEURON_BASE_DIR" nil org-neuron-base-dir)
    (:with-drawers nil nil nil)))

(defun org-neuron-link (link desc info)
  "Convert LINK to Neuron Markdown format.

This function defers to `org-hugo-link' for everything other than
`id', `custom_id' and brain links (`brain' `brain-child'
`brain-parent', `brain-friend').

DESC is the link's description.
INFO is a plist used as a communication channel."
  (message "[org-neuron-link DBG] %s %s" link desc)
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

;; * This is a test element
;; :PROPERTIES:
;; :ID:       541a96fc-56ca-4011-8ca0-1baa3cd755bb
;; :END:
;; - [[brain-parent:4e18cf0b-0952-4074-9d3f-4f2497aab1e9][export org-brain notes to the neuron zettlekasten format]]
;; (org-neuron--get-post-name (org-element-at-point))
;; "541a96fc-56ca-4011-8ca0-1baa3cd755bb"
;; (org-neuron--get-post-name (org-element-at-point) :title)
;; "\"This is a test element\""

(defun org-neuron--valid-subtree (elem)
  "Return t if ELEM is a valid subtree, else nil."
  (org-element-property :ID elem))

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
             (valid (org-neuron--valid-subtree entry))
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

;; (org-neuron--get-valid-subtree)
;; "541a96fc-56ca-4011-8ca0-1baa3cd755bb"
(defun org-neuron--build-path (base-dir dir-paths)
  "Take the BASE-DIR and DIR-PATHS collected in processing an entry.

Build a path from BASE-DIR to this location using this information."
  (message "[org-neuron--build-path DBG] %s %s" base-dir dir-paths)
  (concat base-dir (mapconcat #'identity (reverse dir-paths) "/") "/"))

(defun org-neuron--get-entry-path (info)
  "Return the dir structure under which this Neuron post lives.

Return nil if a valid post subtree is not found. INFO is the
communication channel."
  (save-excursion
    (let* ((base-dir (if (plist-get info :neuron-base-dir)
                         (file-name-as-directory
                          (plist-get info :neuron-base-dir))
                       (user-error "It is mandatory to set the NEURON_BASE_DIR
property or the `org-neuron-base-dir' local variable")))
           (level (org-up-heading-safe))
           (dir-paths '()))
      (if level
          (catch 'break
            (while :infinite
              (let* ((entry (org-neuron--get-valid-subtree))
                     (fname (org-neuron--get-post-name entry :title)))
                (when (not entry)
                  (throw 'break (org-neuron--build-path base-dir dir-paths)))
                (setq dir-paths (append dir-paths (list fname)))
                ;; Keep on jumping to the parent heading if the current entry
                ;; does not have an EXPORT_FILE_NAME property or ID property.
                (setq level (org-up-heading-safe))
                ;; If all levels are exhausted, break
                (unless level
                  (throw 'break (org-neuron--build-path base-dir dir-paths))))))
        base-dir))))

;;;; Publication Directory
(defun org-neuron--get-pub-dir (info)
  "Return the post publication directory path.

The publication directory is created if it does not exist.

INFO is a plist used as a communication channel."
  (let* ((entry-path (org-neuron--get-entry-path info)))
    ;; Create the directory if it does not exist
    (make-directory entry-path :parents)
    (file-truename entry-path)))

(defun org-neuron-export-to-md (outfile &optional visible-only)
  "Export current buffer to a Neuron OUTFILE.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (message "[ox-neuron-export-to-md DBG] Starting")
  (org-hugo--before-export-function nil)
  ;; Allow certain `ox-hugo' properties to be inherited.  It is
  ;; important to set the `org-use-property-inheritance' before
  ;; setting the `info' var so that properties like
  ;; EXPORT_HUGO_SECTION get inherited.
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 'neuron t visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'neuron t))))
    (prog1
        (org-export-to-file 'neuron outfile nil t visible-only)
      (org-hugo--after-export-function info outfile))))

;; (let ((entry (org-element-at-point)))
;;   (concat (org-neuron--get-entry-path (plist-put '() :neuron-base-dir "../"))
;;           (org-neuron--get-post-name entry)
;;           ".md"))
;; ;; => "../18-parvans-the-index/e2bc89e2-5ad7-4d13-990c-f870d2f65b27.md"

(defvar org-neuron--seen-headings '()
  "Internal variable for the elem-to-id translation.")

(defun org-neuron--elem-to-id (menu-headline elem)
  "Given an ELEM, convert it to an ID/Brain link.

Attach the ID / Brain link to the MENU-HEADLINE, for use later.
SEEN-HEADINGS tracks sub-headings that have been processed. This
helps avoid processing of sub-sub-headings.

This is done only if the elem is a sub-heading. It is expected
that sub-headings will be exported into their own files."
  (message "[org-neuron--elem-to-id DBG]")
  ;; (message "[org-neuron--elem-to-id DBG] Elem: %s, Menu: %s"
  ;;          elem menu-headline)
  (let ((parent (org-element-property :parent elem)))
    ;; We only care about valid sub-headings
    (if (and (eq (org-element-type elem) 'headline)
             (eq (org-element-type parent) 'headline)
             (org-neuron--valid-subtree elem)
             (not (memq (org-element-property :ID parent)
                        org-neuron--seen-headings)))
        ;; Replace the sub-heading with a link to the appropriate entry.
        ;; Attach the new link object to the menu
        ;; Remove the sub-heading completely.
        (progn
          (message "[org-neuron--elem-to-id DBG] extracting %s %s"
                   (org-element-type elem)
                   (org-element-property :title elem))
          (org-element-adopt-elements
              menu-headline
            (org-element-create
             'paragraph
             (list :post-blank 1 :pre-blank 2)
             ;; Create a brain-child link to maintain the correct foggel links.
             ;; @TODO: Re-visit this to consider using simple links later.
             (org-element-create
              'link
              (list :type "brain-child" :path (org-element-property :ID elem)))))
          (org-element-extract-element elem)
          ;; Return empty so that return value is discarded.
          (setq org-neuron--seen-headings
                (cons (org-element-property :ID elem)
                      org-neuron--seen-headings))
          nil)
      (progn (message "[org-neuron--elem-to-id DBG] ignoring %s %s"
                      (org-element-type elem)
                      (org-element-property :title elem))
             elem))))

(defun org-neuron--get-pre-processed-buffer ()
  "Return a pre-processed copy of the current buffer.

Internal links to other subtrees are converted to external
links."
  ;; Narrow down to just this subtree
  (message "[org-neuron--get-pre-processed-buffer DBG]")
  (org-narrow-to-subtree)
  (let* ((buffer (generate-new-buffer (concat "*Ox-neuron Pre-processed "
                                              (buffer-name)
                                              " *")))
         ;; Create an abstract syntax tree (AST) of the Org document
         ;; in the current buffer.
         (ast (org-element-parse-buffer))
         (org-use-property-inheritance
          (org-hugo--selective-property-inheritance))
         (local-variables (buffer-local-variables))
         (bound-variables (org-export--list-bound-variables))
	     vars)
    (with-current-buffer buffer
      (let ((inhibit-modification-hooks t)
            (org-mode-hook nil)
            (org-inhibit-startup t))

        (org-mode)
        ;; Copy specific buffer local variables and variables set
        ;; through BIND keywords.
        (dolist (entry local-variables vars)
          (when (consp entry)
	        (let ((var (car entry))
	              (val (cdr entry)))
	          (and (not (memq var org-export-ignored-local-variables))
	               (or (memq var
			                 '(default-directory
			                    buffer-file-name
			                    buffer-file-coding-system))
		               (assq var bound-variables)
		               (string-match "^\\(org-\\|orgtbl-\\)"
				                     (symbol-name var)))
	               ;; Skip unreadable values, as they cannot be
	               ;; sent to external process.
	               (or (not val) (ignore-errors (read (format "%S" val))))
	               (push (set (make-local-variable var) val) vars)))))

        ;; Workaround to prevent exporting of empty special blocks.
        (org-element-map ast 'special-block
          (lambda (block)
            (when (null (org-element-contents block))
              (org-element-adopt-elements block ""))))

        ;; Convert all sub-headings into brain-children / IDs
        (let* ((main-headline (org-element-map ast 'headline
                                (lambda (hl)
                                  (when (not (eq (org-element-type
                                                  (org-element-property :parent hl))
                                                 'headline))
                                    hl))
                                nil t))
               (insert-location (org-element-map ast 'headline
                                  (lambda (hl)
                                    (when (eq (org-element-type
                                               (org-element-property :parent hl))
                                              'headline)
                                      hl))
                                  nil t))
               (menu-title "Children")
               (menu-headline (when insert-location
                                (org-element-create
                                 'headline
                                 (list :level (+ 1
                                                 (org-element-property
                                                  :level main-headline))
                                       :title menu-title
                                       :raw-value menu-title
                                       :pre-blank 1
                                       :post-blank 1)))))
          ;; (message "[ox-neuron--preprocessing DBG] Main: %s, Insert at: %s, Menu: %s"
          ;;          main-headline insert-location menu-headline)
          (when menu-headline
            (org-element-insert-before menu-headline insert-location)
            (setq org-neuron--seen-headings nil)
            (org-element-map ast 'headline
              (apply-partially #'org-neuron--elem-to-id menu-headline))))

        (message "[ox-neuron--preprocessing DBG] AST: %s" ast)
        ;; Turn the AST with updated links into an Org document.

        (insert (org-element-interpret-data ast))
        (set-buffer-modified-p nil)))
    ;; Return base buffer to it's original state and return the buffer
    (org-toggle-narrow-to-subtree)
    buffer))

(defun org-neuron--export-subtree (subtree &optional visible-only)
  "Given a SUBTREE, export it to an MD file.

Note: This is an internal function, use `org-neuron-export-wim-to-md'
instead.

VISIBLE-ONLY controls whether to include hidden elements or not."
  (message "[ox-neuron--export-subtree DBG] Subtree: %s, Starting"
           (org-element-property :title subtree))
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 'neuron t visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'neuron t)))
         (exclude-tags (plist-get info :exclude-tags))
         (is-commented (org-element-property :commentedp subtree))
         (title (org-element-property :title subtree))
         is-excluded matched-exclude-tag ret)
    (message "[ox-neuron--export-subtree DBG] Subtree: %s, Info built"
             title)
    (let ((all-tags (let ((org-use-tag-inheritance t))
                      (org-hugo--get-tags))))
      (when all-tags
        (dolist (exclude-tag exclude-tags)
          (when (member exclude-tag all-tags)
            (setq matched-exclude-tag exclude-tag)
            (setq is-excluded t)))))
    (message "[ox-neuron--export-subtree-to-md DBG] Subtree: %s, Tags Built"
             title)
    (cond
     (is-commented
      (message "[ox-neuron--export-subtree-to-md] `%s' was not exported (commented out)"
               title))
     (is-excluded
      (message "[ox-neuron--export-subtree-to-md] `%s' was not exported (exclude tag `%s')"
               title
               matched-exclude-tag))
     (t
      (message "[ox-neuron--export-subtree-to-md] Exporting `%s', Starting"
               title)
      (let* ((entry (org-element-at-point))
             (org-use-property-inheritance
              (org-hugo--selective-property-inheritance))
             (info (org-combine-plists
                    (org-export--get-export-attributes
                     'neuron t visible-only)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'neuron t)))
             (pub-dir (org-neuron--get-pub-dir info))
             (outfile (concat pub-dir (org-neuron--get-post-name entry) ".md"))
             (buffer (org-neuron--get-pre-processed-buffer)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (setq ret (org-neuron-export-to-md outfile visible-only)))
        (kill-buffer buffer))))
    ;; (message "[ox-neuron--export-subtree-to-md DBG] Subtree: %s, Returning"
    ;;          title)
    ret))

(defun org-neuron--export-subtree-to-md (&optional visible-only)
  "Export the current subtree to a Hugo post.

Note: This is an internal function, use
`org-neuron-export-wim-to-md' instead.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

- If point is under a valid post subtree, export it, and
  also return the exported file name.

- If point is not under a valid post subtree, but one exists
  elsewhere in the Org file, do not export anything, but still
  return t.

- Else, return nil."
  ;; Publish only the current subtree
  (ignore-errors
    (org-back-to-heading :invisible-ok))
  (let ((subtree (org-neuron--get-valid-subtree)))
    (if subtree
        ;; If subtree is a valid post subtree, proceed ..
        (org-neuron--export-subtree subtree visible-only)

      ;; If the point is not in a valid subtree, check if there's a
      ;; valid subtree elsewhere in the same Org file.
      (let ((valid-subtree-found
             (catch 'break
               (org-map-entries
                (lambda ()
                  (throw 'break t))
                ;; Only map through subtrees where ID property is not
                ;; empty.
                "ID<>\"\""))))
        (message "[ox-neuron--export-subtree-to-md DBG] Not in valid Subtree")
        (when valid-subtree-found
          (message "Point is not in a valid Neuron subtree; move to one and try again"))
        valid-subtree-found))))

(defun org-neuron-export-wim-to-md
    (&optional visible-only)
  "Export the current subtree/all subtrees/current file to Neuron posts.

This is an Export \"What I Mean\" function:

- If the current subtree has the ID property, export that
  subtree.

- If the current subtree doesn't have this properties, but one
  of its parent subtrees has, then export from that subtree's
  scope.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (interactive "P")
  (message "[ox-neuron-export-wim-to-md DBG] Starting %s" visible-only)
  (let (ret)
    (save-window-excursion
      (save-restriction
        (widen)
        (save-excursion
          (setq ret (org-neuron--export-subtree-to-md visible-only)))))
    ret))

(provide 'ox-neuron)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-neuron.el ends here
