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
;;; @TODO: Figure out how to make these changes local to the Neuron
;;; export.
(setq org-hugo-front-matter-format "yaml"
      org-hugo-allow-spaces-in-tags nil
      org-hugo-date-format "%Y-%m-%dT%T")


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

(defcustom org-neuron-insert-subheadings-as-children nil
  "When non-nil, create a sub-heading called Children under the current post.

Collect links to all child posts under this subheading. When nil,
such a sub-heading is not created and instead we depend on
Neuron's dirtree plugin to show us the correct links."
  :group 'org-export-neuron
  :type 'boolean)

;;neuron < hugo < blackfriday < md < html
(org-export-define-derived-backend 'neuron 'hugo
  :menu-entry
  '(?N "Export to Neuron-compatible Markdown"
       ((?N "Subtree to Neuron Md files"
            (lambda (_a _s v _b)
              (org-neuron-export-wim-to-md v)))
        (?n "Entire File to Neuron Md files"
            (lambda (_a _s v _b)
              (org-neuron-export-file-to-md v)))))
  :translate-alist
  '((link . org-neuron-link))
  :options-alist
  '((:neuron-base-dir "NEURON_BASE_DIR" nil org-neuron-base-dir t)
    (:hugo-base-dir "NEURON_BASE_DIR" nil org-neuron-base-dir t)
    (:with-drawers nil nil nil t)
    ;; @TODO: Add front-matter support for NEURON_DIRTREE_DISPLAY
    (:neuron-dirtree-display "NEURON_DIRTREE_DISPLAY" nil t t)))

(defun org-neuron--zettel-markup (ztype zid zdesc)
  "Based on ZTYPE, convert ZID and ZDESC into Markdown expected by Neuron."
  (if zdesc
      (pcase ztype
        (:child (format "[[%s|%s]]#" zid zdesc))
        (:parent (format "#[[%s|%s]]" zid zdesc))
        (:sibling (format "[[%s|%s]]" zid zdesc)))
    (pcase ztype
      (:child (format "[[%s]]#" zid))
      (:parent (format "#[[%s]]" zid))
      (:sibling (format "[[%s]]" zid)))))

(defun org-neuron--zettel-id (id)
  "Given an ID, return the zettle-id we will use to as filename."
  (string-limit id 8))

(defun org-neuron--id-link (id-link desc)
  "Convert an ID-LINK and DESC into Neuron Markdown format.

An ID-LINK is a link to some other Org Mode content."
  (let* ((type (org-element-property :type id-link))
         (zid (if (equal type "custom-id")
                  (org-element-property :path id-link)
                (org-neuron--zettel-id
                 (org-element-property :path id-link)))))
    (pcase type
      ("brain-child" (org-neuron--zettel-markup :child zid desc))
      ("brain-parent" (org-neuron--zettel-markup :parent zid desc))
      (_ (org-neuron--zettel-markup :sibling zid desc)))))

(defun org-neuron--image-link (image-link info)
  "Convert IMAGE-LINK, INFO into Neuron Markdown format.

Copy the necessary resources into Neuron's static folder. This is
directly taken from Hugo, with modifications for Neuron output.
Neuron only supports linking to files in the static folder, and
this is all I am supporting right now."
  (let* ((raw-path (org-element-property :path image-link))
         (parent (org-export-get-parent image-link))
         (parent-type (org-element-type parent))
         ;; If this is a hyper-linked image, it's parent type will
         ;; be a link too. Get the parent of *that* link in that
         ;; case.
         (grand-parent (when (eq parent-type 'link)
                         (org-export-get-parent parent)))
         (useful-parent (if grand-parent grand-parent parent))
         (attr (org-export-read-attribute :attr_html useful-parent))
         (path (org-hugo--attachment-rewrite-maybe raw-path info))
         (alt-text (if (plist-get attr :alt) (plist-get attr :alt) "")))
    (message "[org-neuron-link DBG] Handling Images %s %s" alt-text path)
    ;; Neuron only supports linking to files in the static folder
    (format "![%s](%s)" alt-text (concat "./static" path))))

(defun org-neuron-link (link desc info)
  "Convert LINK to Neuron Markdown format.

This function defers to `org-hugo-link' for everything other than
`id', `custom_id' and brain links (`brain' `brain-child'
`brain-parent', `brain-friend').

DESC is the link's description.
INFO is a plist used as a communication channel."
  ;; (message "[org-neuron-link DBG] %s %s" link desc)
  (let* ((type (org-element-property :type link)))
    (cond
     ((member type '(;; Handle ID links
                     "custom-id" "id"
                     ;; Handle Brain links
                     "brain" "brain-child" "brain-parent" "brain-friend"))
      (org-neuron--id-link link desc))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (org-neuron--image-link link info))
     (t ;; Let Hugo deal with it.
      (progn
        (message "[ox-neuron-link DBG] Calling out to ox-hugo-link!")
        ;; org-hugo will help copy stuff to the right place and create
        ;; the appropriate directories.
        (org-hugo-link link desc info))))))

(defun org-neuron--file-node-p ()
  "Return the file ID if the `current-buffer' has File Level properties drawer.

This indicates that the file should be considered as the top-most
level Neuron post in this case."
  (org-id-get (point-min)))

(defun org-neuron--get-file-name (entry &optional fullpath)
  "Return the file-name for ENTRY Neuron post.

If the EXPORT_FILE_NAME is index, and if we are building a
FULLPATH (with full directory nesting), then as a special case
return the ID of the entry instead of the EXPORT_FILE_NAME.

This is because Neuron cannot handle an index.md file as well as
a index/ folder."
  (let ((filename (org-string-nw-p
                   (org-element-property :EXPORT_FILE_NAME entry)))
        (node-id (or (org-element-property :ID entry)
                     (with-temp-buffer
                       (org-mode)
                       (insert (org-element-interpret-data entry))
                       (org-neuron--file-node-p)))))
    (if (and (equal "index" filename) fullpath)
        (org-neuron--zettel-id node-id)
      (or filename (org-neuron--zettel-id node-id)))))

(defun org-neuron--valid-subtree (elem)
  "Return t if ELEM is a valid subtree, else nil."
  (org-element-property :ID elem))

(defun org-neuron--get-valid-post ()
  "Return the Org element for a valid Neuron post.
The condition to check validity is that the ID property is
defined for the heading / file element.

As this function is intended to be called inside a valid Neuron
post subtree, doing so also moves the point to the beginning of
the heading / file.

Return nil if a valid Neuron post is not found. The point will be
moved in this case too."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (valid (org-neuron--valid-subtree entry))
             (fname (org-neuron--get-file-name entry))
             level)
        (when (and valid fname)
          (throw 'break entry))
        ;; Keep on jumping to the parent heading if the current entry
        ;; does not have an ID property.
        (setq level (org-up-heading-safe))
        ;; If no more parent heading exists, check if the file itself
        ;; is a Neuron Node. If so, return the contents of the file,
        ;; else break out of the loop and return nil
        (unless level
          (if (org-neuron--file-node-p)
              (progn (goto-char (point-min))
                     (throw 'break (org-element-parse-buffer)))
            (throw 'break nil)))))))

(defun org-neuron--build-path (base-dir dir-paths)
  "Take the BASE-DIR and DIR-PATHS collected in processing an entry.

Build a path from BASE-DIR to this location using this information."
  ;; (message "[org-neuron--build-path DBG] %s %s" base-dir dir-paths)
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
              (let* ((entry (org-neuron--get-valid-post))
                     (fname (org-neuron--get-file-name entry :fullpath)))
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
  ;; (message "[ox-neuron-export-to-md DBG] Starting")
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
      (org-hugo--after-1-export-function info outfile)
      (unless org-hugo--disable-after-all-exports-hook
        (org-hugo--after-all-exports-function)))))

(defvar org-neuron--seen-headings '()
  "Internal variable for the elem-to-id translation.")

(defun org-neuron--elem-to-id (menu-headline elem)
  "Given an ELEM, convert it to an ID/Brain link.

Attach the ID / Brain link to the MENU-HEADLINE, for use later.
SEEN-HEADINGS tracks sub-headings that have been processed. This
helps avoid processing of sub-sub-headings.

This is done only if the elem is a sub-heading. It is expected
that sub-headings will be exported into their own files."
  ;; (message "[org-neuron--elem-to-id DBG]")
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
      (progn ;; (message "[org-neuron--elem-to-id DBG] ignoring %s %s"
             ;;          (org-element-type elem)
             ;;          (org-element-property :title elem))
             elem))))

(defun org-neuron--process-subheadings (ast)
  "Internal function used to generate a pre-processed buffer.

Takes the parse-tree as AST and removes all Neuron sub-headings
in the post (since these will be separate Neuron posts).

If `org-neuron-insert-subheadings-as-children' is t, this
function collects links to all the children under a new
sub-heading called Children.

This function updates the AST, which the calling function is then
supposed to use for further processing."
  (let* ((main-headline (org-element-map ast 'headline
                          (lambda (hl)
                            (when (not (eq (org-element-type
                                            (org-element-property :parent hl))
                                           'headline))
                              hl))
                          nil t))
         ;; insert-location returns the location of the first valid
         ;; neuron subheading.
         (insert-location (org-element-map ast 'headline
                            (lambda (hl)
                              (when (and (eq (org-element-type
                                              (org-element-property :parent hl))
                                             'headline)
                                         (org-neuron--valid-subtree hl))
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
      (when org-neuron-insert-subheadings-as-children
        (org-element-insert-before menu-headline insert-location))
      (setq org-neuron--seen-headings nil)
      (org-element-map ast 'headline
        (apply-partially #'org-neuron--elem-to-id menu-headline)))))

(defun org-neuron--get-pre-processed-buffer ()
  "Return a pre-processed copy of the current buffer.

Internal links to other subtrees are converted to external
links."
  ;; Narrow down to just this subtree
  ;; (message "[org-neuron--get-pre-processed-buffer DBG]")
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
        (org-neuron--process-subheadings ast)

        ;; (message "[ox-neuron--preprocessing DBG] AST: %s" ast)
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
  ;; (message "[ox-neuron--export-subtree DBG] Subtree: %s, Starting"
  ;;          (org-element-property :title subtree))
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 'neuron t visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'neuron t)))
         (exclude-tags (plist-get info :exclude-tags))
         (is-commented (org-element-property :commentedp subtree))
         (title (org-element-property :title subtree))
         (subtree-beg (org-element-property :begin subtree))
         is-excluded matched-exclude-tag ret)
    ;; (message "[ox-neuron--export-subtree DBG] Subtree: %s, Info built"
    ;;          title)
    ;; Move point to the beginning of the subtree
    (goto-char subtree-beg)
    (let ((all-tags (let ((org-use-tag-inheritance t))
                      (org-hugo--get-tags subtree-beg))))
      (when all-tags
        (dolist (exclude-tag exclude-tags)
          (when (member exclude-tag all-tags)
            (setq matched-exclude-tag exclude-tag)
            (setq is-excluded t)))))
    ;; (message "[ox-neuron--export-subtree-to-md DBG] Subtree: %s, Tags Built"
    ;;          title)
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
             (outfile (concat pub-dir (org-neuron--get-file-name entry) ".md"))
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
  (let ((subtree (org-neuron--get-valid-post)))
    (if subtree
        ;; If subtree is a valid post subtree, export it and all of
        ;; it's valid children posts as well.
        (prog1 (org-neuron--export-subtree subtree visible-only)
          (let ((ast (org-element-parse-buffer 'headline))
                (parent-id (org-element-property :ID subtree)))
            (org-element-map ast 'headline
              (lambda (hl)
                (when (and (org-neuron--valid-subtree hl)
                           (equal parent-id
                                  (org-element-property :ID (org-element-property :parent hl))))
                  (goto-char (org-element-property :begin hl))
                  (org-neuron--export-subtree-to-md visible-only))
                ;; Explicitly return nil since I don't care about
                ;; the return value.
                nil))))

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
        ;; (message "[ox-neuron--export-subtree-to-md DBG] Not in valid Subtree")
        (when valid-subtree-found
          (message "Point is not in a valid Neuron subtree; move to one and try again"))
        valid-subtree-found))))

(defun org-neuron-export-wim-to-md
    (&optional visible-only)
  "Export the current subtree and all it's subheadings to Neuron posts.

This is an Export \"What I Mean\" function:

- If the current subtree has the ID property, export that
  subtree.

- If the current subtree doesn't have this properties, but one
  of its parent subtrees has, then export from that subtree's
  scope.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (interactive "P")
  ;; (message "[ox-neuron-export-wim-to-md DBG] Starting %s" visible-only)
  (let (ret)
    (save-window-excursion
      (save-restriction
        (widen)
        (save-excursion
          (setq ret (org-neuron--export-subtree-to-md visible-only)))))
    ret))

(defun org-neuron-export-file-to-md
    (&optional visible-only)
  "Export all the \"top-level\" headings in the current file to Neuron posts.

Further, if the file itself is a Valid Neuron post (i.e. it has a
file-level properties drawer and title), then export that as the
parent to all the top-level headings.

This action will recursively publish all the subheadings in the file as well.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (interactive "P")
  ;; (message "[ox-neuron-export-file-to-md DBG] Starting %s" visible-only)
  (let (ret)
    (save-window-excursion
      (save-restriction
        (widen)
        (when (org-neuron--file-node-p)
          ;; do something about the file content itself
          )
        (save-excursion
          ;; Pass the file-level properties into subtree export calls,
          ;; so that export has access to them.
          (let ((info (org-combine-plists
                       (org-export--get-export-attributes
                        'neuron nil visible-only)
                       (org-export--get-buffer-attributes)
                       (org-export-get-environment 'neuron nil)
                       (when (org-neuron--file-node-p)
                         ;; copy the file-id into the plist for
                         ;; nesting other headings under it.
                         (plist-put nil :neuron-file-id (org-neuron--file-node-p)))))
                (ast (org-element-parse-buffer 'headline)))
            (org-element-map ast 'headline
              (lambda (hl)
                (when (and (= (org-element-property :level hl) 1)
                           (org-neuron--valid-subtree hl))
                  (goto-char (org-element-property :begin hl))
                  (setq ret (org-neuron--export-subtree-to-md visible-only)))
                ;; Explicitly return nil since I don't care about
                ;; the return value.
                nil))))))
    ret))

(provide 'ox-neuron)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-neuron.el ends here
