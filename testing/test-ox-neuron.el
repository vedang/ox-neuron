;;; test-ox-neuron.el --- Tests for ox-neuron exporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Vedang Manerikar

;; Author: Vedang Manerikar <vedang@helpshift.com>
;; Keywords: testing, org, markdown, zettelkasten

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

;; This file contains test scenarios for the ox-neuron exporter

;;; Code:

;; Require macros from `org-mode'
(require 'org-test
         (concat (file-name-directory (locate-library "org"))
                 "../testing/org-test.el"))
(require 'test-ox
         (concat (file-name-directory (locate-library "org"))
                 "../testing/lisp/test-ox.el"))
;; Require system under test
(require 'ox-neuron)

(ert-deftest test-ox-neuron/org-neuron--zettel-markup ()
  "Generate correct Zettel links in Markdown based on relationship."
  (should
   (string-equal "[[1234|Test Desc]]#"
                 (org-neuron--zettel-markup :child "1234" "Test Desc")))
  (should
   (string-equal "[[1234]]#"
                 (org-neuron--zettel-markup :child "1234" nil)))

  (should
   (string-equal "#[[1234|Test Desc]]"
                 (org-neuron--zettel-markup :parent "1234" "Test Desc")))

  (should
   (string-equal "#[[1234]]"
                 (org-neuron--zettel-markup :parent "1234" nil)))

  (should
   (string-equal "[[1234|Test Desc]]"
                 (org-neuron--zettel-markup :sibling "1234" "Test Desc")))

  (should
   (string-equal "[[1234]]"
                 (org-neuron--zettel-markup :sibling "1234" nil))))

(ert-deftest test-ox-neuron/org-neuron--id-link ()
  "Links to Org Mode content are converted to Neuron Markdown correctly."
  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[7e221a93|Yogurt]]"
      (org-neuron--id-link (org-element-map (org-element-parse-buffer) 'link
                             'identity nil t)
                           "Yogurt"))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[7e221a93]]"
      (org-neuron--id-link (org-element-map (org-element-parse-buffer) 'link
                             'identity nil t)
                           nil))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[7e221a93|Yogurt]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain" (org-element-property :type link))
             link))
         nil t)
       "Yogurt"))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[7e221a93]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain" (org-element-property :type link))
             link))
         nil t)
       nil))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[5b1688d9|The perfect way to make Dahi]]#"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-child" (org-element-property :type link))
             link))
         nil t)
       "The perfect way to make Dahi"))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[5b1688d9]]#"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-child" (org-element-property :type link))
             link))
         nil t)
       nil))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "#[[07caf760|Milk]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-parent" (org-element-property :type link))
             link))
         nil t)
       "Milk"))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "#[[07caf760]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-parent" (org-element-property :type link))
             link))
         nil t)
       nil))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[5b64fca9|Cheese]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-friend" (org-element-property :type link))
             link))
         nil t)
       "Cheese"))))

  (should
   (org-test-in-example-file "./ox-neuron-example.org"
     (string-equal
      "[[5b64fca9]]"
      (org-neuron--id-link
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (when (equal "brain-friend" (org-element-property :type link))
             link))
         nil t)
       nil)))))

(ert-deftest test-ox-neuron/org-neuron--image-link ()
  (should
   (org-test-in-example-file "./ox-neuron-example-mahabharata.org"
     (string-equal
      "![The Opening Prayer](./static/ox-hugo/01-narayanam-namaskrutya.png)"
      (let ((info (org-combine-plists
                   (org-export--get-export-attributes
                    'neuron t nil)
                   (org-export--get-buffer-attributes)
                   (org-export-get-environment 'neuron t)))
            (image-link (org-element-map (org-element-parse-buffer) 'link
                          (lambda (link)
                            (when (org-export-inline-image-p link org-html-inline-image-rules)
                              link))
                          nil t)))
        (org-neuron--image-link image-link info))))))

(ert-deftest test-ox-neuron/org-neuron--valid-subtree ()
  "Headings with :ID: property are valid subtrees."
  (should-not
   (org-test-at-marker "./ox-neuron-example.org" "* Dahi"
	 (org-neuron--valid-subtree (org-element-at-point))))

  (should
   (org-test-at-marker "./ox-neuron-example.org" "* Cheese"
	 (org-neuron--valid-subtree (org-element-at-point)))))

(ert-deftest test-ox-neuron/org-neuron--get-valid-post ()
  "Point moves to nearest valid subtree above point, and element is returned."
  (should
   (org-test-at-marker "./ox-neuron-example.org" "<point>Dahi</point>"
     (let ((element (org-neuron--get-valid-post)))
       (string-equal "Yogurt" (org-element-property :title element)))))

  (should-not
   (org-test-at-marker "./ox-neuron-example-no-file-node.org" "<point>Dahi</point>"
     (org-neuron--get-valid-post)))
  ;; If the heading does not have :ID: but it is in a file which has a
  ;; :PROPERTIES: drawer and :ID: field, it is still a valid post.
  (should
   (org-test-at-marker "./ox-neuron-example.org" "<point>Kharvas</point>"
     (let ((tree (org-neuron--get-valid-post)))
       (string-equal "07caf760-019b-4b7c-8b29-e1189490af31"
                     (org-element-property :value
                                           (org-element-map tree
                                               'node-property 'identity nil t))))))
  ;; If both the top-most heading as well as file do not have an :ID:,
  ;; then the post is not valid.
  (should-not
   (org-test-at-marker "./ox-neuron-example-no-file-node.org" "<point>Dahi</point>"
     (org-neuron--get-valid-post))))

(ert-deftest test-ox-neuron/org-neuron--file-node-p ()
  "Files with a toplevel properties drawer with ID are considered Neuron posts.

Note: The properties drawer should be the first thing in the file."
  (should
   (org-test-at-marker "./ox-neuron-example.org" "* Yogurt"
     (org-neuron--file-node-p)))

  (should-not
   (org-test-at-marker "./ox-neuron-example-no-file-node.org" "* Yogurt"
	 (org-neuron--file-node-p)))

  ;; Make sure org-neuron--file-node-p returns the correct ID (file node ID)
  (should
   (org-test-at-marker "./ox-neuron-example.org" "* Yogurt"
	 (string-equal "07caf760-019b-4b7c-8b29-e1189490af31"
                   (org-neuron--file-node-p)))))

(provide 'test-ox-neuron)
;;; test-ox-neuron.el ends here
