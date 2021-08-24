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

(ert-deftest test-ox-neuron/org-neuron--valid-subtree ()
  "Headings with :ID: property are valid subtrees."
  (should-not
   (org-test-with-temp-text "* Headline\n*a* b"
	 (org-neuron--valid-subtree (org-element-at-point))))

  (should
   (org-test-with-temp-text
       "* Headline\n:PROPERTIES:\n:ID:  7e221a93-6e26-414f-b2b1-1716a15c4539\n:END:"
	 (org-neuron--valid-subtree (org-element-at-point)))))

(ert-deftest test-ox-neuron/org-neuron--file-node-p ()
  "Files with a toplevel properties drawer with ID are considered Neuron posts.

Note: The properties drawer should be the first thing in the file."
  (should
   (org-test-with-temp-text
       ":PROPERTIES:\n:ID:       07caf760-019b-4b7c-8b29-e1189490af31\n:END:\n#+filetags: :iota: \n#+neuron_base_dir: \"/home/vedang/src/data/\"\n#+title: Milk Products are the best\n\nWhat was life before I ever tasted Cheese? Paneer? Kharvas! \n\n<point>* Yogurt\n:PROPERTIES:\n:ID:  7e221a93-6e26-414f-b2b1-1716a15c4539\n:END:"
	 (org-neuron--file-node-p)))

  (should-not
   (org-test-with-temp-text
       "#+filetags: :iota: \n#+neuron_base_dir: \"/home/vedang/src/data/\"\n#+title: Milk Products are the best\n\nWhat was life before I ever tasted Cheese? Paneer? Kharvas! \n\n<point>* Yogurt\n:PROPERTIES:\n:ID:  7e221a93-6e26-414f-b2b1-1716a15c4539\n:END:"
	 (org-neuron--file-node-p)))

  (should
   (equal 231
          (org-test-with-temp-text
              ":PROPERTIES:\n:ID:       07caf760-019b-4b7c-8b29-e1189490af31\n:END:\n#+filetags: :iota: \n#+neuron_base_dir: \"/home/vedang/src/data/\"\n#+title: Milk Products are the best\n\nWhat was life before I ever tasted Cheese? Paneer? Kharvas! \n\n<point>* Yogurt\n:PROPERTIES:\n:ID:  7e221a93-6e26-414f-b2b1-1716a15c4539\n:END:"
	        (org-neuron--file-node-p)
            (point))))

  (should-not
   (org-test-with-temp-text
       "#+filetags: iota \n:PROPERTIES:\n:ID:       07caf760-019b-4b7c-8b29-e1189490af31\n:END:\n#+neuron_base_dir: \"/home/vedang/src/data/\"\n#+title: Milk Products are the best\n\nWhat was life before I ever tasted Cheese? Paneer? Kharvas! \n\n<point>* Yogurt\n:PROPERTIES:\n:ID:  7e221a93-6e26-414f-b2b1-1716a15c4539\n:END:"
	 (org-neuron--file-node-p))))

(provide 'test-ox-neuron)
;;; test-ox-neuron.el ends here
