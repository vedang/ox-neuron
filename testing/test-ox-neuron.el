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

(provide 'test-ox-neuron)
;;; test-ox-neuron.el ends here
