;;; org-ref-cite-activate.el --- org-cite activate processor
;;
;; Copyright(C) 2021 John Kitchin
;;
;; This file is not currently part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This adds a keymap to citation objects, a tooltip that shows how they export
;; to LaTeX, and fontification on different parts of a citation that might be
;; invalid for this processor.

(require 'oc-basic)
(require 'org-ref-cite-core)
(require 'org-ref-cite-follow)

;; * Activation

;;; Code:

(defcustom org-ref-cite-citation-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "C-<right>") 'org-ref-cite-next-reference)
    (define-key map (kbd "C-<left>") 'org-ref-cite-previous-reference)
    (define-key map (kbd "S-<right>") 'org-ref-cite-shift-right)
    (define-key map (kbd "S-<left>") 'org-ref-cite-shift-left)
    (define-key map (kbd "S-<up>") 'org-ref-cite-sort-year-ascending)
    (define-key map (kbd "C-a") 'org-ref-cite-goto-cite-beginning)
    (define-key map (kbd "C-e") 'org-ref-cite-goto-cite-end)
    (define-key map (kbd "C-d") 'org-ref-cite-delete)
    (define-key map (kbd "C-q") ' org-ref-cite-jump-to-visible-key)
    (define-key map (kbd "C-/") 'org-ref-cite-describe-keymap)
    (define-key map (kbd "C-k") 'org-ref-cite-kill-cite)
    (define-key map (kbd "M-w") 'org-ref-cite-copy-cite)
    (define-key map (kbd "M-m") 'org-ref-cite-mark-cite)
    (define-key map (kbd "M-s") 'org-ref-cite-update-style)
    (define-key map (kbd "M-p") 'org-ref-cite-update-pre/post)
    (define-key map (kbd "M-r") 'org-ref-cite-replace-key-with-suggestions)
    (define-key map (kbd "RET") 'org-ref-cite-follow)
    ;; this seems like it is causing issues when you click at the end of a cite. taking it out for now.
    ;; for some reason it is inserting a line?
    ;; (define-key map (kbd "<mouse-1>") 'org-ref-cite-follow)
    map)
  "A keymap for `org-cite' citation elements."
  :group 'org-ref-cite)


(defun org-ref-cite-describe-keymap ()
  "Describe the `org-ref-cite-citation-keymap' keymap."
  (interactive)
  (describe-keymap org-ref-cite-citation-keymap))


(defcustom org-ref-cite-invalid-prefix-suffix-color
  "red"
  "Color for invalid prefix/suffixes in multireference citations."
  :group 'org-ref-cite)


(defcustom org-ref-cite-invalid-style-color
  "red"
  "Color for invalid styles that are not defined in `org-ref-cite-styles'."
  :group 'org-ref-cite)


(defcustom org-ref-cite-activation-functions
  '(org-cite-basic-activate
    org-ref-cite-activate-keymap
    org-ref-cite-activate-tooltip
    org-ref-cite-activate-style-fontification
    org-ref-cite-activate-prefix-suffix)
  "List of activation functions for a citation.
Each function takes one argument, a citation."
  :group 'org-ref-cite)


(defface org-ref-cite-invalid-prefix-suffix-face
  `((t (:inherit org-cite :foreground ,org-ref-cite-invalid-prefix-suffix-color)))
  "Face for invalid prefix/suffix text in multireference citations")


(defface org-ref-cite-invalid-style-face
  `((t (:inherit org-cite :foreground ,org-ref-cite-invalid-style-color)))
  "Face for invalid prefix/suffix text in multireference citations")


(defun org-ref-cite-activate-keymap (citation)
  "Activation function for CITATION to add keymap and tooltip"
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    ;; Put the keymap on a citation
    (put-text-property beg end 'keymap org-ref-cite-citation-keymap)))


(defun org-ref-cite-activate-tooltip (citation)
  "Put a tooltip on the citation.
The exports the citation according to the backend defined in
CITE_EXPORT keyword, and defaults to the latex backend."
  (pcase-let* ((`(,beg . ,end) (org-cite-boundaries citation)))
    ;; put a rendered tooltip on the style part. Note that this assumes a latex
    ;; export.
    (put-text-property
     beg (1- (org-with-point-at beg (search-forward ":")))
     ;; Running this export results in running the org-mode hooks. I use this
     ;; function to delay getting the string until you mouse over.
     'help-echo (lambda (window object position)
		  (with-selected-window window
		    (goto-char position)
		    (let* ((context (org-element-context))
			   (cite-string (buffer-substring
					 (org-element-property :begin context)
					 (org-element-property :end context)))
			   ;; #+cite_export: name bibliography-style citation-style
			   (cite-export (cadr (assoc "CITE_EXPORT"
						     (org-collect-keywords
						      '("CITE_EXPORT")))))
			   (org-cite-proc (when cite-export
					    (cl-first (split-string cite-export))))
			   (backend (if cite-export
					(cl-loop for (backend ep _) in org-cite-export-processors
						 when (equal ep (intern-soft org-cite-proc))
						 return backend)
				      org-ref-cite-default-preview-backend)))
		      (when (or (string= "nil" backend)
				(equal backend t))
			(setq backend org-ref-cite-default-preview-backend))
		      (org-trim (org-export-string-as
				 (concat
				  (if cite-export
				      (concat (format "#+cite_export: %s\n" cite-export))
				    "")
				  cite-string)
				 backend t))))))))


(defun org-ref-cite-activate-style-fontification (citation)
  "Activation function for CITATION to fontify invalid styles"
  (when  (member (cl-second (assoc 'latex org-cite-export-processors))
		 '(org-ref-cite natbib))
    ;; Check the style
    (unless (member (or (org-element-property :style citation) "") (org-ref-cite-flat-style-strings))
      (add-text-properties
       (org-element-property :begin citation)
       (1- (org-with-point-at (org-element-property :begin citation) (search-forward ":")))
       '(face org-ref-cite-invalid-style-face help-echo
	      "This style is not supported in org-ref-cite.")))))


(defun org-ref-cite-activate-prefix-suffix (citation)
  "Activation for CITATION to fontify prefix/suffix text that will be ignored."
  ;; this only applies to org-ref-cite and natbib. biblatex is more flexible
  ;; than these ones.
  (when  (member (cl-second (assoc 'latex org-cite-export-processors))
		 '(org-ref-cite natbib))
    (cl-loop for i from 0 for ref in (org-cite-get-references citation) do
	     ;; Only prefixes on the first citation are actually supported.
	     ;; And it will be concatenated with the global prefix.
	     (when (and (> i 0)
			(org-element-property :prefix ref))
	       (add-text-properties
		(org-element-property :begin ref)
		(+ (org-element-property :begin ref)
		   (length (org-no-properties
			    (cl-third (org-cite-make-paragraph
				       (org-element-property :prefix ref))))))
		'(face org-ref-cite-invalid-prefix-suffix-face help-echo
		       "Prefix text is not valid here and will be ignored in export.")))

	     ;; only suffixes on the last citation is supported.
	     (when (and (< i (- (length (org-cite-get-references citation)) 1))
			(org-element-property :suffix ref))
	       (add-text-properties
		(- (org-element-property :end ref)
		   (length (org-no-properties
			    (cl-third (org-cite-make-paragraph
				       (org-element-property :suffix ref))))))
		(org-element-property :end ref)
		'(face org-ref-cite-invalid-prefix-suffix-face help-echo
		       "Suffix text is not valid here and will be ignored in export."))))))


(defun org-ref-cite-activate (citation)
  "Run all the activation functions in `org-ref-cite-activation-functions'.
Argument CITATION is an org-element holding the references."
  (cl-loop for activate-func in org-ref-cite-activation-functions
	   do
	   (funcall activate-func citation)))


(org-cite-register-processor 'org-ref-cite-activate
  :activate #'org-ref-cite-activate)

(provide 'org-ref-cite-activate)

;;; org-ref-cite-activate.el ends here
