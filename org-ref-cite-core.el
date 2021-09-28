;;; org-ref-cite-core.el --- Core functions
;;
;; Copyright(C) 2021 John Kitchin
;;
;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref-cite
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org "9.5") (avy "0") (hydra "0") (bibtex-completion "0"))

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
;;
;;; Commentary:
;;
;; This is a core library that provides functions used in the processor components.

;;; Code:
(require 'oc)
(require 'oc-basic)
(require 'avy)
(require 'bibtex-completion)


(defcustom org-ref-cite-styles
  '(((("text" "t")) . "\\citet")
    ((("text" "t") ("bare" "b")) . "\\citealt")
    ((("text" "t") ("caps" "c")) . "\\Citet")
    ((("text" "t") ("full" "f")) . "\\citet*")
    ((("text" "t") ("caps-full" "cf")) . "\\Citet*")
    ((("text" "t") ("bare-caps" "bc")) . "\\Citealt*")
    ((("text" "t") ("bare-full" "bf")) . "\\citealt*")
    ((("text" "t") ("bare-full" "bcf")) . "\\Citealt*")
    ;; author styles
    ((("author" "a")) . "\\citeauthor")
    ((("author" "a") ("caps" "c")) . "\\Citeauthor")
    ((("author" "a") ("full" "f")) . "\\citeauthor*")
    ((("author" "a") ("caps-full" "cf")) . "\\Citeauthor*")
    ;; suppress author
    ((("noauthor" "na")) . "\\citeyearpar")
    ((("noauthor" "na") ("bare" "b")) . "\\citeyear")
    ((("nocite")) . "\\nocite")
    ;; parenthetical / nil styles
    ((("p")) . "\\citep")
    ((("p") ("bare" "b")) . "\\citealp")
    ((("p") ("caps" "c")) . "\\Citep")
    ((("p") ("full" "f")) . "\\citep*")
    ((("p") ("bare-caps" "bc")) . "\\Citealp")
    ((("p") ("bare-full" "bf")) . "\\citealp*")
    ((("p") ("bare-caps-full" "bcf")) . "\\Citealp*")
    ;; miscellaneous
    ((("num")) . "\\citenum")
    (((nil)) . "\\cite"))
  "Alist of styles and commands for `org-ref-cite'.
Each style element is ((Main-style variants) . COMMAND).
Main-style is a list of strings that match the main style.
variants are a list of strings that match the variant.

All combinations of the Main-style and variants map on to COMMAND
at export."
  :group 'org-ref-cite)


(defcustom org-ref-cite-style-format
  'short-short
  "Format for org-ref-cite-styles.
Should be a symbol like: 'long-long, 'long-short, 'short-long,
'short-short or 'all"
  :group 'org-ref-cite)


(defcustom org-ref-cite-default-style
  "/t"
  "The default style to use when inserting citations.
It should either be \"\" or start with a /.
It should be a member of the output of `org-ref-cite-flat-style-strings')"
  :group 'org-ref-cite)


(defcustom org-ref-cite-style-annotation-function
  #'org-ref-cite-annotate-style
  "Function to annotate styles in selection.
Two options are `org-ref-cite-basic-annotate-style', and
`org-ref-cite-annotate-style'."
  :group 'org-ref-cite)


(defcustom org-ref-cite-default-preview-backend 'latex
  "Default backend to generate previews with.
It should be one of the org-mode exporters defined in
`org-cite-export-processors'. You can override this with the
cite_export keyword in a buffer."
  :group 'org-ref-cite)


;; * Style

(defface org-ref-cite-annotate-style-face
  `((t (:inherit org-cite)))
  "Face for an annotated style.")


(defun org-ref-cite-get-cite-styles ()
  "Returns cite styles for the `org-ref-cite' processor.
This uses the (style variant) structure of org-cite.
This is intended for use in registering a processor."
  (let ((cite-styles '()))
    (cl-loop for ((style substyle) . cmd) in org-ref-cite-styles
	     do
	     (if (assoc style cite-styles)
		 (when substyle (push substyle (cdr (assoc style cite-styles))))
	       (push (list style) cite-styles)))
    cite-styles))


(defun org-ref-cite-get-combinatorial-style-commands ()
  "Return the combinatorial possible styles and commands.
Returns a list of elements like (STYLE . CMD) for every
combination of the full and abbreviated names. STYLE is a cons
cell like org-cite uses (Main-style . variant)."
  (apply #'append
	 (cl-loop for ((style substyle) . cmd) in org-ref-cite-styles
		  collect
		  (apply #'append
			 (cl-loop for st in style collect
				  (if substyle
				      (cl-loop for subst in substyle collect
					       (cons (cons st subst) cmd))
				    (list (cons (cons st nil) cmd))))))))

(defun org-ref-cite-get-combinatorial-styles ()
  "Return the combinatorial possible styles for the export processor."
  (let ((styles (org-cite-supported-styles (list (org-ref-cite-guess-export-processor)))))

    (apply #'append
	   (cl-loop for group in styles
		    collect
		    (apply #'append
			   (cl-loop for (variant abbrv) in (cdr group) collect
				    (if substyle
					(cl-loop for subst in substyle collect
						 (cons (cons st subst) cmd))
				      (list (cons (cons st nil) cmd)))))))))


(defun org-ref-cite--style-to-command (style)
  "Look up the command for STYLE where style is a (style . variant) from an export processor."
  (cdr (assoc style (org-ref-cite-get-combinatorial-style-commands))))


(defun org-ref-cite-flat-style-strings ()
  "Returns a flat list of possible style strings."
  (cl-loop for (style . command) in (org-ref-cite-get-combinatorial-style-commands)
	   collect
	   (concat (car style)
		   (when (cdr style) (format "/%s" (cdr style))))))


(defun org-ref-cite-guess-backend ()
  "Returns best guess for the export backend.
Looks for a #+cite_export keyword, and defaults to
`org-ref-cite-default-preview-backend'."
  (let* (;; #+cite_export: name bibliography-style citation-style
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
    (when (or (null backend) (string= "nil" backend))
      (setq backend org-ref-cite-default-preview-backend))
    (cons backend cite-export)))


(defun org-ref-cite-guess-export-processor ()
  "Returns best guess for the export processor.
Looks for a #+cite_export keyword, and defaults to
`org-ref-cite-default-preview-backend'."
  (let* (;; #+cite_export: name bibliography-style citation-style
	 (cite-export (cadr (assoc "CITE_EXPORT"
				   (org-collect-keywords
				    '("CITE_EXPORT")))))
	 (org-cite-proc (when cite-export
			  (cl-first (split-string cite-export)))))
    (or org-cite-proc
	(cl-loop for (backend ep _) in org-cite-export-processors
		 when (equal backend org-ref-cite-default-preview-backend)
		 return ep)
	'basic)))


(defun org-ref-cite-basic-annotate-style (s)
  "Annotation function for selecting style.
Argument S is the style candidate string."
  (let* ((w (+  (- 5 (length s)) 20))
	 (backend (org-ref-cite-guess-backend)))
    (concat (make-string w ? )
	    (propertize
	     (org-trim
	      (org-export-string-as (format "%s\n[cite/%s:@key]"
					    (if (cdr backend)
						(format "#+cite_export: %s\n" (cdr backend))
					      "")
					    s)
				    (car backend)
				    t))
	     'face 'org-ref-cite-annotate-style-face))))


(defun org-ref-cite-annotate-style (s)
  "Annotation function for selecting style.
Argument S is the style string.

If point is on a citation, it makes an export preview of the citation with the style."
  (let ((context (org-element-context)))
    (if (member (org-element-type context) '(citation citation-reference))
	(let* ((citation (if (member (org-element-type context) '(citation))
			     context
			   (org-element-property :parent context)))
	       (references (org-cite-get-references citation))
	       (cite-string (format "[cite/%s:%s]"
				    s
				    (org-element-interpret-data references)))
	       (cite-export (cadr (assoc "CITE_EXPORT"
					 (org-collect-keywords
					  '("CITE_EXPORT")))))
	       (org-cite-proc (when cite-export
				(cl-first (split-string cite-export))))
	       (backend (if cite-export
			    (cl-loop for (backend ep _) in org-cite-export-processors
				     when (equal ep (intern-soft cite-export))
				     return backend)
			  org-ref-cite-default-preview-backend ))
	       (export-string (concat
			       (if cite-export
				   (concat (format "#+cite_export: %s\n" cite-export))
				 "")
			       cite-string)))
	  (when (or (null backend) (string= "nil" backend)) (setq backend org-ref-cite-default-preview-backend))
	  (concat
	   (make-string (+  (- 5 (length s)) 20) ? )
	   (propertize
	    ;; I think these should be one line.
	    (replace-regexp-in-string "
" ""
(org-trim (org-export-string-as
	   export-string
	   backend t)))
	    'face 'org-ref-cite-annotate-style-face)))
      ;; not on a citation or reference, use the basic one.
      (org-ref-cite-basic-annotate-style s))))


(defun org-ref-cite-select-style ()
  "Select a style with completion."
  (interactive)
  (let* ((styles (org-cite-supported-styles (list (org-ref-cite-guess-export-processor))))
	 (style-candidates (let ((styles (org-cite-supported-styles (list (org-ref-cite-guess-export-processor)))))
			     (delete nil (cl-loop for group in styles append
						  (cl-loop for variant in (cdr group) append
							   (list `(,(car group) . (,variant))))))))

	 (cons-candidates (pcase org-ref-cite-style-format
			    ('short-short
			     (cl-loop for (main variant) in style-candidates
				      collect (cons (cl-second main) (cl-second variant))))
			    ('short-long
			     (cl-loop for (main variant) in style-candidates
				      collect (cons (cl-second main) (cl-first variant))))
			    ('long-long
			     (cl-loop for (main variant) in style-candidates
				      collect (cons (cl-first main) (cl-first variant))))
			    ('long-short
			     (cl-loop for (main variant) in style-candidates
				      collect (cons (cl-first main) (cl-second variant))))
			    (_
			     style-candidates)))
	 (candidates (mapcar (lambda (c) (format "%s/%s" (or (car c) "") (cdr c))) cons-candidates)))

    (completing-read "Style: "
		     (lambda (str pred action)
		       (if (eq action 'metadata)
			   `(metadata
			     ;; I use this closure since we need the table to do the annotation.
			     (annotation-function . org-ref-cite-annotate-style)
			     (cycle-sort-function . identity)
			     ;; Sort so shorter keys (abbreviations come first)
			     (display-sort-function . (lambda (candidates)
							(sort candidates (lambda (a b)
									   (> (length a) (length b))))))
			     ;; Group by the first letter.
			     (group-function . (lambda (style transform)
						 (if transform
						     style
						   ;; this is the group calculation
						   (cond
						    ((string= "" style)
						     "Misc")
						    ((string= "t" (substring style 0 1))
						     "Text")
						    ((string= "p" (substring style 0 1))
						     "Parenthetical")
						    ((string= "num" style)
						     "Number")
						    ((string= "a" (substring style 0 1))
						     "Author")
						    ((or (string= "no" (substring style 0 2))
							 (string= "na" (substring style 0 2)))
						     "No author")
						    (t
						     "Misc"))))))
			 (complete-with-action action candidates str pred))))))


(defun org-ref-cite-update-style ()
  "Change the style of the citation at point."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (refs (org-cite-get-references current-citation))
	 (style (org-ref-cite-select-style))
	 (cp (point)))
    (setf (buffer-substring (org-element-property :begin current-citation)
			    (org-element-property :end current-citation))
	  (format "[cite%s:%s]" (if (string= "" style)
				    ""
				  (concat "/" style))
		  (org-element-interpret-data refs)))
    (goto-char cp)))


;; * Navigation functions
;; There can be some subtle failures when there are duplicate keys sometimes.
(defun org-ref-cite-next-reference ()
  "Move point to the next reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum))
			datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position
				   refs current-ref
				   (lambda (r1 r2)
				     (= (org-element-property :begin r1)
					(org-element-property :begin r2)))))))
    (cond
     ;; ((null current-ref)
     ;;  (goto-char (org-element-property :begin (first (org-cite-get-references  datum)))))
     ;; this means it was not found.
     ((null index)
      (goto-char (org-element-property :begin (first refs))))
     ;; on last reference, try to jump to next one
     ((= index (- (length refs) 1))
      (when  (re-search-forward "\\[cite" nil t)
	(goto-char
	 (org-element-property :begin (first (org-cite-get-references
					      (org-element-context)))))))
     ;; otherwise jump to the next one
     (t
      (goto-char
       (org-element-property :begin (nth (min (+ index 1) (- (length refs) 1)) refs)))))))


(defun org-ref-cite-previous-reference ()
  "Move point to previous reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position
				   refs current-ref
				   (lambda (r1 r2)
				     (= (org-element-property :begin r1)
					(org-element-property :begin r2)))))))
    (cond
     ;; not found or on style part
     ((or (= index 0) (null index))
      (when (re-search-backward "\\[cite" nil t 2)
	(goto-char (org-element-property
		    :begin
		    (car (last (org-cite-get-references (org-element-context))))))))

     (t
      (goto-char (org-element-property :begin (nth (max (- index 1) 0) refs)))))))


(defun org-ref-cite-goto-cite-beginning ()
  "Move to the beginning of the citation."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum))))
    (if (= (point) (org-element-property :begin current-citation))
	(org-beginning-of-line)
      (goto-char (org-element-property :begin current-citation)))))


(defun org-ref-cite-goto-cite-end ()
  "Move to the end of the citation.
If at the end, use `org-end-of-line' instead."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum))))
    (if (= (point) (- (org-element-property :end current-citation)
		      (org-element-property :post-blank current-citation)))
	(org-end-of-line)

      (goto-char (- (org-element-property :end current-citation)
		    (org-element-property :post-blank current-citation))))))

(defun org-ref-cite-jump-to-visible-key ()
  "Jump to a visible key with avy."
  (interactive)
  (avy-with avy-goto-typo
    (avy-process (org-element-map (org-element-parse-buffer) 'citation
		   (lambda (c)
		     (org-element-property :begin c))))
    (avy--style-fn avy-style)))


;; * Editing

(defun org-ref-cite-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun org-ref-cite-shift-left ()
  "Shift the reference at point to the left."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (seq-position refs current-ref
			      (lambda (r1 r2)
				(and (string= (org-element-property :key r1)
					      (org-element-property :key r2))
				     (equal (org-element-property :prefix r1)
					    (org-element-property :prefix r2))
				     (equal (org-element-property :suffix r1)
					    (org-element-property :suffix r2)))))))
    (when (= 1 (length refs))
      (error "You only have one reference. You cannot shift this"))
    (when (null index)
      (error "Nothing to shift here"))
    (setf (buffer-substring (org-element-property :contents-begin current-citation)
			    (org-element-property :contents-end current-citation))
	  (org-element-interpret-data (org-ref-cite-swap index (- index 1) refs)))
    ;; Now get on the original ref.
    (let* ((newrefs (org-cite-get-references current-citation))
	   (index (seq-position newrefs current-ref
				(lambda (r1 r2)
				  (and (string= (org-element-property :key r1)
						(org-element-property :key r2))
				       (equal (org-element-property :prefix r1)
					      (org-element-property :prefix r2))
				       (equal (org-element-property :suffix r1)
					      (org-element-property :suffix r2)))))))

      (goto-char (org-element-property :begin (nth index newrefs))))))


(defun org-ref-cite-shift-right ()
  "Shift the reference at point to the right."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (seq-position refs current-ref
			      (lambda (r1 r2)
				(and (string= (org-element-property :key r1)
					      (org-element-property :key r2))
				     (equal (org-element-property :prefix r1)
					    (org-element-property :prefix r2))
				     (equal (org-element-property :suffix r1)
					    (org-element-property :suffix r2)))))))
    (when (= 1 (length refs))
      (error "You only have one reference. You cannot shift this"))

    (when (null index)
      (error "Nothing to shift here"))

    ;; Don't go past the end.
    (unless (= index (-  (length refs) 1))
      (setf (buffer-substring (org-element-property :contents-begin current-citation)
			      (org-element-property :contents-end current-citation))
	    (org-element-interpret-data (org-ref-cite-swap index (+ index 1) refs)))
      ;; Now get on the original ref.
      (let* ((newrefs (org-cite-get-references current-citation))
	     (index (seq-position newrefs current-ref
				  (lambda (r1 r2)
				    (and (string= (org-element-property :key r1)
						  (org-element-property :key r2))
					 (equal (org-element-property :prefix r1)
						(org-element-property :prefix r2))
					 (equal (org-element-property :suffix r1)
						(org-element-property :suffix r2)))))))
	(unless index (error "Nothing found"))
	(goto-char (org-element-property :begin (nth index newrefs)))))))


(defun org-ref-cite-sort-year-ascending ()
  "Sort the references at point by year (from earlier to later)."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (cp (point)))

    (setf (buffer-substring (org-element-property :contents-begin current-citation)
			    (org-element-property :contents-end current-citation))
	  (org-element-interpret-data
	   (sort refs (lambda (ref1 ref2)
			(let* ((e1 (bibtex-completion-get-entry
				    (org-element-property :key ref1)))
			       (e2 (bibtex-completion-get-entry
				    (org-element-property :key ref2)))
			       (y1 (string-to-number (or (cdr (assoc "year" e1)) "0")))
			       (y2 (string-to-number (or (cdr (assoc "year" e2)) "0"))))
			  (> y2 y1))))))
    (goto-char cp)))


(defun org-ref-cite-delete ()
  "Delete the reference or citation at point."
  (interactive)
  (org-cite-delete-citation (org-element-context)))


(defun org-ref-cite-update-pre/post ()
  "Change the pre/post text of the reference at point.
Notes: For export, prefix text is only supported on the first
reference. suffix text is only supported on the last reference.
This function will let you put prefix/suffix text on any
reference, but font-lock will indicate it is not supported for
natbib export."
  (interactive)
  (let* ((datum (org-element-context))
	 (ref (if (eq (org-element-type datum) 'citation-reference)
		  datum
		(error "Not on a citation reference")))
	 (current-citation (org-element-property :parent datum))
	 (refs (org-cite-get-references current-citation))
	 (key (org-element-property :key ref))
	 (pre (read-string "Prefix text: " (org-element-property :prefix ref)))
	 (post (read-string "Suffix text: " (org-element-property :suffix ref)))
	 (index (seq-position refs ref
			      (lambda (r1 r2)
				(and (string= (org-element-property :key r1)
					      (org-element-property :key r2))
				     (equal (org-element-property :prefix r1)
					    (org-element-property :prefix r2))
				     (equal (org-element-property :suffix r1)
					    (org-element-property :suffix r2)))))))
    (when (and (not (string= "" pre))
	       (> index 0))
      (message "Warning, prefixes only work on the first key."))

    (when (and (not (string= "" post))
	       (not (= index (- (length refs) 1))))
      (message "Warning, suffixes only work on the last key."))

    (setf (buffer-substring (org-element-property :begin ref)
			    (org-element-property :end ref))
	  (org-element-interpret-data `(citation-reference
					(:key ,key :prefix ,pre
					      :suffix ,post))))))


(defun org-ref-cite-kill-cite ()
  "Kill the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (kill-region (org-element-property :begin datum) (org-element-property :end datum))))


(defun org-ref-cite-replace-key-with-suggestions ()
  "Replace key at point with suggestions.
This is intended for use in fixing bad keys, but would work for similar keys."
  (interactive)
  (let* ((datum (org-element-context))
	 (prefix (org-element-property :prefix datum))
	 (suffix (org-element-property :suffix datum))
	 (beg (org-element-property :begin datum))
	 (end (org-element-property :end datum))
	 (key (org-element-property :key datum))
	 (bibtex-completion-bibliography (org-cite-list-bibliography-files))
	 (keys (cl-loop for cand in (bibtex-completion-candidates) collect
			(cdr (assoc "=key=" (cdr cand)))))
	 (suggestions (org-cite-basic--close-keys key keys))
	 (choice (completing-read "Replace with: " suggestions))
	 (cp (point)))
    (setf (buffer-substring beg end)
	  (org-element-interpret-data
	   `(citation-reference (:key ,choice :prefix ,prefix :suffix ,suffix))))
    (goto-char cp)))


;; * miscellaneous utilities

(defun org-ref-cite-mark-cite ()
  "Mark the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (set-mark (- (org-element-property :end datum)
		 (or (org-element-property :post-blank datum) 0)))
    (goto-char (org-element-property :begin datum))))


(defun org-ref-cite-copy-cite ()
  "Copy the reference/citation at point."
  (interactive)
  (org-ref-cite-mark-cite)
  (call-interactively 'kill-ring-save))


;; * a minimal inserter

(defun org-ref-cite--complete-key (&optional multiple)
  "Prompt for a reference key and return a citation reference string.

When optional argument MULTIPLE is non-nil, prompt for multiple keys, until one
of them is nil.  Then return the list of reference strings selected.

This uses bibtex-completion and is compatible with
`org-cite-register-processor'. You can use it in an insert
processor like this:

 (org-cite-register-processor 'my-inserter
  :insert (org-cite-make-insert-processor #'org-ref-cite--complete-key
					  #'org-ref-cite-select-style))

 (setq org-cite-insert-processor 'my-inserter)."
  (let* ((table (bibtex-completion-candidates))
         (prompt
          (lambda (text)
            (completing-read text table nil t))))
    (if (null multiple)
        (let ((key (cdr (assoc "=key=" (cdr (assoc (funcall prompt "Key: ") table))))))
          (org-string-nw-p key))
      (let* ((keys nil)
             (build-prompt
              (lambda ()
                (if keys
                    (format "Key (\"\" to exit) %s: "
                            (mapconcat #'identity (reverse keys) ";"))
                  "Key (\"\" to exit): "))))
        (let ((key (funcall prompt (funcall build-prompt))))
          (while (org-string-nw-p key)
            (push (cdr (assoc "=key=" (cdr (assoc key table)))) keys)
            (setq key (funcall prompt (funcall build-prompt)))))
        keys))))


;; * An Emacs 28 compatible completing-read inserter
;; This uses annotations and grouping

(defface org-ref-cite-annotate-cite-key-face
  `((t (:inherit org-cite)))
  "Face for an annotated style.")


(defun org-ref-cite--complete-key-28 (&optional multiple)
  "Prompt for a reference key and return a citation reference string.

When optional argument MULTIPLE is non-nil, prompt for multiple keys, until one
of them is nil.  Then return the list of reference strings selected.

This uses bibtex-completion and is compatible with
`org-cite-register-processor'. You can use it in an insert
processor like this:

 (org-cite-register-processor 'my-inserter
  :insert (org-cite-make-insert-processor #'org-ref-cite--complete-key
					  #'org-ref-cite-select-style))

 (setq org-cite-insert-processor 'my-inserter

This is an Emacs-28 compatible complete-key function. It provides
annotation on a modified version of the
bibtex-completion-candidates."

  (let* ((table (cl-loop for candidate in (bibtex-completion-candidates) collect
			 (append (list (cdr (assoc "=key=" candidate)))
				 candidate)))
         (prompt
          (lambda (text)
	    (completing-read "Select: "
			     (lambda (str pred action)
			       (if (eq action 'metadata)
				   `(metadata
				     ;; I use this closure since we need the table to do the annotation.
				     (annotation-function . (lambda (s)
							      (let ((w (max 0 (+  (- 5 (length s)) 30))))
								(concat (make-string w ? )
									(propertize
									 (or
									  (cl-second (assoc s table))
									  " ")
									 'face 'org-ref-cite-annotate-cite-key-face)))))
				     (cycle-sort-function . identity)
				     (display-sort-function . (lambda (candidates)
								(sort candidates #'string<)))
				     (group-function . (lambda (key transform)
							 (if transform
							     key
							   (if (> (length key) 1)
							       (substring key  0 1)
							     "empty")))))
				 (complete-with-action action table str pred)))))))
    (if (null multiple)
	(org-string-nw-p (funcall prompt "Key: "))
      (let* ((keys nil)
             (build-prompt
	      (lambda ()
                (if keys
                    (format "Key (\"\" to exit) %s: "
                            (mapconcat #'identity (reverse keys) ";"))
                  "Key (\"\" to exit): "))))
        (let ((key (funcall prompt (funcall build-prompt))))
          (while (org-string-nw-p key)
            (push key keys)
            (setq key (funcall prompt (funcall build-prompt)))))
        keys))))


(provide 'org-ref-cite-core)

;;; org-ref-cite-core.el ends here
