;;; org-ref-cite-ref.el --- add styles for references
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
;;
;;; Commentary:
;;
;; Add styles for cross-references. A cross-reference is like a
;; citation-reference but it is a reference to a location within a document.
;;
;; This is an experiment to see if we can get a total replacement for org-ref
;; reference links by leveraging the org-cite syntax.
;;
;; Provides:
;; 1. activation to use font-lock to differentiate these from citations.
;; 2. An insert function
;; 3. Following behavior is currently not defined
;; 4. Exporting
;;
;; Insertion can be "clever" if `org-ref-cite-clever' is set to non-nil. This
;; means it will add prefix text based on the label you are referencing. The
;; label prefix is mapped to reference prefix in `org-ref-cite-clever-prefixes'.
;; For example, adding a cross-refence to the label tab-data will put a prefix
;; of Table before the reference.

;;; Code:

(require 'org-ref-cite-core)

(defcustom org-ref-cite-ref-styles '(((("ref" "r")) . "\\ref")
				     ((("ref" "r") ("eq")) . "\\eqref")
				     ((("ref" "r") ("page")) . "\\pageref")
				     ((("ref" "r") ("name")) . "\\nameref")
				     ((("ref" "r") ("auto")) . "\\autoref")
				     ((("ref" "r") ("c")) . "\\cref")
				     ((("ref" "r") ("C")) . "\\Cref"))
  "List of cross-ref-styles."
  :group 'org-ref-cite)


(defcustom org-ref-cite-ref-default-style
  "/ref"
  "Default cross-reference style."
  :group 'org-ref-cite)

;; I am not sure this is the right thing to do. It makes it work well for the
;; current export engine by reusing code, but will mean you might see these
;; styles on citations, which is unlikely to be what you want.
(setq org-ref-cite-styles (append org-ref-cite-styles org-ref-cite-ref-styles))


(defface org-ref-cite-ref-face
  `((t (:inherit org-cite :foreground "dark red")))
  "Face for a cross-reference.")


(defface org-ref-cite-invalid-ref-face
  `((t (:inherit org-ref-cite-ref-face :foreground "red")))
  "Face for an invalid cross-reference.")


(defcustom org-ref-cite-clever t
  "If non-nil, add prefixes to cross-references.
The prefixes are defined in `org-ref-cite-clever-prefixes'."
  :group 'org-ref-cite)


(defcustom org-ref-cite-clever-prefixes
  '(("eq" . "Eq.")
    ("sec" . "Section")
    ("fig" . "Figure")
    ("tab" . "Table")
    ("lst" . "Listing"))
  "Alist of clever prefixes.")


(defcustom org-ref-cite-ref-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "RET") 'org-ref-cite-ref-follow)
    ;; this seems like it is causing issues when you click at the end of a cite. taking it out for now.
    ;; for some reason it is inserting a line?
    (define-key map (kbd "<mouse-1>") 'org-ref-cite-ref-follow)
    map)
  "A keymap for `org-cite' cross-references."
  :group 'org-ref-cite)


;; * Inserting a cross-reference
;; ** Cross-reference candidates - labels

(defvar org-ref-cite-label-regexps
  '(;; CUSTOM_ID in a heading
    ":CUSTOM_ID:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; #+name
    "^\\s-*#\\+name:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; labels in latex
    "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}")
  "List of regular expressions to labels.
The label should always be in group 1.")


(defun org-ref-cite-get-labels ()
  "Return a list of referenceable labels in the document.
You can reference:
A NAME keyword
A CUSTOM_ID property on a heading
A LaTeX label

See `org-ref-cite-label-regexps' for the patterns that find these.

I am not putting label links here for now to keep it totally
separate from `org-ref'. I think that the NAME keyword is
adequate for figures and tables, and CUSTOM_ID is ok for
headings. You can always fall back on the \\label syntax if you
need to.

Returns a list of cons cells (label . context).

It is important for this function to be fast, since we use it in
font-lock."
  (let ((rx (string-join org-ref-cite-label-regexps "\\|"))
	(labels '())
	context)
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward rx nil t)
	 (setq context (buffer-substring
			(save-excursion (forward-line -1) (point))
			(save-excursion (forward-line +2) (point))))
	 (cl-pushnew (cons (match-string-no-properties 1)
			   ;; This attempts to pad the context.
			   (string-join
			    (mapcar (lambda (s)
				      (concat (make-string 20 ? ) s))
				    (split-string context "\n"))
			    "\n"))
		     labels))))
    ;; reverse so they are in the order we find them.
    (reverse labels)))


(defun org-ref-cite-annotate-label (s)
  "Annotation function for a label.
Argument S is the label.
Note that labels is dynamically bound."
  (cdr (assoc s labels)))


(defun org-ref-cite-clever-prefix (label)
  "Return a clever prefix for LABEL."
  (or (cl-loop for (label-prefix . prefix) in org-ref-cite-clever-prefixes
	       when
	       (string-prefix-p label-prefix label)
	       return prefix)
      ""))


(defun org-ref-cite-ref-annotate-style (s)
  "Annotation function for selecting style.
Argument S is the style candidate string."
  (let* ((w (+  (- 5 (length s)) 20))
	 (backend (org-ref-cite-guess-backend)))
    (concat (make-string w ? )
	    (propertize
	     (org-trim
	      (org-export-string-as (format "[cite/%s:@key]"
					    s)
				    t))
	     'face 'org-ref-cite-ref-face))))


(defun org-ref-cite-ref-insert (&optional set-style)
  "Function for inserting a citationr reference or a cross-reference.
With no prefix arg insert citation reference.
With one prefix arg, insert a cross-reference.
With two prefix args, insert a cross-reference and choose the style.
SET-STYLE is the prefix arg."
  (interactive "P")
  (let* ((labels (org-ref-cite-get-labels))
	 (label (completing-read "Label: " (lambda (str pred action)
					     (if (eq action 'metadata)
						 `(metadata
						   ;; I use this closure since
						   ;; we need the table to do
						   ;; the annotation.
						   (annotation-function . org-ref-cite-annotate-label))
					       (complete-with-action action labels str pred)))))
	 (style (cond
		 ((or (string-prefix-p "eq-" label)
		      (string-prefix-p "eq:" label))
		  "/ref/eq")
		 (t
		  org-ref-cite-ref-default-style)))
	 (style-strings))
    (when set-style
      (setq style-strings (apply #'append
				 (cl-loop for ((style substyle) . cmd) in org-ref-cite-ref-styles
					  collect
					  (apply #'append
						 (cl-loop for st in style collect
							  (if substyle
							      (cl-loop for subst in substyle collect
								       (concat "/" st "/" subst))
							    (list (concat "/" st))))))))
      (setq style (completing-read "Ref style: "
				   (lambda (str pred action)
				     (if (eq action 'metadata)
					 `(metadata
					   ;; I use this closure since we need
					   ;; the table to do the annotation.
					   (annotation-function . org-ref-cite-annotate-style))
				       (complete-with-action action style-strings str pred))))))
    (insert (format "[cite%s:%s@%s]" style
		    (if org-ref-cite-clever
			(concat  (org-ref-cite-clever-prefix label) " ")
		      "")
		    label))))


(defun org-ref-cite-ref-insert-processor (context arg)
  "Function for inserting a citation or a cross-reference.
With one prefix ARG, insert a cross-reference
With two prefix ARG delete reference/cite at point.
Argument CONTEXT is an org element at point, usually a citation
or citation-reference.
This is called by `org-cite-insert'."
  (interactive (list (org-element-context) current-prefix-arg))

  ;; I do this here in case you change the actions after loading this, so that
  ;; it should be up to date.
  (ivy-set-actions
   'org-cite-insert
   org-ref-cite-alternate-insert-actions)

  (cond
   ;; the usual case where we insert a ref
   ((null arg)
    (bibtex-completion-init)
    (let* ((bibtex-completion-bibliography (org-cite-list-bibliography-files))
	   (candidates (bibtex-completion-candidates)))
      (ivy-read "BibTeX entries: " candidates
		:action (lambda (candidate)
			  (org-ref-cite-insert-citation
			   (cdr (assoc "=key=" (cdr candidate))) arg)))))

   ;; insert a cross-reference
   ((= (prefix-numeric-value  arg) 4)
    (org-ref-cite-ref-insert))

   ;; insert cross-reference with style
   ((= (prefix-numeric-value  arg) 16)
    (org-ref-cite-ref-insert t))))

;; * Activating a cross-reference

(defun org-ref-cite-ref-p (reference)
  "Return non-nil if REFERENCE is a cross-reference."
  (let* ((style (org-element-property :style reference))
	 (ref-styles (cl-loop for ((s v) . _) in org-ref-cite-ref-styles
			      append
			      (cl-loop for ms in s
				       when (null v) append (list ms)
				       append
				       (cl-loop for vs in v append
						(list (format "%s/%s"
							      (or ms "")
							      (or vs ""))))))))
    (member style ref-styles)))


(defun org-ref-cite-ref-replace ()
  "Replace the key at point.
A suggestion for a close key will be provided."
  (interactive)
  (let* ((datum (org-element-context))
	 (reference (if (eq 'citation-reference (org-element-type datum))
			datum
		      (error "Not on a cross-reference")))
	 (label (org-element-property :key reference))
	 (beg (org-element-property :begin reference))
	 (end (org-element-property :end reference))
	 (prefix (org-element-property :prefix reference))
	 (labels (mapcar 'car (org-ref-cite-get-labels)))
	 (suggestions (or (org-cite-basic--close-keys label labels) labels))
	 (choice (completing-read "Replace with: " suggestions))
	 (cp (point)))

    (setf (buffer-substring beg end)
	  (org-element-interpret-data
	   `(citation-reference (:key ,choice :prefix ,prefix))))
    (goto-char cp)))


(defun org-ref-cite-activate-cross-reference (reference)
  "Activate a cross-reference.
REFERENCE will actually be a citation object.
We need to define:
1. the face
2. a tooltip."
  (when (org-ref-cite-ref-p reference)
    (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
      ;; Set the face
      (put-text-property beg end 'face 'org-ref-cite-ref-face)
      (put-text-property beg end 'help-echo "This is a cross-reference")
      (put-text-property beg end 'keymap nil)
      (put-text-property beg end 'org-ref-cite-crossref t)

      ;; activate each label
      (let ((labels (org-ref-cite-get-labels)))
	(cl-loop for ref in (org-cite-get-references reference)
		 do
		 (let ((beg (org-element-property :begin ref))
		       (end (org-element-property :end ref)))
		   ;; add  a tool tip
		   (put-text-property
		    beg end 'help-echo (lambda (window object position)
					 (with-selected-window window
					   (goto-char position)
					   (let* ((context (org-element-context))
						  (label (org-element-property :key context)))
					     (cdr (assoc label (org-ref-cite-get-labels)))))))

		   ;; ref keymap
		   (put-text-property beg end 'keymap org-ref-cite-ref-keymap)

		   ;; activate bad labels
		   (when (not (assoc (org-element-property :key ref) labels))
		     ;; set error face
		     (put-text-property beg end
					'face 'org-ref-cite-invalid-ref-face)
		     (put-text-property beg end
					'keymap (let ((map (copy-keymap org-mouse-map)))
						  (define-key map (kbd "RET")
						    'org-ref-cite-ref-replace)
						  (define-key map (kbd "<mouse-1>")
						    'org-ref-cite-ref-replace)

						  map))

		     (put-text-property beg end
					'help-echo "Invalid cross-reference. Press RET to replace."))))))))

;; You should make sure the cross-reference is activated last. It undoes some
;; things that are done first, e.g. replacing the bad-key face, since it is not
;; a key in a bibtex file.
(add-to-list 'org-ref-cite-activation-functions
	     'org-ref-cite-activate-cross-reference
	     t)


;; * Following a cross-reference

(defun org-ref-cite-ref-follow (&optional datum _)
  "Follow function consistent with the org-cite API.
Optional argument DATUM: The element at point.
If you follow on the style part you will be prompted for a key to act on."
  (interactive)
  (if (get-text-property (point) 'org-ref-cite-crossref)

      ;; follow a crossref
      (when (eq 'citation-reference (org-element-type datum))
	(let ((label (org-element-property :key datum))
	      (rx (string-join org-ref-cite-label-regexps "\\|")))
	  (goto-char (point-min))
	  (catch 'found
	    (while (re-search-forward rx)
	      (when (string= (match-string-no-properties 1) label)
		(goto-char (match-beginning 1))
		(throw 'found label))))))

    ;; on a regular citation or reference. This is just a copy of the code for
    ;; the original follow. maybe an advice would be better?
    (when (null datum) (setq datum (org-element-context)))
    (if (eq 'citation-reference (org-element-type datum))
	(org-ref-cite-citation-reference/body)
      ;; at style part or end part
      (if (= (point) (org-element-property :end datum))
	  (org-return)
	(let* ((bibtex-completion-bibliography (org-cite-list-bibliography-files))
	       (candidates (bibtex-completion-candidates))
	       (follow-alist (mapcar (lambda (x)
				       (cons (cdr (assoc "=key=" (cdr x))) (car x)))
				     candidates))
	       (refs (org-cite-get-references datum))
	       (keys (mapcar (lambda (ref) (org-element-property :key ref)) refs))
	       (key (completing-read "Citation key: "
				     (lambda (str pred action)
				       (if (eq action 'metadata)
					   `(metadata
					     ;; I use this closure since we need
					     ;; the table to do the annotation.
					     (annotation-function . org-ref-cite-annotate-follow-key))
					 (complete-with-action action keys str pred))))))
	  (search-forward (concat "@" key))
	  (goto-char (match-beginning 0))
	  (org-ref-cite-citation-reference/body))))))


;; * Exporting a cross-reference

;; This is a minor change, which just conditionally exports cross-references
;; separate from citations.
(defun org-ref-cite-ref-export-citation (citation style _ info)
  "Export CITATION object.
Argument STYLE The style as a cons cell from the exporter.
Optional argument INFO plist from the exporter."
  (let ((cmd (org-ref-cite--style-to-command style)))
    ;; We have a cross-reference
    (if (member cmd (mapcar 'cdr org-ref-cite-ref-styles))
	(format "%s%s%s"
		;; This is a little hacky, but I don't see a better way. A
		;; prefix only makes sense on the first reference I think, and I
		;; never use multiple references like this.
		(if-let ((prefix (org-element-property :prefix (car (org-cite-get-references citation)))))
		    (org-export-data prefix info)
		  "")
		cmd
		(org-ref-cite--build-arguments citation))
      ;; This must be a regular citation.
      (concat (org-ref-cite--style-to-command style)
	      (org-ref-cite--build-optional-arguments citation info)
	      (org-ref-cite--build-arguments citation)))))


;; * Advice overrides
;; I am not sure what the best thing to do here is, for now I am going to
;; override things with advice. An alternative is to make another processor.
(advice-add  'org-ref-cite-follow   :override 'org-ref-cite-ref-follow)
(advice-add  'org-ref-cite-export-citation  :override 'org-ref-cite-ref-export-citation)
(advice-add  'org-ref-cite-insert-processor  :override 'org-ref-cite-ref-insert-processor)


(provide 'org-ref-cite-ref)

;;; org-ref-cite-ref.el ends here
