;;; org-ref-cite-utils.el --- Utilities for org-ref-cite
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
;; This library provides some utilities, like opening the readme, extracting the
;; bibliography, and a debugging function.

;;;###autoload
(defun org-ref-cite-help ()
  "Open the `org-ref-cite' manual."
  (interactive)
  (find-file (expand-file-name
              "readme.org"
              (file-name-directory
               (find-library-name "org-ref-cite")))))


(defun org-ref-cite-get-unique-keys ()
  "Get the unique keys in the buffer."
  (delete-dups
   (org-element-map (org-element-parse-buffer) 'citation-reference
     (lambda (cr)
       (org-element-property :key cr)))))


;;;###autoload
(defun org-ref-cite-extract-bibliography (format output)
  "Extract the bibliography from the current org-file.
FORMAT: formatted bibtex
OUTPUT:  buffer copy file"
  (interactive (list (completing-read "Format: " '(formatted bibtex) nil t)
		     (completing-read "Output: " '(buffer copy file) nil t)))
  (let* ((keys (org-ref-cite-get-unique-keys))
	 (strings (cl-loop for key in keys collect
			   (pcase format
			     ("formatted"
			      (bibtex-completion-apa-format-reference key))

			     ("bibtex"
			      (save-window-excursion
				(bibtex-completion-show-entry (list key))
				(bibtex-beginning-of-entry)
				(bibtex-copy-entry-as-kill)
				(pop bibtex-entry-kill-ring)))))))
    (pcase output

      ("buffer"
       (let ((buf (get-buffer-create "*org-ref-cite-export*")))
	 (with-current-buffer buf
	   (erase-buffer)
	   (insert (string-join strings "\n\n"))
	   (when (string= format "bibtex") (bibtex-mode))
	   (pop-to-buffer buf))))

      ("copy"
       (kill-new (string-join strings "\n\n")))

      ("file"
       (let ((fname (read-file-name "File: " nil nil nil (concat (file-name-base (buffer-file-name))
								 (pcase format
								   ("bibtex" ".bib")
								   ("formatted" ".txt"))))))
	 (with-temp-file fname
	   (insert (string-join strings "\n\n"))))))))



(defun org-ref-cite-s-insert (s &rest args)
  "Insert S formatted with ARGS."
  (insert (apply #'format s args)))


;;;###autoload
(defun org-ref-cite ()
  "Generate a summary buffer of the current buffer.
This buffer shows the current setup, shows bad citations, etc."
  (interactive)

  (let* ((buf (get-buffer-create "*org-ref-cite*"))
	 (fname (buffer-file-name))
	 (data (org-element-parse-buffer))
	 (citations (org-element-map data 'citation 'identity))
	 (citation-references (org-element-map data 'citation-reference 'identity))
	 (bibtex-completion-bibliography (org-cite-list-bibliography-files))
	 (valid-keys (cl-loop for candidate in (bibtex-completion-candidates) collect
			      (cdr (assoc "=key=" (cdr candidate)))))
	 (bibliographystyle (cadr (assoc "BIBLIOGRAPHYSTYLE"
					 (org-collect-keywords '("BIBLIOGRAPHYSTYLE")))))
	 (bibliography (mapcar (lambda (f)
				 (format "[[%s]]" f))
			       (org-cite-list-bibliography-files)))
	 (natbib-options (or
			  ;; keyword settings
			  (cadr (assoc
				 "NATBIB_OPTIONS"
				 (org-collect-keywords
				  '("NATBIB_OPTIONS"))))

			  ;; settings in the org-file
			  (save-excursion
			    (goto-char (point-min))
			    (when (re-search-forward "\\\\usepackage\\[?\\([^]]*\\)\\]?{natbib}" nil t)
			      (match-string-no-properties 1)))

			  ;; default global settings
			  (cl-loop for (option package preview) in
				   (append org-latex-default-packages-alist org-latex-packages-alist)
				   when (string= package "natbib")
				   return option)))
	 (unique-keys (org-ref-cite-get-unique-keys))
	 (bad-references (cl-loop for ref in citation-references
				  if (not (member (org-element-property :key ref) valid-keys))
				  collect ref))
	 ;; I think we can only really support prefix on the first cite, and
	 ;; suffix on the last cite. This is a convention though, it is not part
	 ;; of the syntax, which allows a prefix and suffix on each
	 ;; citation-reference.
	 (questionable-cites (cl-loop for citation in citations
				      ;; look for prefix on entry after the first one
				      if (or (cl-loop for reference in (cdr (org-cite-get-references citation))
						      if (org-element-property :prefix reference)
						      return t)
					     ;; or suffix before the last one.
					     (cl-loop for reference in (butlast (org-cite-get-references citation))
						      if (org-element-property :suffix reference)
						      return t))
				      collect (list citation (buffer-substring-no-properties
							      (org-element-property :begin citation)
							      (org-element-property :end citation))))))

    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (org-ref-cite-s-insert "#+title: org-ref-cite report for %s\n\n" fname)
      (org-ref-cite-s-insert "- bibliographystyle :: %s\n" bibliographystyle)
      (org-ref-cite-s-insert "- bibliography :: %s\n" bibliography)
      (org-ref-cite-s-insert "- natbib options :: %s\n" natbib-options)

      ;; natbib options

      (org-ref-cite-s-insert "- # unique references :: %s\n" (length unique-keys))

      (when bad-references
	(insert "\n* Bad references\n\n")
	(cl-loop for ref in bad-references do
		 (insert "- ")
		 (insert-button (org-element-property :key ref)
				'face '(:foreground "red")
				'keymap (let ((map (make-sparse-keymap)))
					  (define-key map (kbd "<mouse-1>")
					    `(lambda ()
					       (interactive)
					       (save-window-excursion
						 (find-file ,fname)
						 (goto-char ,(org-element-property :begin ref))
						 (org-ref-cite-replace-key-with-suggestions))))
					  map)
				'mouse-face 'highlight
				'help-echo "Bad key, click to replace.")
		 (org-ref-cite-s-insert " (Possible keys: %s)\n" (org-cite-basic--close-keys
								  (org-element-property :key ref)
								  valid-keys))))

      (when questionable-cites
	(insert "* Questionable cites

These citations have prefix text on one or more references that are not the first one, or suffix text that is not on the last reference. This has undefined export behavior.\n\n")
	(cl-loop for (citation . cite) in questionable-cites do
		 (org-ref-cite-s-insert " - ")
		 (insert-button "[goto]"
				'face '(:foreground "red")
				'keymap (let ((map (make-sparse-keymap)))
					  (define-key map (kbd "<mouse-1>")
					    `(lambda ()
					       (interactive)
					       (find-file ,fname)
					       (goto-char ,(org-element-property :begin citation))))
					  map)
				'mouse-face 'highlight
				'help-echo "Bad key, click to replace.")
		 (org-ref-cite-s-insert " %s\n" cite)))

      (insert "\n* LaTeX setup\n\n")
      (cl-loop for s in `(,(format "org-latex-prefer-user-labels = %s"
				   org-latex-prefer-user-labels)
			  ,(format "bibtex-dialect = %s" bibtex-dialect)
			  ,(format "emacs-version = %s" (emacs-version))
			  ,(format "org-version = %s" (org-version))
			  ,(format "org-latex-pdf-process is defined as %s" org-latex-pdf-process)

			  ,(format "bibtex-completion installed = %s" (featurep 'bibtex-completion))
			  ,(format "bibtex-completion loaded = %s" (fboundp 'bibtex-completion-candidates)))
	       do
	       (insert "- " s "\n"))
      (insert (format "- org-latex-default-packages-alist\n"))
      (cl-loop for el in org-latex-default-packages-alist
	       do
	       (insert (format "  %S\n" el)))

      (if (null org-latex-packages-alist)
	  (insert "-  org-latex-packages-alist is nil\n")
	(insert "-  org-latex-packages-alist\n")
	(cl-loop for el in org-latex-packages-alist
		 do
		 (insert (format "  %S\n" el)))))

    (display-buffer-in-side-window buf '((side . right)))))

(provide 'org-ref-cite-utils)

;;; org-ref-cite-utils.el ends here
