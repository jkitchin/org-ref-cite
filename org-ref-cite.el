;;; org-ref-cite.el --- A library to use org-cite like org-ref

;; Copyright(C) 2021 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref-cite
;; Version: 1.0
;; Keywords: org-mode, cite
;; Package-Requires: ((ivy-bibtex "0") (org-ref-cite-core "0"))
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
;; This package sets up the `org-ref-cite' citation processor. It provides
;; processors for activation, following, insertion and export. The target
;; backend for export is LaTeX with the natbib package and bibtex citation
;; processor.
;;
;; The default setup uses `bibtex-completion' for candidates with `ivy' for
;; completion. See readme.org for details on how to use modular pieces of this
;; library in your own processor.
;;
;;; Code:

(require 'oc)

(require 'org-ref-cite-advice)
(require 'org-ref-cite-activate)
(require 'org-ref-cite-insert)
(require 'org-ref-cite-follow)
(require 'org-ref-cite-export)
(require 'org-ref-cite-compat)
(require 'org-ref-cite-utils)


(ivy-set-display-transformer
 ' org-ref-cite-insert-processor
 'ivy-bibtex-display-transformer)


(org-cite-register-processor 'org-ref-cite
  :activate #'org-ref-cite-activate
  :follow #'org-ref-cite-follow
  :insert #'org-ref-cite-insert-processor
  :export-bibliography #'org-ref-cite-export-bibliography
  :export-citation #'org-ref-cite-export-citation
  :export-finalizer #'org-ref-cite-use-package
  :cite-styles (org-ref-cite-get-cite-styles))

(provide 'org-ref-cite)
;;; org-ref-cite.el ends here
