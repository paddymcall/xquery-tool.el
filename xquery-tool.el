;;; xquery-tool.el --- A simple interface to saxonb's xquery.

;; Copyright (C) 2015 Patrick McAllister

;; Author: Patrick McAllister <pma@rdorte.org>
;; Keywords: xml, xquery, emacs
;; URL: https://github.com/paddymcall/xquery-tool.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program lets you run an xquery against a file, via saxonb.

;; If the result contains nodes, it tries to link the results back to
;; the original file.

;; To use, customize the `xquery-tool-java-binary' and
;; `xquery-tool-saxonb-jar' settings (M-x customize-group RET
;; xquery-tool), and then call `xquery-tool-query' from a buffer
;; visiting an xml document.

;;; Code:

(require 'xmltok)
(require 'url-parse)

(defcustom xquery-tool-java-binary "/usr/bin/java"
  "Command name to invoke the Java Binary on your system."
  :group 'xquery-tool
  :type '(file))

;; (setq xquery-tool-java-binary "/usr/bin/java")

(defcustom xquery-tool-saxonb-jar "/usr/share/java/saxonb.jar"
  "Full path of the saxonb.jar on your system."
  :group 'xquery-tool
  :type '(file))

(defun xquery-tool-query (xquery xml-thing &optional save-namespace)
  "Run the query XQUERY on the xml contained in XML-THING.

XQUERY can be:
 - a string: then that is used to compose an xquery;
 - a filename: then that is taken as input without further processing.

XML-THING can be:
- a buffer containing an xml document; (region?)
- a filename to an xml document.

To use this function, you might first have to customize the
`xquery-tool-java-binary' and `xquery-tool-saxonb-jar'
settings (M-x customize-group RET xquery-tool).

If SAVE-NAMESPACE is not nil (or you use a prefix arg in the
interactive call), then the attributes added to enable tracking
of elements in the source document are not deleted."
  (interactive
   (let ((xquery (read-from-minibuffer "Your xquery: "))
	 (source-buffer (find-file
			 (read-file-name (format "Run on this file (default: %s): " (file-name-nondirectory (buffer-file-name))) nil (buffer-file-name)))))
     (when (buffer-modified-p source-buffer)
       (if (yes-or-no-p "Save buffer first?")
	   (save-buffer source-buffer)
	 (error "Can't work on modified buffer")))
     (list xquery source-buffer current-prefix-arg)))
  (let ((target-buffer (get-buffer-create "*xpath tool buffer*"))
	(xml-source (cond ((bufferp xml-thing) (buffer-file-name xml-thing))
			  ((and (file-exists-p xml-thing) (file-accessible-directory-p xml-thing)) xml-thing)))
	(xquery-file
	 (if (file-exists-p xquery)
	     xquery
	   (xquery-tool-setup-xquery-file xquery (buffer-file-name xml-thing))))
	process-status)
    (setq xml-thing (xquery-tool-parse-to-shadow xml-source))
    (with-current-buffer target-buffer
      (if buffer-read-only (read-only-mode -1))
      (erase-buffer))
    (setq process-status
	  (call-process "java" ;; program
		  xquery-file				    ;; infile
		  target-buffer;; destination
		  nil;; update display
		  ;; args
		  "-classpath" xquery-tool-saxonb-jar
		  "net.sf.saxon.Query"
		  (format "-s:%s" xml-thing)
		  "-"))
    (if (= 0 process-status)
	(message "Success.")
      (message "Something went wrong."))
    (with-current-buffer target-buffer
      (goto-char (point-min))
      (xquery-tool-setup-xquery-results target-buffer save-namespace)
      (set-buffer-modified-p nil)
      (read-only-mode))
    (switch-to-buffer-other-window target-buffer)))

(defun xquery-tool-setup-xquery-results (target-buffer &optional save-namespaces)
  "Try to link the results in TARGET-BUFFER to the src buffer.

If SAVE-NAMESPACES is nil (the default), then the shadow
namespaces used for constructing the links are removed."
  (let ((current-pos (make-marker))
	teied-item
	teied-candidates)
    (with-current-buffer target-buffer
      (save-excursion
	(goto-char (point-min))
	(while (xmltok-forward)
	  (cond
	   ((and (member xmltok-type '(start-tag empty-element)) (or xmltok-namespace-attributes xmltok-attributes))
	    (set-marker current-pos (point))
	    (dolist (xatt xmltok-attributes)
	      (when (or (string= (xmltok-attribute-prefix xatt) "teied") (string= (xmltok-attribute-local-name xatt) "start"))
		(make-text-button
		 (1+ xmltok-start)
		 xmltok-name-end
		 'help-echo "Try to open corresponding element."
		 'action 'xquery-tool-get-and-open-location
		 'follow-link t
		 'target (xmltok-attribute-value xatt))))
	    ;; remove all traces of teied namespace thing
	    (unless save-namespaces
	      (xquery-tool-forget-namespace "teied"))
	    (goto-char current-pos))))
	(set-marker current-pos nil)))))

(defun xquery-tool-get-and-open-location (position)
  "Find the target to open at POSITION."
  (let ((target (get-text-property position 'target)))
    (if (and target (url-generic-parse-url target))
	(xquery-tool-open-location target)
      (error "This does not look like an url: %s" target))))


(defun xquery-tool-open-location (url)
  "Open the location specified by URL."
  (let ((file-name (url-filename (url-generic-parse-url url)))
	(location (string-to-number (url-target (url-generic-parse-url url)))))
    (if	(find-file-other-window file-name)
	(cond
	 ((and (>= location (point-min)) (<= location (point-max)))
	  (goto-char location))
	 ((buffer-narrowed-p)
	  (if (yes-or-no-p "Requested location is outside current scope, widen? ")
	      (progn (widen)
		    (xquery-tool-open-location url))))
	 (t (error "Can't find location %s in this buffer" location)))
      (error "Can't find file %s" file-name))))


(defun xquery-tool-forget-namespace (&optional namespace)
  "Remove all references to a namespace NAMESPACE.

Gets rid of namespace declarations and attributes under that
namespace (not elements, though)."
  (let* ((namespace (or namespace "teied"))
	 (namespace-candidates;; make an alist to look up namespace
	  (cl-remove-if 'null
			(append
			 (mapcar  (lambda (x) (when (string= (xmltok-attribute-prefix x) namespace)
						(cons (xmltok-attribute-prefix x) x))) xmltok-attributes)
			 (mapcar  (lambda (x) (when (string= (xmltok-attribute-local-name x) namespace)
						(cons (xmltok-attribute-local-name x) x))) xmltok-namespace-attributes))))
	 delete-me)
    (save-excursion
      (save-restriction
	(setq delete-me (pop namespace-candidates))
	(goto-char (xmltok-attribute-name-start (cdr delete-me)))
	(delete-region (xmltok-attribute-name-start (cdr delete-me)) (1+ (xmltok-attribute-value-end (cdr delete-me))))
	(just-one-space)
	(when namespace-candidates
	  (goto-char xmltok-start)
	  (xmltok-forward)
	  (xquery-tool-forget-namespace namespace))))))



(defun xquery-tool-setup-xquery-file (xquery &optional xml-file)
  "Construct an xquery file containing XQUERY.

If XML-FILE is specified, look at that for namespace declarations."
  (let ((tmp (find-file-noselect (make-temp-file "xquery-tool")))
	namespaces)
    (when xml-file
      (when (null (file-exists-p xml-file))
	(error (format "Could not access xml-file %s." xml-file)))
      (with-current-buffer (find-file-noselect xml-file)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (and (xmltok-forward) (not (eq xmltok-type 'start-tag))) t)
	    (dolist (naspa-att xmltok-namespace-attributes)
	      (let ((naspa-val (xmltok-attribute-value naspa-att))
		    (naspa-name (xmltok-attribute-local-name naspa-att)))
		(with-current-buffer tmp
		  (if (string= naspa-name "xmlns")
		      (insert (format "declare default element namespace \"%s\";\n"
				      naspa-val))
		    (insert (format "declare namespace %s=\"%s\";\n"
				    naspa-name naspa-val))))))
	    ))))
    (with-current-buffer tmp
      (insert xquery)
      (save-buffer)
      (buffer-file-name (current-buffer)))))

;; Based on `tei-edit-parse-to-shadow'.
(defun xquery-tool-parse-to-shadow (&optional xmlfile)
  "For each start-tag or empty element in XMLFILE, add a @teied:start attribute."
  (let* ((original (if (and xmlfile (file-exists-p xmlfile)) (find-file-noselect xmlfile) (current-buffer)))
	 (original-file (buffer-file-name original))
	 (tmp-file (expand-file-name
		    (format "%s_%s" (emacs-pid) (file-name-nondirectory original-file))
		    temporary-file-directory))
	(new-namespace " xmlns:teied=\"potemkin\"")
	(factor (length new-namespace)))
    (if (or
	 (null (file-exists-p tmp-file))
	 (file-newer-than-file-p original-file tmp-file))
	(with-current-buffer (get-buffer-create "*potemkin*")
	  (switch-to-buffer (current-buffer))
	  (erase-buffer)
	  (insert-buffer-substring-no-properties original)
	  (goto-char (point-min))
	  ;; set namespace on first start tag (hoping it's the root element)
	  (while (and (xmltok-forward) (not (eq xmltok-type 'start-tag)) t))
	  (save-excursion
	    (goto-char xmltok-name-end)
	    (insert new-namespace)
	    (setq factor
		  (+ factor
		     (* -1 (- (point)
			      (progn
				(insert (format " teied:start=\"%s#%s\"" original-file xmltok-start))
				(point)))))))
	  (while (xmltok-forward)
	    (when (member xmltok-type '(start-tag empty-element))
	      (save-excursion
		(goto-char xmltok-name-end)
		(setq factor
		      (+ factor
			 (* -1 (- (point)
				  (progn
				    (insert (format " teied:start=\"%s#%s\"" original-file (- xmltok-start factor)))
				    (point)))))))))
	  (write-file tmp-file)))
    tmp-file))

(provide 'xquery-tool)

;;; xquery-tool.el ends here
