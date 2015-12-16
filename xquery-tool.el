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

(defcustom xquery-tool-result-buffer-name "*xquery-tool results*"
  "Name of buffer to show results of xqueries in."
  :group 'xquery-tool)

(defcustom xquery-tool-temporary-xquery-file-name "xquery-temp.xq"
  "Filename for storing one-off xqueries.
It will be created in `temporary-file-directory'."
  :group 'xquery-tool)

;; (defcustom xquery-tool-temporary-indexed-xml-file-name "xquery-temp-indexed.xml"
;;   "Filename for storing an indexed version of the xml you're querying.
;; It will be created in `temporary-file-directory'."
;;   :group 'xquery-tool)

(defcustom xquery-tool-temporary-xml-file-name "xquery-temp.xml"
  "Filename for storing xml that is not in a file (region, e.g.).
It will be created in `temporary-file-directory'."
  :group 'xquery-tool)

(defcustom xquery-tool-link-namespace "tmplink"
  "Name of namespace to use for linking xquery results to original file."
  :group 'xquery-tool)

(defvar xquery-tool-xquery-history nil
  "A var to hold the history of xqueries.")

(defvar xquery-tool-last-xquery-on-full-file nil
  "If non-nil, the last xquery was run on a full xml document.
Important for knowing when to regenerate the xml source.")

(defun xquery-tool-xq-file (&optional fn)
  "Get full path to temporary file with name FN.
This is where we store the xquery.  Default for FN is
`xquery-tool-temporary-xquery-file-name'."
  (let ((fn (or fn xquery-tool-temporary-xquery-file-name)))
    (expand-file-name fn temporary-file-directory)))

(defun xquery-tool-indexed-xml-file (&optional fn)
  "Get full path to temporary file with name FN.
This is where the indexed xml file is stored.  Default for FN is
`xquery-tool-temporary-indexed-xml-file-name'."
  (let ((fn (or fn (format "%s_%s" (file-name-nondirectory (buffer-file-name)) (emacs-pid)))))
    (expand-file-name fn temporary-file-directory)))

(defun xquery-tool-xml-file (&optional fn)
    "Get full path to temporary file with name FN.
This is where temporary xml docs are stored that don't have their
own files.  Default for FN is
`xquery-tool-temporary-xml-file-name'."
    (let ((fn (or fn xquery-tool-temporary-xml-file-name)))
  (expand-file-name fn temporary-file-directory)))

;;;###autoload
(defun xquery-tool-query (xquery xml-thing &optional save-namespace)
  "Run the query XQUERY on the xml contained in XML-THING.

XQUERY can be:
 - a string: then that is used to compose an xquery;
 - a filename: then that is taken as input without further processing.

XML-THING can be:
- a buffer containing an xml document (if a region is active, it
  will operate only on that region);
- a path to an xml document.

To use this function, you might first have to customize the
`xquery-tool-java-binary' and `xquery-tool-saxonb-jar'
settings (M-x customize-group RET xquery-tool).

If SAVE-NAMESPACE is not nil (or you use a prefix arg in the
interactive call), then the attributes added to enable tracking
of elements in the source document are not deleted."
  (interactive
   (let ((xquery (read-string "Your xquery: " nil 'xquery-tool-xquery-history)))
     (list xquery (current-buffer) current-prefix-arg)))
  (let ((target-buffer (get-buffer-create "*xpath tool buffer*"))
	(xml-source (cond ((bufferp xml-thing) (buffer-file-name xml-thing))
			  ((and (stringp xml-thing) (file-exists-p xml-thing)) xml-thing)
			  (t (error "Can not parse argument: %s" xml-thing))))
	(xquery-file
	 (if (file-exists-p xquery)
	     xquery
	   (xquery-tool-setup-xquery-file xquery (buffer-file-name))))
	process-status)
    (setq xml-thing (xquery-tool-parse-to-shadow xml-source))
    (with-current-buffer target-buffer
      (if buffer-read-only (read-only-mode -1))
      (erase-buffer))
    (setq process-status
	  (call-process "java" ;; program
			xml-thing;; infile
			target-buffer;; destination
			nil;; update display
			;; args
			"-classpath" xquery-tool-saxonb-jar
			"net.sf.saxon.Query"
			"-s:-"
			(format "-q:%s" xquery-file)))
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
	      (when (or (string= (xmltok-attribute-prefix xatt) xquery-tool-link-namespace) (string= (xmltok-attribute-local-name xatt) "start"))
		(make-text-button
		 (1+ xmltok-start)
		 xmltok-name-end
		 'help-echo "Try to open corresponding element."
		 'action 'xquery-tool-get-and-open-location
		 'follow-link t
		 'target (xmltok-attribute-value xatt))))
	    ;; remove all traces of xquery-tool-link-namespace namespace thing
	    (unless save-namespaces
	      (xquery-tool-forget-namespace xquery-tool-link-namespace))
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
namespace (not elements, though).

When this function is called, it expects that position in buffer
and the values xmltok-* have been set up by `xmltok-forward'."
  (let* ((el-end (point-marker))
	 (namespace (or namespace xquery-tool-link-namespace))
	 (namespace-candidates;; make an alist to look up namespace
	  (cl-remove-if 'null
			(append
			 (mapcar  (lambda (x) (when (string= (xmltok-attribute-prefix x) namespace)
						(cons (xmltok-attribute-prefix x) x))) xmltok-attributes)
			 (mapcar  (lambda (x) (when (string= (xmltok-attribute-local-name x) namespace)
						(cons (xmltok-attribute-local-name x) x))) xmltok-namespace-attributes))))
	 delete-me)
    (save-excursion
      (setq delete-me (pop namespace-candidates))
      (goto-char (xmltok-attribute-name-start (cdr delete-me)))
      (delete-region (xmltok-attribute-name-start (cdr delete-me)) (1+ (xmltok-attribute-value-end (cdr delete-me))))
      (just-one-space)
      (save-match-data
	(save-excursion
	  (goto-char xmltok-start)
	  (when (re-search-forward "\\s-+>" el-end t)
	    (goto-char (- (point) (length (match-string 0))))
	    (delete-char (1- (length (match-string 0)))))))
      (when namespace-candidates
	(goto-char xmltok-start)
	(xmltok-forward)
	(xquery-tool-forget-namespace namespace)))))


(defun xquery-tool-setup-xquery-file (xquery &optional xml-file)
  "Construct an xquery file containing XQUERY.

If XML-FILE is specified, look at that for namespace declarations."
  (let ((tmp (find-file-noselect (xquery-tool-xq-file)))
	namespaces)
    (with-current-buffer tmp
      (erase-buffer))
    (when xml-file
      (when (null (file-exists-p xml-file))
	(error (format "Could not access xml-file %s." xml-file)))
      (with-current-buffer (find-file-noselect xml-file)
	(save-excursion
	  (save-restriction
	    (widen)
	    (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
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
  "Fix xml in xmlfile so that it can be traced from xquery.
Currently, for each start-tag or empty element in XMLFILE, this
add an @`xquery-tool-link-namespace':start attribute."
  (let* ((original (if (and xmlfile (file-exists-p xmlfile)) (find-file-noselect xmlfile) (current-buffer)))
	 (start (if (use-region-p) (region-beginning) (point-min)))
	 (end (if (use-region-p) (region-end) (point-max)))
	 (original-file (buffer-file-name original))
	 (tmp-file (xquery-tool-indexed-xml-file))
	(new-namespace (format " xmlns:%s=\"potemkin\"" xquery-tool-link-namespace))
	(factor (- (length new-namespace) (if (use-region-p) (1- (region-beginning)) 0))))
    (if (or
	 (null (file-exists-p tmp-file))
	 (file-newer-than-file-p original-file tmp-file)
	 (use-region-p)
	 (null xquery-tool-last-xquery-on-full-file))
	(with-temp-buffer
	  (if (file-exists-p tmp-file) (delete-file tmp-file))
	  (insert-buffer-substring-no-properties original start end)
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
				(insert (format " %s:start=\"%s#%s\"" xquery-tool-link-namespace original-file xmltok-start))
				(point)))))))
	  (while (xmltok-forward)
	    (when (member xmltok-type '(start-tag empty-element))
	      (save-excursion
		(goto-char xmltok-name-end)
		(setq factor
		      (+ factor
			 (* -1 (- (point)
				  (progn
				    (insert (format " %s:start=\"%s#%s\"" xquery-tool-link-namespace original-file (- xmltok-start factor)))
				    (point)))))))))
	  (write-file tmp-file nil)))
    ;; remember if run on region or not
    (if (use-region-p)
	(setq xquery-tool-last-xquery-on-full-file nil)
      (setq xquery-tool-last-xquery-on-full-file t))
    tmp-file))

(provide 'xquery-tool)

;;; xquery-tool.el ends here
