;;; some tests for xquery-tool

;;; run from command line as `emacs --no-init-file --no-site-file -batch -l xquery-tool.el -l tests/xquery-tool-tests.el -f ert-run-tests-batch-and-exit'

(require 'format-spec)
(require 'xml)
(require 'subr-x)
(require 'ert)
(require 'xquery-tool)

(defun xquery-tool-get-test-dir ()
  "Find directory containing tests."
  (file-name-as-directory
   (concat
    (file-name-directory (or (find-lisp-object-file-name 'xquery-tool-query nil) "./"))
    "tests")))

(ert-deftest xquery-tool-test-get-namespace-candidates ()
    (let ((cases '(("<TEI xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\">" . nil)
		   ("<TEI xmlns:tmplink=\"potemkin\" tmplink:start=\"pramanavarttikavrtti.xml#3064\" xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\">" . ([31 38 44 46 75 t nil] [6 11 19 21 29 t nil]))
		   ("</TEI>" . nil)
		   ("<empty xmlns:tmplink=\"potemkin\" tmplink:start=\"pramanavarttikavrtti.xml#3064\" xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\" />" . ([33 40 46 48 77 t nil] [8 13 21 23 31 t nil])))))
      (dolist (case cases)
	(with-temp-buffer
	  (insert (car case))
	  (goto-char (point-min))
	  (xmltok-forward)
	  (should (equal (xquery-tool-get-namespace-candidates "tmplink") (cdr case)))))))

(ert-deftest xquery-tool-test-setup-xquery-results ()
  (let
      ((cases '(
		("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#98\">$5.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#302\">$7.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#507\">$8.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#715\">$4.50</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#898\">$6.95</price>" .
		 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price>$5.95</price>\n<price>$7.95</price>\n<price>$8.95</price>\n<price>$4.50</price>\n<price>$6.95</price>"))))
    (dolist (case cases)
      (with-temp-buffer
	(insert (car case))
	(should
	 (equal
	  (progn
	    (xquery-tool-setup-xquery-results)
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (cdr case)))))))

(ert-deftest xquery-tool-test-query ()
  "Check general functionality of `xquery-tool-query'.
Does not check the links, though."
  (xquery-tool-wipe-temp-files (directory-files temporary-file-directory 'full "^xquery-tool-") 'force)
  (let* ((tmp (find-file-noselect (make-temp-file "xquery-tool-test-src")))
	 (test-src (file-truename (expand-file-name "simple.xml" (file-name-directory (symbol-file 'xquery-tool-test-query)))))
	 (xquery-tool-omit-xml-declaration nil)
	 (cases
	  ;; default case
	  `(("//price" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<price>$5.95</price>
<price>$7.95</price>
<price>$8.95</price>
<price>$4.50</price>
<price>$6.95</price>")
	    ("//price" nil 'wrap "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xq-tool-results>
<price>$5.95</price>
<price>$7.95</price>
<price>$8.95</price>
<price>$4.50</price>
<price>$6.95</price>
</xq-tool-results>
")
	    ("//price" nil nil 'save-namespace ,(format-spec
						 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<price xmlns:tmplink=\"potemkin\"
       tmplink:start=\"file://%p#98\">$5.95</price>
<price xmlns:tmplink=\"potemkin\"
       tmplink:start=\"file://%p#302\">$7.95</price>
<price xmlns:tmplink=\"potemkin\"
       tmplink:start=\"file://%p#507\">$8.95</price>
<price xmlns:tmplink=\"potemkin\"
       tmplink:start=\"file://%p#715\">$4.50</price>
<price xmlns:tmplink=\"potemkin\"
       tmplink:start=\"file://%p#898\">$6.95</price>"
						 (format-spec-make ?p (buffer-file-name tmp))))
	    ("/" nil nil nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<breakfast_menu>\n	  <food>\n		    <name>Belgian Waffles</name>\n		    <price>$5.95</price>\n		    <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>\n		    <calories>650</calories>\n	  </food>\n	  <food>\n		    <name>Strawberry Belgian Waffles</name>\n		    <price>$7.95</price>\n		    <description>Light Belgian waffles covered with strawberries and whipped cream</description>\n		    <calories>900</calories>\n	  </food>\n	  <food>\n		    <name>Berry-Berry Belgian Waffles</name>\n		    <price>$8.95</price>\n		    <description>Light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>\n		    <calories>900</calories>\n	  </food>\n	  <food>\n		    <name>French Toast</name>\n		    <price>$4.50</price>\n		    <description>Thick slices made from our homemade sourdough bread</description>\n		    <calories>600</calories>\n	  </food>\n	  <food>\n		    <name>Homestyle Breakfast</name>\n		    <price>$6.95</price>\n		    <description>Two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>\n		    <calories>950</calories>\n	  </food>\n</breakfast_menu>"))))
    (dolist (case cases)
      (with-current-buffer tmp
	(erase-buffer)
	(insert-file-contents test-src)
	(save-buffer)
	(should
	 (equal
	  (progn
	    (with-current-buffer (apply 'xquery-tool-query (butlast case))
	      (buffer-substring-no-properties (point-min) (point-max))))
	  (car (last case))))))
    (delete-file (buffer-file-name tmp))
    (kill-buffer tmp)))

;; (ert "xquery-tool-test-query")

(ert-deftest xquery-tool-test-set-attribute ()
  (let ((cases
	 ;; char args result
	 '((1 "lang" "en" "xml" ((name ((xml:lang . "en")) "Belgian Waffles")))
	   (1 "lang" "en" nil ((name ((lang . "en")) "Belgian Waffles"))))))
    (dolist (case cases)
      (with-temp-buffer
	(insert "<name>Belgian Waffles</name>")
	(should
	 (equal
	  (save-excursion
	    (apply 'xquery-tool-set-attribute (butlast case))
	    (xml-parse-region (point-min) (point-max)))
	  (car (last case))))))))

(ert-deftest xquery-tool-test-get-attributes ()
  (let ((cases '(("c2-test.xml" 116 ((("xi" . "parse") . "text") (("xi" . "href") . "count.txt")))
		 ("c3-test.xml" 153 ((("xi" . "parse") . "text") (("xi" . "href") . "data.xml")))
		 ("simple.xml" 898))))
    (dolist (case cases)
      (with-temp-buffer
	(insert-file-contents
	 (file-truename
	  (expand-file-name (car case) (file-name-directory (symbol-file 'xquery-tool-test-query)))))
	(goto-char (elt case 1))
	(xmltok-forward)
	(should (equal (xquery-tool-get-attributes) (elt case 2)))))))

(ert-deftest xquery-tool-test-get-attribute ()
  (let ((cases '(
		 ;; default namespace (of element)
		 ("c2-test.xml" 116 ("href") [128 nil 132 134 143 t nil])
		 ;; namespace of element
		 ("c3-test.xml" 153 ("parse" "xi") [181 nil 186 188 192 t nil])
		 ("c3-test.xml" 153 ("parse") [181 nil 186 188 192 t nil])
		 ;; nil with namespace
		 ("c3-test.xml" 153 ("parse" "xml"))
		 ("simple.xml" 898 ("a")))))
    (dolist (case cases)
      (with-temp-buffer
	(insert-file-contents
	 (file-truename
	  (expand-file-name (car case) (file-name-directory (symbol-file 'xquery-tool-test-query)))))
	(goto-char (elt case 1))
	(xmltok-forward)
	(should (equal (apply 'xquery-tool-get-attribute (elt case 2)) (elt case 3)))))))


(ert-deftest xquery-tool-test-xinclude-general ()
  "Test general functionality of xinclude stuff.

TODO: fix paths so that test passes on different machines."
  (xquery-tool-wipe-temp-files (directory-files temporary-file-directory 'full "^xquery-tool-") 'force)
  (let* ((xquery-tool-result-root-element-name "xq-tool-results")
	 (xquery-tool-omit-xml-declaration nil)
	 (xquery-tool-resolve-xincludes t)
	 (test-dir (xquery-tool-get-test-dir))
	 (cases ;; filename args-for-query result
	  `(("xi-base.xml" ("/") ((document
				   ((xmlns:xi . "http://www.w3.org/2001/XInclude"))
				   "\n  "
				   (p nil "120 Mz is adequate for an average home user.")
				   "\n  "
				   (disclaimer
				    ((xml:base . ,(format "file://%sdisclaimer.xml" test-dir)))
				    "\n      "
				    (p nil "The opinions represented herein represent those of the individual\n  and should not be interpreted as official policy endorsed by this\n  organization.")
				    "\n   ")
				   "\n  "
				   (p nil "Just checking!")
				   "\n")))
	    ("xi-base.xml" ("//p" 'wrap) ((xq-tool-results nil "\n"
							       (p
								((xmlns:xi . "http://www.w3.org/2001/XInclude"))
								"120 Mz is adequate for an average home user.")
							       "\n"
							       (p
								((xmlns:xi . "http://www.w3.org/2001/XInclude"))
								"The opinions represented herein represent those of the individual\n  and should not be interpreted as official policy endorsed by this\n  organization.")
							       "\n"
							       (p
								((xmlns:xi . "http://www.w3.org/2001/XInclude"))
								"Just checking!")
							       "\n"))))))
    (dolist (case cases)
      (with-current-buffer (apply
			    'xquery-tool-query
			    ;; xml doc must be second arg
			    (car (elt case 1))
			    (find-file-noselect
			     (expand-file-name (car case)
					       test-dir))
			    (cdr (elt case 1)))
	;; (pp (xml-parse-region))
	;; (pp (last case))
	(xml-parse-region))
      (should
       (equal
	(with-current-buffer (apply
			      'xquery-tool-query
			      ;; xml doc must be second arg
			      (car (elt case 1))
			      (find-file-noselect
			       (expand-file-name (car case)
						 test-dir))
			      (cdr (elt case 1)))
	  ;; (pp (xml-parse-region))
	  ;; (pp (last case))
	  (xml-parse-region))
	(car (last case)))))))

;; (ert "xquery-tool-test-xinclude-general")

(ert-deftest xquery-tool-test-positions ()
  "Test whether the links back to the orginal buffer are correct."
  (let* ((xquery-tool-resolve-xincludes nil)
	 (test-dir (xquery-tool-get-test-dir))
	(cases `(("simple.xml"
		  ((breakfast_menu
		    ((tmplink:start . ,(format "file://%ssimple.xml#40" test-dir))
		     (xmlns:tmplink . "potemkin"))
		    "\n	"
		    (food
		     ((tmplink:start . ,(format "file://%ssimple.xml#58" test-dir)))
		     "\n		"
		     (name
		      ((tmplink:start . ,(format "file://%ssimple.xml#67" test-dir)))
		      "Belgian Waffles")
		     "\n		"
		     (price
		      ((tmplink:start . ,(format "file://%ssimple.xml#98" test-dir)))
		      "$5.95")
		     "\n		"
		     (description
		      ((tmplink:start . ,(format "file://%ssimple.xml#121" test-dir)))
		      "Two of our famous Belgian Waffles with plenty of real maple syrup")
		     "\n		"
		     (calories
		      ((tmplink:start . ,(format "file://%ssimple.xml#216" test-dir)))
		      "650")
		     "\n	")
		    "\n	"
		    (food
		     ((tmplink:start . ,(format "file://%ssimple.xml#251" test-dir)))
		     "\n		"
		     (name
		      ((tmplink:start . ,(format "file://%ssimple.xml#260" test-dir)))
		      "Strawberry Belgian Waffles")
		     "\n		"
		     (price
		      ((tmplink:start . ,(format "file://%ssimple.xml#302" test-dir)))
		      "$7.95")
		     "\n		"
		     (description
		      ((tmplink:start . ,(format "file://%ssimple.xml#325" test-dir)))
		      "Light Belgian waffles covered with strawberries and whipped cream")
		     "\n		"
		     (calories
		      ((tmplink:start . ,(format "file://%ssimple.xml#420" test-dir)))
		      "900")
		     "\n	")
		    "\n	"
		    (food
		     ((tmplink:start . ,(format "file://%ssimple.xml#455" test-dir)))
		     "\n		"
		     (name
		      ((tmplink:start . ,(format "file://%ssimple.xml#464" test-dir)))
		      "Berry-Berry Belgian Waffles")
		     "\n		"
		     (price
		      ((tmplink:start . ,(format "file://%ssimple.xml#507" test-dir)))
		      "$8.95")
		     "\n		"
		     (description
		      ((tmplink:start . ,(format "file://%ssimple.xml#530" test-dir)))
		      "Light Belgian waffles covered with an assortment of fresh berries and whipped cream")
		     "\n		"
		     (calories
		      ((tmplink:start . ,(format "file://%ssimple.xml#643" test-dir)))
		      "900")
		     "\n	")
		    "\n	"
		    (food
		     ((tmplink:start . ,(format "file://%ssimple.xml#678" test-dir)))
		     "\n		"
		     (name
		      ((tmplink:start . ,(format "file://%ssimple.xml#687" test-dir)))
		      "French Toast")
		     "\n		"
		     (price
		      ((tmplink:start . ,(format "file://%ssimple.xml#715" test-dir)))
		      "$4.50")
		     "\n		"
		     (description
		      ((tmplink:start . ,(format "file://%ssimple.xml#738" test-dir)))
		      "Thick slices made from our homemade sourdough bread")
		     "\n		"
		     (calories
		      ((tmplink:start . ,(format "file://%ssimple.xml#819" test-dir)))
		      "600")
		     "\n	")
		    "\n	"
		    (food
		     ((tmplink:start . ,(format "file://%ssimple.xml#854" test-dir)))
		     "\n		"
		     (name
		      ((tmplink:start . ,(format "file://%ssimple.xml#863" test-dir)))
		      "Homestyle Breakfast")
		     "\n		"
		     (price
		      ((tmplink:start . ,(format "file://%ssimple.xml#898" test-dir)))
		      "$6.95")
		     "\n		"
		     (description
		      ((tmplink:start . ,(format "file://%ssimple.xml#921" test-dir)))
		      "Two eggs, bacon or sausage, toast, and our ever-popular hash browns")
		     "\n		"
		     (calories
		      ((tmplink:start . ,(format "file://%ssimple.xml#1018" test-dir)))
		      "950")
		     "\n	")
		    "\n")))
		 ("xi-base.xml"
		  ((document
		    ((tmplink:start . ,(format "file://%sxi-base.xml#23" test-dir))
		     (xmlns:tmplink . "potemkin")
		     (xmlns:xi . "http://www.w3.org/2001/XInclude"))
		    "\n  "
		    (p
		     ((tmplink:start . ,(format "file://%sxi-base.xml#79" test-dir)))
		     "120 Mz is adequate for an average home user.")
		    "\n  "
		    (xi:include
		     ((tmplink:start . ,(format "file://%sxi-base.xml#133" test-dir))
		      ;; this is before we fix up links!
		      (href . "disclaimer.xml")))
		    "\n  "
		    (p
		     ((tmplink:start . ,(format "file://%sxi-base.xml#171" test-dir)))
		     "Just checking!")
		    "\n"))))))
    (dolist (case cases)
      ;; (pp (cons "expected" (car (last case))))
      (xquery-tool-wipe-temp-files nil 'force)
      (with-current-buffer (find-file-noselect (expand-file-name (car case) test-dir))
	(should
	 (equal
	  (with-current-buffer (find-file-noselect (xquery-tool-parse-to-shadow (current-buffer)))
	    ;; (pp (cons "result" (xml-parse-region (point-min) (point-max))))
	    (xml-parse-region (point-min) (point-max)))
	  (car (last case))))	
	(kill-buffer (current-buffer))))))

(ert-deftest xquery-tool-test-positions-narrowed ()
  "Test whether the links back to the orginal buffer are correct
when the buffer is narrowed."n
  (let* ((test-dir (xquery-tool-get-test-dir))
	(xquery-tool-omit-xml-declaration 'yes)
	(cases `(
		 ("simple.xml"
		  (687 . 712)
		  ((name
		    ((tmplink:start . ,(format "file://%ssimple.xml#687" test-dir))
		     (xmlns:tmplink . "potemkin"))
		    "French Toast")))
		 ("simple.xml"
		  (251 . 453)
		  ((food
		    ((tmplink:start . ,(format "file://%ssimple.xml#251" test-dir))
		     (xmlns:tmplink . "potemkin"))
		    "\n		"
		    (name
		     ((tmplink:start . ,(format "file://%ssimple.xml#260" test-dir)))
		     "Strawberry Belgian Waffles")
		    "\n		"
		    (price
		     ((tmplink:start . ,(format "file://%ssimple.xml#302" test-dir)))
		     "$7.95")
		    "\n		"
		    (description
		     ((tmplink:start . ,(format "file://%ssimple.xml#325" test-dir)))
		     "Light Belgian waffles covered with strawberries and whipped cream")
		    "\n		"
		    (calories
		     ((tmplink:start . ,(format "file://%ssimple.xml#420" test-dir)))
		     "900")
		    "\n	"))))))
    (dolist (case cases)
      (xquery-tool-wipe-temp-files nil 'force)
      (with-current-buffer (find-file-noselect (expand-file-name (car case) test-dir))
	(save-excursion
	  (save-restriction
	    (widen)
	    (narrow-to-region (car (elt case 1)) (cdr (elt case 1)))
	    (should
	     (equal
	      (with-current-buffer (find-file-noselect (xquery-tool-parse-to-shadow (current-buffer)))
		;; (pp (xml-parse-region (point-min) (point-max)))
		(xml-parse-region (point-min) (point-max)))
	      (car (last case))))))))))

(ert-deftest xquery-tool-test-positions-xinclude ()
  "Test back links for xinclude files."
  (let* ((test-dir (xquery-tool-get-test-dir))
	(xquery-tool-omit-xml-declaration 'yes)
	(xquery-tool-resolve-xincludes 'yes)
	(cases `(("xi-base.xml"
		  ((document
		    ((xmlns:tmplink . "potemkin")
		     (xmlns:xi . "http://www.w3.org/2001/XInclude")
		     (tmplink:start . ,(format "file://%sxi-base.xml#23" test-dir)))
		    "\n  "
		    (p
		     ((tmplink:start . ,(format "file://%sxi-base.xml#79" test-dir)))
		     "120 Mz is adequate for an average home user.")
		    "\n  "
		    (disclaimer
		     ((tmplink:start . ,(format "file://%sdisclaimer.xml#23" test-dir))
		      (xml:base . ,(xquery-tool-indexed-xml-file-name "3abf43ca6771e650af2e5c455db7a14c478dd23a")))
		     "\n      "
		     (p
		      ((tmplink:start . ,(format "file://%sdisclaimer.xml#38" test-dir)))
		      "The opinions represented herein represent those of the individual\n  and should not be interpreted as official policy endorsed by this\n  organization.")
		     "\n   ")
		    "\n  "
		    (p
		     ((tmplink:start . ,(format "file://%sxi-base.xml#171" test-dir)))
		     "Just checking!")
		    "\n"))))))
    (dolist (case cases)
      ;; (pp (cons "expected" (car (last case))))
      (xquery-tool-wipe-temp-files nil 'force)
      (with-current-buffer (find-file-noselect (expand-file-name (car case) test-dir))
	(should
	 (equal
	  (with-current-buffer (xquery-tool-query "/" (current-buffer) nil 'save nil)
	    ;;(pp (cons "result" (xml-parse-region (point-min) (point-max))))
	    (xml-parse-region (point-min) (point-max)))
	  (car (last case))))))))

;; (ert "xquery-tool-test-positions-xinclude")

(ert-deftest xquery-tool-test-namespace-fun ()
  "Test things on a seriously namespaced document."
  (let* ((test-dir (xquery-tool-get-test-dir))
	(xquery-tool-result-root-element-name 'beep)
	(xquery-tool-omit-xml-declaration t)
	(cases `(("namespace-test.xml";; actually a libreoffice document
		  (2209 . 2858);; range to work on
		  "//text()";; query
		  ;; result
		 ((,xquery-tool-result-root-element-name nil "
sam soup2016-02-09T10:46:26.7281246822016-02-09T10:47:43.326659469sam soupPT1M16S1LibreOffice/5.0.4.2$Linux_X86_64 LibreOffice_project/00m0$Build-2
")))
		 ("namespace-test.xml";; actually a libreoffice document
		 nil;; range to work on
		 "//office:meta//text()";; query
		 ;; result; same as for narrowed above
		 ((,xquery-tool-result-root-element-name nil "
sam soup2016-02-09T10:46:26.7281246822016-02-09T10:47:43.326659469sam soupPT1M16S1LibreOffice/5.0.4.2$Linux_X86_64 LibreOffice_project/00m0$Build-2
"))))))
    (dolist (case cases)
      (xquery-tool-wipe-temp-files nil 'force)
      (with-current-buffer (find-file-noselect (expand-file-name (car case) test-dir))
	(when (consp (elt case 1))
	  (widen)
	  (narrow-to-region (car (elt case 1)) (cdr (elt case 1))))
	(should
	 (equal
	  (with-current-buffer (xquery-tool-query (elt case 2) (current-buffer) 'wrap nil nil)
	    ;; (pp (cons "result" (xml-parse-region (point-min) (point-max))))
	    (xml-parse-region (point-min) (point-max)))
	  (elt case 3)))))))

;; (ert "xquery-tool-test-namespace-fun")

(ert-deftest xquery-tool-namespace-test-2 ()
  "Try not to croak when saxon namespace is already there."  
  (let* ((test-dir (xquery-tool-get-test-dir))
	(xquery-tool-result-root-element-name 'beep)
	(xquery-tool-omit-xml-declaration t)
	(cases `(("namespace-test2.xml";; actually a libreoffice document
		  nil ;; range to work on
		  "//body/p/text()";; query
		  ;; result
		  ((,xquery-tool-result-root-element-name nil "\nempty\n")))
		 ("namespace-test-narrow.xml"
		  nil ;; range to work on
		  "count(//l)";; query
		  ;; result
		  ((,xquery-tool-result-root-element-name nil "\n2\n")))
		 ("namespace-test-narrow.xml"
		  (35 . 143)	       ;; range to work on
		  "count(//l)";; query
		  ;; result
		  ((,xquery-tool-result-root-element-name nil "\n2\n"))))))
    (dolist (case cases)
      (xquery-tool-wipe-temp-files nil 'force)
      (save-restriction
	(with-current-buffer (find-file-noselect (expand-file-name (car case) test-dir))
	  (widen)
	  (when (consp (elt case 1))
	    (narrow-to-region (car (elt case 1)) (cdr (elt case 1))))
	  (should
	   (equal
	    (with-current-buffer (xquery-tool-query (elt case 2) (current-buffer) 'wrap nil nil)
	      (message (pp (cons "result" (xml-parse-region (point-min) (point-max)))))
	      (xml-parse-region (point-min) (point-max)))
	    (elt case 3))))))))

;; (ert "xquery-tool-namespace-test-2")



(ert-deftest xquery-tool-test-unicode-things ()
  "Test if unicode things work (match on attribute, name of element, content)."
  (let ((xquery-tool-result-root-element-name 'beep)
	 (xquery-tool-omit-xml-declaration t)
	 (xml-doc "<dīv xml:id=\"āṣūḥṇṭḹ\">mḹāṣṭḥḹअत्र</dīv>")
	 (cases `(("count(//dīv)" . "1")
		  ("count(//*[@xml:id=\"āṣūḥṇṭḹ\"])" . "1")
		  ("count(//*[matches(@xml:id, \"^āṣū\")])" . "1")
		  ("count(//*[matches(./text(), \"mḹāṣṭḥḹअ\")])" . "1"))))
    (dolist (case cases)
      (xquery-tool-wipe-temp-files nil 'force)
      (with-temp-buffer
	(insert xml-doc)
	(should
	 (equal
	  (with-current-buffer (xquery-tool-query (car case) (current-buffer) nil nil nil)
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (cdr case)))))))

;; (ert 'xquery-tool-test-unicode-things)


(ert-deftest xquery-tool-test-setup-xquery-file ()
  ;; with a temp buffer
  (should
   (equal
    (file-exists-p
     (with-temp-buffer
       (xquery-tool-setup-xquery-file "/" (current-buffer))))
    t))
  ;; with a named buffer
  (should
   (equal
    (file-exists-p
     (let* ((buff  (get-buffer-create "xquery tool test buffer"))
	   (qfile (xquery-tool-setup-xquery-file "/" buff)))
       (kill-buffer buff)
       qfile))
    t))
  ;; with an existing file
  (should
   (equal
    (file-exists-p
     (let* ((fname  (expand-file-name (make-temp-name "xquery-tool") temporary-file-directory))
	    (f  (with-temp-file fname
		  (insert "<not really xml> </ XYZ")))
	   (qfile (xquery-tool-setup-xquery-file "/" fname)))
       (delete-file fname)
       qfile))
    t))
  ;; with a non-existing file
  (should-error
   (xquery-tool-setup-xquery-file
    "/"
    (expand-file-name (make-temp-name "xquery-tool") temporary-file-directory))))

;; (ert "xquery-tool-test-setup-xquery-file")


