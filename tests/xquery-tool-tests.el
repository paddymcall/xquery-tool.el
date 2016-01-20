;;; some tests for xquery-tool

;;; run from command line as `emacs --no-init-file --no-site-file -batch -l ert -l xquery-tool.el -l tests/xquery-tool-tests.el -f ert-run-tests-batch-and-exit'

(require 'format-spec)

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
      ((cases '(("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#98\">$5.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#302\">$7.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#507\">$8.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#715\">$4.50</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf:///%20*temp*-777077#898\">$6.95</price>" . "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price>$5.95</price>\n<price>$7.95</price>\n<price>$8.95</price>\n<price>$4.50</price>\n<price>$6.95</price>"))))
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
  "Test general functionality of xinclude stuff."
  (xquery-tool-wipe-temp-files (directory-files temporary-file-directory 'full "^xquery-tool-") 'force)
  (let ((xquery-tool-result-root-element-name "xq-tool-results")
	(xquery-tool-omit-xml-declaration nil)
	(xquery-tool-resolve-xincludes t)
	(cases ;; filename args-for-query result
	 '(("xi-base.xml" ("/") ((document
				      ((xmlns:xi . "http://www.w3.org/2001/XInclude"))
				      "\n  "
				      (p nil "120 Mz is adequate for an average home user.")
				      "\n  "
				      (disclaimer
				       ((xml:base . "file:///home/beta/webstuff/emacs-things/xquery-tool.el/tests/disclaimer.xml"))
				       "\n      "
				       (p nil "The opinions represented herein represent those of the individual\n  and should not be interpreted as official policy endorsed by this\n  organization.")
				       "\n   ")
				      "\n  "
				      (p nil "Just checking!")
				      "\n")))
	   ("xi-base.xml" ("//p" nil 'wrap) ((xq-tool-results nil "\n"
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
      (with-current-buffer (find-file-noselect
			    (expand-file-name (car case)
					      (file-name-directory (symbol-file 'xquery-tool-test-query))))
	(should
	 (equal
	  (progn (with-current-buffer (apply 'xquery-tool-query (elt case 1))
		   ;; (pp (xml-parse-region))
		   ;; (pp (last case))
		   (xml-parse-region)))
	  (car (last case))))))))

