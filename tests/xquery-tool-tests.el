;;; some tests for xquery-tool

;;; run from command line as `emacs -batch -l ert -l xquery-tool.el -l tests/xquery-tool-tests.el -f ert-run-tests-batch-and-exit'

(ert-deftest xquery-tool-test-query ()
  "Check general functionality of `xquery-tool-query'.
Does not check the links, though."
  (let ((tmp (find-file-noselect (make-temp-file "xquery-test-src")))
	(test-src (file-truename (expand-file-name "simple.xml" (file-name-directory (symbol-file 'xquery-tool-test-query)))))
	(cases
	 '(("//price" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price>$5.95</price>\n<price>$7.95</price>\n<price>$8.95</price>\n<price>$4.50</price>\n<price>$6.95</price>")
	   ;;; won't pass ---> pid is wrong
	   ;; ("//price" nil 'save-namespaces "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#98\">$5.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#302\">$7.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#507\">$8.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#715\">$4.50</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#898\">$6.95</price>")
	   )))
    (dolist (case cases)
      (with-current-buffer tmp
	(erase-buffer)
	(insert-file-contents test-src)
	(save-buffer)
	(should
	 (equal
	  (progn
	    (apply 'xquery-tool-query (butlast case))
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (car (last case))))))
    (delete-file (buffer-file-name tmp))
    (kill-buffer tmp)))

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
      ((cases '(("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#98\">$5.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#302\">$7.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#507\">$8.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#715\">$4.50</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#898\">$6.95</price>" . "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price>$5.95</price>\n<price>$7.95</price>\n<price>$8.95</price>\n<price>$4.50</price>\n<price>$6.95</price>"))))
    (dolist (case cases)
      (with-temp-buffer
	(insert (car case))
	(should
	 (equal
	  (progn
	    (xquery-tool-setup-xquery-results)
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (cdr case)))))))


