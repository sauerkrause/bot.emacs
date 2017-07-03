;; -*- lexical-binding: t -*-

(defun ping-command (text process sender response target)
  "pongs back"
  (concat "pong " text))
(puthash "ping" 'ping-command command-table)

(defun say-command (text process sender response target)
  "says the arguments passed"
  text)
(puthash "say" 'say-command command-table)

(defun async-say-command (fn text process sender response target)
  "Asynchronously says the arguments passed"
  (funcall fn text))
(puthash "async-say" 'async-say-command async-command-table)

(defun get-xml-tags (xml symbol)
  (xml-get-children (car xml) symbol))

(defun handle-random-quote (fn xml)
  (let* ((guids (mapcar (lambda (i)
			  (car (cddr (car (xml-get-children i 'guid)))))
			(get-xml-tags xml 'item)))
	 (quote-ids (mapcar (lambda (i)
			      (replace-regexp-in-string "[./:a-z]" "" i))
			    guids)))
    (qdb-quote fn (random-choice quote-ids))))

(defun handle-quote (fn xml id)
  (let* ((description (car (mapcar (lambda (i)
				     (car (cddr (car (xml-get-children i 'description)))))
				   (get-xml-tags xml 'item)))))
    
    (labels ((substitute-str (string)
	       (with-temp-buffer
		 (insert string)
		 (dolist (substitution '(("&amp;" . "&")
					 ("&lt;" . "<")
					 ("&gt;" . ">")
					 ("&nbsp;" . " ")
					 ("&apos;" . "'")
					 ("&quot;" . "\"")))
		   (goto-char (point-min))
		   (while (search-forward (car substitution) nil t)
		     (replace-match (cdr substitution) t t nil)))
		 (buffer-string)))
	     (dehtml (string)
	       (with-temp-buffer
		 (insert string)
		 (dolist (substitution '(("<br>" . "\n")
					 ("<i>" . "")
					 ("</i>" . "")))
		   (goto-char (point-min))
		   (while (search-forward (car substitution) nil t)
		     (replace-match (cdr substitution) t t nil)))
		 (buffer-string)))
	     (strip-br (string)
		    (replace-regexp-in-string "\<br \/\>" "" string))
	     (strip-tt (string)
		       (replace-regexp-in-string "^\<tt\>" "" string))
	     (strip-end-tt (string)
			   (replace-regexp-in-string "\<\/tt\>$" "" string))
	     (strip (string)
		    (strip-br (strip-tt (strip-end-tt string))))
	     (fix-tags (string)
		       (replace-regexp-in-string 
			"\&gt\;" ">"
			(replace-regexp-in-string "\&lt\;" "<" string))))
      (funcall fn (cons (format "QDB Quote: #%s" id) (split-string (dehtml
				 (substitute-str (strip description))) "\n"))))))
(defun qdb-quote (fn id)
  (let ((url (format "http://qdb.us/qdb.xml?action=quote&quote=%s" id)))
    (web-http-get (lambda (httpc headers body)
		    (handle-quote fn (xml-from-body body) id))
		  :url url)))
(defun qdb-fn (fn name)
  (let ((url (format "http://qdb.us/qdb.xml?action=%s" name)))
    (web-http-get (lambda (httpc headers body)
		    (handle-random-quote fn (xml-from-body body)))
		  :url url)))
(defun qdb-command (fn text process sender response target)
  "Gives back a qdb quote. argument may be a quote id or 'random'"
  (let ((arg (car (split-string text))))
    (let ((api-fn (member (downcase arg) '("random" "top" "best" "latest" "bottom" "leetness" "picks"))))
      (if api-fn
	  (qdb-fn fn (car api-fn))
	(qdb-quote fn arg)))))

(puthash "qdb" 'qdb-command async-command-table)

(defun random-command (text process sender response target)
  "Picks a random value from a comma separated list or space separated if no commas"
  (let* ((words (mapcar (apply-partially 'replace-regexp-in-string "^ *" "")
			(split-string text (if (find ?, text) "," " ")))))
    (random-choice words)))
(puthash "random" 'random-command command-table)

(defun get-string-from-file (filepath)
  "Return content of file"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun uptime-command (text process sender response target)
  "Returns uptime info from machine bot is using"
  (format-seconds
   "%Y %D %h:%m:%s"
   (string-to-number
    (car 
     (split-string 
      (get-string-from-file "/proc/uptime"))))))
(puthash "uptime" 'uptime-command command-table)

(defun cpu-load-command (text process sender response target)
  "Returns load average from machine bot is using"
  (let ((string (split-string (get-string-from-file "/proc/loadavg"))))
    (format "Load avg: %s" (join-strings (butlast string 2)))))
(puthash "load" 'cpu-load-command command-table)

(defun uname-command (text process sender response target)
  "Returns uname info from machine bot is using"
  (get-string-from-file "/proc/version"))
(puthash "uname" 'uname-command command-table)

(define-reply 8ball '("It is certain."
    "It is decidedly so."
    "Without a doubt."
    "Yes, definitely."
    "You may rely on it."
    "As I see it, yes."
    "Most likely."
    "Outlook good."
    "Yes."
    "Signs point to yes."
    "Reply hazy try again."
    "Ask again later."
    "Cannot predict now."
    "Better not tell you now."
    "Concentrate and ask again."
    "Don't count on it."
    "My reply is no."
    "My sources say no."
    "Outlook not so good."
    "Very doubtful.")
"Emulates an 8-ball's responses")

(define-reply scale-1->10 (mapcar 'number-to-string
				  (number-sequence 1 10))
  "Replies back with a number between 1 and 10 (inclusive)")

(defun post-jellybeans (name number)
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash "number" (number-to-string number) query-data)
    (web-http-post
     (lambda (con header data)
       nil)
     :url (format "http://192.168.1.17:8000/%s/jellybeans" name)
     :data query-data)))

(define-reply botsnack '(":3") "Use to reward good bot behavior")
(define-reply botsmack '("3:") "Use to punish bad bot behavior")

(defun fortune-command (text process sender response target)
  "Returns a fortune"
  (let ((fortune (shell-command-to-string "fortune -s")))
    (split-string (replace-regexp-in-string "\t"
					    "        "
					    fortune) "\n")))
(puthash "fortune" 'fortune-command command-table)

(defun fortran-command (text process sender response target)
  "Returns a fortune with FORTRAN77 syntax"
  (cons "PROGRAM FORTUNE"
	(append (mapcar 'upcase (fortune-command text process sender response target)) (list "END PROGRAM"))))
(puthash "fortran" 'fortran-command command-table)

(define-reply yes-no '("yes" "no") "Returns yes or no")
(define-reply poop '("💩") "Displays a unicode pile of poo glyph")
(define-reply look-of-disapproval '("ಠ_ಠ") "Returns the look of disapproval")

(let ((flipped nil))
  (define-reply table-flip (list (let ((result (if flipped
						   "(╯^_^）╯︵ ┬─┬"
						 "(╯°□°）╯︵ ┻━┻")))
				   (setq flipped (not flipped))
				   result))
    "Flips a stateful table."))

(let ((nato-table (make-hash-table :test 'equal)))
  (labels ((as-in (alos)
		  (mapcar (apply-partially 'format "as in %s") alos))
	   (letter-as-in (letter list)
			 (cons letter (as-in list))))
  (mapcar (lambda (cons)
	    (puthash (car cons)
		     (cdr cons)
		     nato-table))
	    (list
	     (letter-as-in "a" '("aether" "aegis" "aeon" "aisle"))
	     (letter-as-in "c" '("czar" "cent" "hundred" "cnidaria"))
	     (letter-as-in "d" '("django"))
	     (letter-as-in "e" '("ewe"))
	     (letter-as-in "g" '("ghoti" "gnat" "gnome" "GNU/Linux, or as I've recently taken to calling it, GNU+Linux"))
	     (letter-as-in "h" '("hour" "honor" "herb"))
	     (letter-as-in "j" '("jalapeño" "javelina"))
	     (letter-as-in "k" '("knight" "knot"))
	     (letter-as-in "l" '("fifty" "Llewellyn"))
	     (letter-as-in "m" '("mnemonic" "thousand" "Mancy"))
	     (letter-as-in "o" '("opossum"))
	     (letter-as-in "p" '("pneumonia" "pneumatic" "pharmacy" "psalm" "pterodactyl" "pseudoscience"))
	     (letter-as-in "q" '("qatar"))
	     (letter-as-in "t" '("tsar"))
	     (letter-as-in "v" '("five"))
	     (letter-as-in "w" '("write" "wraith" "write"))
	     (letter-as-in "x" '("xylophone" "Xilinx" "ten" "xenophobia"))
	     (letter-as-in "y" '("ypres")))))
  (defun nato-command (text process sender response target)
    "Returns a perversion of the nato alphabet for the letter given as an argument"
    (let ((arg (car (split-string text))))
      (when arg
	(random-choice (gethash (downcase arg) nato-table)))))
  (puthash "nato" 'nato-command command-table))

(define-reply where '("Up your butt and around the corner" "Under your couch cushions" "In your hand" "Somewhere safe where you won't forget where it is" "Your mother would know") "Tells you where you might find something")


(defun cloud-rcirc-print-hook (process sender response target text)
  (when (and (or (string-match
		  (regexp-quote "☁") text)
		 (string-match
		  (regexp-quote "cloud") (downcase text)))
	     (not (string-match "CLOUD$" text))
	     (not (string= (rcirc-server-name process) sender)))
    (let ((r (random 3)))
      (when (eql 0 r)
	  (rcirc-send-message process target "CLOUD")))))
(add-hook 'rcirc-print-functions 'cloud-rcirc-print-hook)

(define-reply
  shh
  '("http://tinyurl.com/qjsfxyp")
  "Shushes you")

(define-reply
  sting
  '("ba-dum *ching*")
  "Stings")


(define-reply face
  '("( ͡° ͜ʖ ͡°)"
    ">_<"
    "<(｀^´)>"
    "((d [-_-]b))"
    "(p_-)"
    "¯\_(ツ)_/¯"
    "（￣ー￣）"
    "ヽ（´ー｀）┌"
    "(｀-´)>"
    "┐('～`；)┌"
    "(╬ ಠ益ಠ)"
    "（‐＾▽＾‐）"
    "┌(；`～,)┐"
    "(≧ロ≦)"
    "8)"
    "(8")
  "Makes a face")

(defun handle-commit (fn)
  (web-http-get (lambda (httpc headers body)
		  (funcall fn body))
		:url "http://whatthecommit.com/index.txt"))

(defun commit-command (fn text process sender response target)
  "Gives back a commit"
  (handle-commit fn))
(puthash "commit" 'commit-command async-command-table)

(defun rcirc-handler-ctcp-BOTINFO (process target sender args)
  (rcirc-send-string process
                     (concat "NOTICE " sender
                             " :\C-aBOTINFO "
			     "Come on, I'm using rcirc. How could I be a bot?"
			     "\C-a")))

(defun eightface-command (fn text process sender response target)
  "(8"
  (funcall fn "8)"))
(puthash "8" 'eightface-command async-command-table)

(define-reply fridayp (list (format "%s" (equal "Friday" (format-time-string "%A"))))
  "Predicate for current state of friday")
(define-reply should
  (list "If you have to ask, no.")
  "Tells you whether or not you should.")
