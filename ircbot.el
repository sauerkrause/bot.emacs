;; -*- lexical-binding: t -*-

(require 'rcirc)
(require 'dom)
(require 'web)
(require 'cl)
(require 'list-utils)

;; lookup in command-table the function for a command if it exists, call it and pass it the rest of the args.
(setq command-table (make-hash-table :test 'equal))
(setq async-command-table (make-hash-table :test 'equal))
(setq command-needs-auth-table (make-hash-table :test 'equal))
(setq pending-funcalls (make-hash-table :test 'equal))

;; hook for rcirc print    
(defun my-rcirc-highlight-hook (process sender response target text)
  (when (and (string-match (concat "^" (regexp-quote (rcirc-nick process))) text)
	     (not (string= (rcirc-server-name process) sender)))
    ;; do your stuff
    (let ((text
	   (replace-regexp-in-string (format "^%s[^a-zA-Z0-9]*" (regexp-quote (rcirc-nick process))) "" text)))
      (handle-authable-command
       (gethash (car (split-string text)) command-needs-auth-table)
       process sender target text))))

(defun handle-authable-command (auth process sender target text)
  (flet ((handle-line (line)
		      (rcirc-send-message process target reply)))
	(if auth
	    nil
	  (if (async-command-p text)
	      (handle-async-command process sender nil target text)
	    (let* ((reply (handle-command text process sender nil target))
		   (reply-lines (if (listp reply)
				    reply
				  (cons (if (stringp reply)
					    reply
					  reply) nil))))
	      (mapcar (apply-partially 'rcirc-send-message process target)
		      reply-lines))))))

;; add the hook
(defun my-rcirc-url-hook (process sender response target text)
  (when (not (string= (rcirc-server-name process) sender))
    (let ((urls (cl-remove-if (lambda (i)
				(let* ((url-struct (url-generic-parse-url i))
				       (type (url-type url-struct)))
				  (or (eq nil type)
				      (not (or (equal "http" type)
					       (equal "https" type)))
				      (string-match "tinyurl.com" i))))
			      (split-string text))))
      (if urls
	  (mapcar (lambda (url)
		    (funcall (apply-partially 'handle-async-command process sender nil target)
			     (format "tinyurl %s" url)))
		  urls)))))

(add-hook 'rcirc-print-functions 'my-rcirc-highlight-hook)
(add-hook 'rcirc-print-functions 'my-rcirc-url-hook)

(defun my-rcirc-authed-hook (process sender response target text)
  (when (string= sender "NickServ")
    (let* ((words (split-string text))
	   (nick (elt words 0))
	   (second (elt words 1))
	   (auth-status (when (string= second "ACC")
			  (string= (elt words 2) "3"))))
      (when auth-status
	  (mapcar 'funcall
		  (gethash nick pending-funcalls)))
      (remhash nick pending-funcalls))))
(add-hook 'rcirc-print-functions 'my-rcirc-authed-hook)

(defun append-hash (key value hash)
  (let ((pending-calls (gethash key hash)))
    (puthash key (append pending-calls (list value)) hash)))

(defun handle-command (text process sender response target)
  (let ((words (split-string text)))
    (let ((command (gethash (downcase (car words)) command-table)))
      (when command
	(apply command (list (mapconcat 'identity (cdr words) " ") 
			       process 
			       sender 
			       response 
			       target))))))

(defun async-reply (process target reply)
  (let ((reply-lines (if (listp reply)
				    reply
				  (cons (if (stringp reply)
					    reply
					  reply) nil))))
    (mapcar (apply-partially 'rcirc-send-message process target)
	    reply-lines)))

(defun join-strings (words)
  (mapconcat 'identity words " "))

(defun handle-async-command (process sender response target text)
  (let ((words (split-string text)))
    (let ((command (gethash (downcase (car words)) async-command-table)))
      (when command
	(funcall command (apply-partially
			  'async-reply process target)
		 (mapconcat 'identity (cdr words) " ")
		 process sender response target)))))

(defun authed-command-p (text)
  (let ((words (split-string text)))
    (gethash (downcase (car words)) command-needs-auth-table)))

(defun async-command-p (text)
  (let ((words (split-string text)))
    (gethash (downcase (car words)) async-command-table)))

;; Let's write some commands.
(defun ping-command (text process sender response target)
  (concat "pong " text))
(puthash "ping" 'ping-command command-table)

(defun commands-command (text process sender response target)
  (let (commands)
    (let ((fn (lambda (key value)
		(setq commands (cons key commands)))))
    (maphash fn
	     command-table)
    (maphash fn async-command-table)
    (mapconcat 'identity (sort commands 'string<) " "))))
(puthash "commands" 'commands-command command-table)

(defun say-command (text process sender response target)
  text)
(puthash "say" 'say-command command-table)

(defun async-say-command (fn text process sender response target)
  (funcall fn text))
(puthash "async-say" 'async-say-command async-command-table)

(defun source-command (text process sender response target)
  "https://github.com/sauerkrause/bot.emacs")
(puthash "source" 'source-command command-table)

(require 'web)
(defun spit-page (fn url)
  (web-http-get (lambda (httpc header page-data)
		  (funcall fn page-data))
		:url url))

;; (defun points-command (fn text process sender response target)
;;   (let* ((victim (if (car (split-string text))
;; 		     (car (split-string text))
;; 		   sender))
;; 	 (url (format "%s/%s/points" jellybeanjs-host victim)))
;;     (spit-page fn url)))
;; (puthash "points" 'points-command async-command-table)

;; (defun jellybeans-command (fn text process sender response target)
;;   (let* ((victim (if (car (split-string text))
;; 		     (car (split-string text))
;; 		   sender))
;; 	 (url (format "%s/%s/jellybeans" jellybeanjs-host victim)))
;;     (spit-page fn url)))
;; (puthash "jellybeans" 'jellybeans-command async-command-table)
(defun html-from-body (body)
  (with-temp-buffer
    (insert body)
    (libxml-parse-html-region (point-min)
			      (point-max))))
(defun get-elements-by-tag (dom tag)
  (let ((parsed dom))
    (let ((cur parsed)
	  (out nil))
      (while cur
	(if (eq (car cur) tag)
	      (setq out (cons tag (cdr cur))))
	(if (consp (car cur))
	    (let ((subout (get-elements-by-tag (car cur) tag)))
	      (if subout
		  (setq out (cons subout out)))))
	(setq cur (cdr cur)))
      (list-utils-flatten out))))
(defun parse-dom (dom)
  (if (stringp dom)
      dom
    (let ((tag (car dom))
	  (subtags (cddr dom)))
      (cons tag (mapcar 'parse-dom subtags)))))
(defun find-title (body)
  (let ((titles
	 (get-elements-by-tag
	  (parse-dom (html-from-body body)) 'title)))
    (when titles
      (elt titles 1))))

(defun put-title-tinyurl (fn url page-data)
  (web-http-get
   (lambda (httpc header body)
     (let ((content-type (gethash 'content-type header)))
       (if (equal "text/html" (car (split-string content-type "; ")))
	(funcall fn
		 (format "%s%s" page-data 
			 (let ((title (find-title body)))
			   (if title
			       (format " : (%s)" title)
			     ""))))
	(funcall fn page-data))))
   :url url))

(defun tinyurl (fn url)
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'url url query-data)
    (web-http-post
     (lambda (httpc header page-data)
       (message "%s" page-data)
       (put-title-tinyurl fn url page-data))
     :url "http://tinyurl.com/api-create.php"
     :data query-data)))

(defun tinyurl-command (fn text process sender response target)
  (let* ((url (car (split-string text))))
    (tinyurl fn url)))
(puthash "tinyurl" 'tinyurl-command async-command-table)

(defun xml-from-body (body)
  (with-temp-buffer
    (insert body)
    (xml-parse-region (point-min) (point-max))))

(defun get-weather-description (xml)
  (format "%s and %s at %s"
	  (get-weather-attribute 'weather xml)
	  (get-weather-attribute 'temperature_string xml)
	  (get-weather-attribute 'location xml)))

(defun get-weather-attribute (sym xml)
  (let ((tmp-node
          (car (xml-get-children (car xml)
                                 sym))))
    (car (cddr tmp-node))))
(defun weather-command (fn text process sender response target)
  (let* ((station (car (split-string text)))
	 (url (format "http://w1.weather.gov/xml/current_obs/%s.xml" station)))
    (web-http-get (lambda (httpc headers body)
		    (funcall fn (get-weather-description (xml-from-body body))))
		  :url url)))
(puthash "weather" 'weather-command async-command-table)

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
(defun handle-quote (fn xml)
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
      (funcall fn (split-string (dehtml
				 (substitute-str (strip description))) "\n")))))
(defun qdb-quote (fn id)
  (let ((url (format "http://qdb.us/qdb.xml?action=quote&quote=%s" id)))
    (web-http-get (lambda (httpc headers body)
		    (handle-quote fn (xml-from-body body)))
		  :url url)))
(defun qdb-fn (fn name)
  (let ((url (format "http://qdb.us/qdb.xml?action=%s" name)))
    (web-http-get (lambda (httpc headers body)
		    (handle-random-quote fn (xml-from-body body)))
		  :url url)))
(defun qdb-command (fn text process sender response target)
  (let ((arg (car (split-string text))))
    (let ((api-fn (member (downcase arg) '("random" "top" "best" "latest" "bottom" "leetness" "picks"))))
      (if api-fn
	  (qdb-fn fn (car api-fn))
	(qdb-quote fn arg)))))

(puthash "qdb" 'qdb-command async-command-table)

(qdb-command nil "random" nil nil nil nil)
(qdb-command nil "37167" nil nil nil nil)

(defun random-choice (choices)
  (if (and choices (listp choices))
	(let ((element 
	       (elt choices (random (length choices)))))
	  element)))

(defun random-command (text process sender response target)
  (let* ((words (mapcar (apply-partially 'replace-regexp-in-string "^ *" "")
			(split-string text (if (find ?, text) "," " ")))))
    (random-choice words)))
(puthash "random" 'random-command command-table)

(defmacro define-reply (cmd choices)
  `(progn
     (defun ,cmd (text process sender response target)
       (let ((choices ,choices))
	 (random-choice choices)))
     (puthash (symbol-name ',cmd) ',cmd command-table)))

(defun uptime-command (text process sender response target)
  (shell-command-to-string "uptime"))
(puthash "uptime" 'uptime-command command-table)

(defun uname-command (text process sender response target)
  (shell-command-to-string "uname -a"))
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
    "Very doubtful."))

(define-reply scale-1->10 (mapcar 'number-to-string
				  (number-sequence 1 10)))

(defun post-jellybeans (name number)
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash "number" (number-to-string number) query-data)
    (web-http-post
     (lambda (con header data)
       nil)
     :url (format "http://192.168.1.17:8000/%s/jellybeans" name)
     :data query-data)))

(define-reply botsnack '(":3"))
(define-reply botsmack '("3:"))

(defun fortune-command (text process sender response target)
  (let ((fortune (shell-command-to-string "fortune -s")))
    (split-string (replace-regexp-in-string "\t"
					    "        "
					    fortune) "\n")))
(puthash "fortune" 'fortune-command command-table)

(defun fortran-command (text process sender response target)
  (let* ((fortune (shell-command-to-string "fortune -s"))
	 (fortran (format "PROGRAM FORTUNE\n%s\nEND PROGRAM" (upcase fortune))))
    (split-string (replace-regexp-in-string "\t"
					    "        "
					    fortran) "\n")))
(puthash "fortran" 'fortran-command command-table)

(define-reply yes-no '("yes" "no"))
(define-reply poop '("💩"))
(define-reply look-of-disapproval '("ಠ_ಠ"))

(define-reply latvia '("Finally has internet. Kan download potato. Family is no longer cry. But no warm. Kan download blanket?"
		       "I make field. In field, many potato. Frost come and take potato. Now I have not potato"
		       "I have not potato. Am sad. Also cold."
		       "Politburo came took wife children and house. No potato. Yet alive. Death not come. Very cold."
		       "Neighbor tell politburo about axe. They are take, now not make wood fire. So cold."
		       "\"Is so cold.\" \"How cold is?\" \"Very. Also dark\""
		       "\"I hope son no die during night\" \"What 'hope'?\" \"I know what you say\" \"No am serious. What is hope?\" \"I know not\""
		       "Latvian comedian say \"What deal with potato?\" Crowd not laugh. Comedian squint to see audience. There no crowd. All die from malnourish."
		       "Two Latvian look at clouds. One see potato. Other see impossible dream. Is same cloud"
		       "Latvian Nursery Rhyme: One potato, one potato, one potato, no more potato. Soldier eat potato and rape daughter."
		       "Man hungry. He steal bread for feed family. Get home, all family sent Siberia! “More bread for me,” man think. But bread have worm."
		       "What are one potato say other potato? Premise ridiculous. Who have two potato?"
		       "Man car break down near house of farmer. Take shelter in barn. Find farmer daughter in barn. Oh! Hot stuff! But TOO LATE! Is already rape by soldier."
		       "Latvian walk into bar with mule. Bartender say, “Why so long face?” Latvian say, “I was thinking of my daughter. She has been lie with soldier for potato feed baby. “"
		       "Three Latvian are brag about sons. “My son is soldier. He have rape as many women as want,” say first Latvian. “Zo?” second say, “My son is farmer. He have all potato he want!” Third Latvian wait long time, then say, “My son is die at birth. For him, struggle is over.” “Wow! You are win us,” say others. But all are feel sad."
		       "Q : What are one potato say other potato? A : Premise ridiculous. Who have two potato?"
		       "Q : How many Latvian is take screw in light bulb? A : 25. One screw in, 24 ride bicycle generator for 1-hour shift. But time probably better spend search food."
		       "Q: What is happening if you cross Latvian and potato? A: This is cruel joke. please, no more."
		       "How many potatoes does it take to kill Latvian? None."
		       "Two Latvian look at clouds. Whole sky is clouds. Weather is bad. Latvian are cold."
		       "Latvia jokes funny. Man laugh. Then soldier kill man and take potato. Man sad again."
		       "Is dead dog in road. Is dead Latvian in road. What difference? Dog have fur keep warm. Also, freedom. And dog try eat poop for pleasure not just survive. So many thing!"
		       "Is Latvian couple have been marry 60 years! But for long times, is no making sex. For 60th wedding anniversary, wife is buy for husband hooker for the have sex! Hooker is arrive at door one fine day and is say to husband, “Hello! I here give you super sex!” Man is say, “Oh! I will have the soup.” Then hooker is say, “What? You have soup? Why you no told this?”"
		       "Boy: But mother, I no are like grandma. Mother: Eat anyway. Is no potato."
		       "Latvian is rub lamp find genii. Genii say, “What is three wishes?” Latvian say, “I wish potato!” Then, POOF! Potato! Latvian so happy! “Oh! Is potato! Is potato!” say Latvian. Genii ask, “What is next wish?” Latvian is say, “I wish you go away so can enjoy potato!” POOF! Too bad. Also, was only lamp."
		       "Three Latvian are brag about sons. “My son is soldier. He have rape as many women as want,” say first Latvian. “Zo?” second say, “My son is farmer. He have all potato he want!” Third Latvian wait long time, then say, “My son is die at birth. For him, struggle is over.” “Wow! You are win us,” say others. But all are feel sad."
		       "Q: Why did chicken cross road? A: I have not seen chicken since I was very young, on my parents' farm. This is before the Cossacks slaughtered them. I can still hear screams of sister as soldiers rape her. But back to question, where did you see chicken? I am very, very hungry."
		       "\"Knock knock\" \"Who’s there?\" \"Latvian.\" \"Latvian who?\" \"Please open door. Is cold.\""
		       "Man find potato. Is actually rock. Such is life."
		       "Latvian walk into bar with poodle under one arm and salami under other. Eat salami first."
		       "Latvian walk into bar and say, “I have not eaten for many days. One full beer will be too much for me. How much just maybe one shot beer?” Bartender say, “This is can do for you. Is two centimes.” Latvian say, “Oh. I was hoping it would be less. I do not have that much.”"
		       "A fishmonger says to a bootblack, \"Are there any more potato left?\" Bootblack says, \"Yes, one. But it has gone bad.\" The fishmonger says, \"I am very hungry. I have not eaten for three days. I shall eat it, even if it makes me very ill.\" And bootblack says, \"I did not speak truth. In reality, there is no food left. You shall go hungry yet another day, my friend.\""
		       "Latvian try to cross river. Has dog, potatoes, and dead son's body. Can only take two across river at one time. If he leave dog with potatoes or corpse, dog eat them. Is very sad. Also is not good boat."
		       "Why is Latvian throw clock out window? Will be no appointments anymore, only endure til death."
		       "Man walk in bar with twelve-inch pianist. Is deformed by malnutrition."
		       "What is have four wheels and flies? Is body-disposal truck! Many have suicide this week."
		       "Two Latvians are argue over wodka. One say, \"For wodka, I give you daughter.\" Friend is say OK. Deal is struck. But he is surprise! She deformed by malnutrition."
		       "Man is have something on his mind. Is hat! And is crust of bread under hat, for to hide from family."
		       "Man is wait bread line. Wait until starve. Is very funny, yes!"
		       "Latvian walk into bar. Latvian cry, \"Ouch!\" Stomach is hurting with so little food."
		       "Q: Why potato is skin red? A: Latvian hungry. Soldier find Latvian steal potato. Shoot Latvian is blood, dead. All over potato."
		       "Latvian tell doctor is break leg in 2 place after drink wodka. doctor say quit going those places."
		       "Before you judge a Ukranian, walk a kilometer in his shoes. After that who care? He a kilometer away and you have his shoes. The end."
		       "Latvian psychiatrist tell me to drown my troubles. I go home and ask my wife go swimming. Trouble over. More potato for me."
		       "Latvian man go to doctor. 'Doctor doctor I have tumor growth in brain'. But Doctor dead due to poor health care infrastructure and no money for potato. Also man's tumor inoperable."
		       "In America: you have potato. In Latvia: you have potato?"
		       "Two Latvian are look at sun. Is not actually sun but meltdown from nuclear reactor. Maybe now am warm enough plant potato."
		       "American tell joke about Latvian starvation. Latvian no get joke (or potato). Great famine become worse, many suffer."
))
(define-reply meta-latvia '("<DAUGHTER RAPED BY SOLDIER>. all are sad. except <SOLDIER>."
		       "<POTATO> lose. <MALNOURISH>. such is life."
		       "<COLD> and <MALNOURISH>. such is life."
		       "Not happy with latvia? Good. No happy in latvia. only struggle and malnourish"
		       "<MALNOURISH>. struggle"
		       "<DEATH>. Happy because no more struggle"))

(let ((flipped nil))
  (define-reply table-flip (list (let ((result (if flipped
						   "(╯^_^）╯︵ ┬─┬"
						 "(╯°□°）╯︵ ┻━┻")))
				   (setq flipped (not flipped))
				   result))))

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
	     (letter-as-in "g" '("ghoti" "gnat" "gnome" "GNU/Linux"))
	     (letter-as-in "h" '("hour" "honor" "herb"))
	     (letter-as-in "k" '("knight" "knot"))
	     (letter-as-in "l" '("fifty"))
	     (letter-as-in "m" '("mnemonic" "thousand"))
	     (letter-as-in "o" '("opossum"))
	     (letter-as-in "p" '("pneumonia" "pneumatic" "psalm" "pterodactyl" "pseudoscience"))
	     (letter-as-in "q" '("qatar"))
	     (letter-as-in "t" '("tsar"))
	     (letter-as-in "v" '("five"))
	     (letter-as-in "w" '("write" "wraith" "write"))
	     (letter-as-in "x" '("xylophone" "Xilinx" "ten" "xenophobia"))
	     (letter-as-in "y" '("ypres")))))
  (defun nato-command (text process sender response target)
    (let ((arg (car (split-string text))))
      (when arg
	(random-choice (gethash (downcase arg) nato-table)))))
  (puthash "nato" 'nato-command command-table))

(define-reply cuisine '("sushi" "bbq" "thai" "tex mex" "mexican" "chinese" "vietnamese" "tacos/burritos" "korean" "americana" "sammiches" "burgers" "unhealthy food" "cajun" "seafood" "steak" "chicken" "pizza" "Reply hazy. Try again later." "indian"))
