;; -*- lexical-binding: t -*-

(require 'rcirc)
(require 'dom)
(require 'web)
(require 'cl)

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
	      (mapcar (lambda (r)
		(rcirc-send-message process target r))
		      reply-lines))))))

;; add the hook
(add-hook 'rcirc-print-functions 'my-rcirc-highlight-hook)

(defun my-rcirc-authed-hook (process sender response target text)
  (when (string= sender "NickServ")
    (let* ((words (split-string text))
	   (nick (elt words 0))
	   (second (elt words 1))
	   (auth-status (when (string= second "ACC")
			  (string= (elt words 2) "3"))))
      (when auth-status
	  (mapcar (lambda (f)
		    (funcall f))
		  (gethash nick pending-funcalls)))
      (remhash nick pending-funcalls))))
(add-hook 'rcirc-print-functions 'my-rcirc-authed-hook)

(defun append-hash (key value hash)
  (let ((pending-calls (gethash key hash)))
    (puthash key (append pending-calls (list value)) hash)))

(defun handle-command (text process sender response target)
  (let ((words (split-string text)))
    (let ((command (gethash (car words) command-table)))
      (when command
	(apply command (list (mapconcat 'identity (cdr words) " ") 
			       process 
			       sender 
			       response 
			       target))))))

(defun async-reply (process target reply)
  (rcirc-send-message process target reply))

(defun join-strings (words)
  (mapconcat 'identity words " "))

(defun handle-async-command (process sender response target text)
  (let ((words (split-string text)))
    (let ((command (gethash (car words) async-command-table)))
      (when command
	(funcall command (lambda (text)
			   (async-reply process target text))
		 (mapconcat 'identity (cdr words) " ")
		 process sender response target)))))

(defun authed-command-p (text)
  (let ((words (split-string text)))
    (gethash (car words) command-needs-auth-table)))

(defun async-command-p (text)
  (let ((words (split-string text)))
    (gethash (car words) async-command-table)))

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

(defun random-choice (choices)
  (if (and choices (listp choices))
	(let ((element 
	       (elt choices (random (length choices)))))
	  element)))

(defun random-command (text process sender response target)
  (let* ((words (split-string text)))
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

(define-reply yes-no '("yes" "no"))
(define-reply poop '("💩"))
(define-reply look-of-disapproval '("ಠ_ಠ"))

(setq flipped nil)
(define-reply table-flip (list (let ((result (if flipped
					     "(╯^_^）╯︵ ┬─┬"
					   "(╯°□°）╯︵ ┻━┻")))
			       (setq flipped (not flipped))
			       result)))
