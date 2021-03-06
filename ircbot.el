;; -*- lexical-binding: t -*-

(require 'rcirc)
(require 'dom)
(require 'web)
(require 'cl)
(require 'list-utils)

;; lookup in command-table the function for a command if it exists, call it and pass it the rest of the args.
(setq command-table (make-hash-table :test 'equal))
(setq custom-command-table (make-hash-table :test 'equal))
(setq async-command-table (make-hash-table :test 'equal))
(setq command-needs-auth-table (make-hash-table :test 'equal))
(setq pending-funcalls (make-hash-table :test 'equal))

;; hook for rcirc print    
(defun my-rcirc-highlight-hook (process sender response target text)
  "Highlight hook that hooks in commands to rcirc when bot is highlighted."
  (when (and (or (string-match (concat "^" (regexp-quote (rcirc-nick process))) text)
		 (string-match (concat "^" (regexp-quote "(")) text))
	     (not (string= (rcirc-server-name process) sender)))
    ;; do your stuff
    (message "%s" text)
    (let ((text
	   (replace-regexp-in-string (concat "^" (regexp-quote "(")) 
				     ""
				     (replace-regexp-in-string (format "^%s[^a-zA-Z0-9]*"
								       (regexp-quote (rcirc-nick process))) "" text))))
      (message "stripped text: %s" text)
      (handle-authable-command
       (gethash (car (split-string text)) command-needs-auth-table)
       process sender target text))))

(defun handle-authable-command (auth process sender target text)
  "Handles a potentially authable command.

An authable command is one which the bot operator feels should require
authentication due to being unsafe. At the moment, authable commands
do not exist and are not handled, ironically enough."
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
					  (format "%s" reply)) nil))))
	      (mapcar (apply-partially 'rcirc-message process target)
		      reply-lines))))))

(defun rcirc-message (process target line)
  (let ((words (split-string line)))
    (if (equal "/me" (car words))
	(rcirc-cmd-me (join-strings (cdr words)) process target)
      (rcirc-send-message process target line))))
;; add the hook
(defun my-rcirc-url-hook (process sender response target text)
  "Hook that looks for urls in the text of a message that are not
already tinyurl'd.

The bot handles said urls by translating it to being the same as
the sender instructing the bot to tinyurl the url."
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
		    (tinyurl (apply-partially 'async-reply process target) url)
		    ;; (funcall (apply-partially 'handle-async-command process sender nil target)
		    ;; 	     (format "tinyurl %s" url))
		    )
		  urls)))))

;; add our print-function hooks
(add-hook 'rcirc-print-functions 'my-rcirc-highlight-hook)
(add-hook 'rcirc-print-functions 'my-rcirc-url-hook)
(defun rcirc-handler-NOTICE (process sender args text)
  (rcirc-check-auth-status process sender args text)
  (let ((target (car args))
        (message (cadr args)))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP-response process target sender
				     (match-string 1 message)))))

(defun my-rcirc-authed-hook (process sender response target text)
  "A hook we are not using yet which will parse the response
from asking NickServ if nick is identified as who they are.
After it has determined they are who they say, it will execute
pending funcalls for said nick"
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
  "Appends a pending funcall passed in as value to the list
currently in the key entry of hash."
  (let ((pending-calls (gethash key hash)))
    (puthash key (append pending-calls (list value)) hash)))

(defun handle-command (text process sender response target)
  "Handles a plain, old command synchronously. reads text for command
and responds to with the output of the command if it exists"
  (let ((words (split-string text)))
    (let ((command (let ((builtin (gethash (downcase (car words)) command-table)))
		     (if builtin
			 builtin
		       (gethash (downcase (car words)) custom-command-table)))))
      (when command
	(apply command (list (mapconcat 'identity (cdr words) " ") 
			       process 
			       sender 
			       response 
			       target))))))

(defun async-reply (process target reply)
  "Reply function to be generally bound to a closure, passed around,
potentially stored and called later. It will message reply
to the bound process and target"
  (let ((reply-lines (if (listp reply)
			 reply
		       (cons (if (stringp reply)
				 reply
			       reply) nil))))
    (mapcar (apply-partially 'rcirc-send-message process target)
	    reply-lines)))

(defun join-strings (words)
  "Basically a simple reverse of split-string
Joins words together with \" \" into a single string"
  (mapconcat 'identity words " "))

(defun handle-async-command (process sender response target text)
  "Handles a command that claims to be asynchronous.
An async-command is stored in async-command-table and
takes a fn as an argument which it may pass into a callback
for a long running operation. the fn will take an argument of the
text to reply with."
  (let ((words (split-string text)))
    (let ((command (gethash (downcase (car words)) async-command-table)))
      (when command
	(funcall command (apply-partially
			  'async-reply process target)
		 (mapconcat 'identity (cdr words) " ")
		 process sender response target)))))

(defun authed-command-p (text)
  "Non-nil when command in text requires auth"
  (let ((words (split-string text)))
    (gethash (downcase (car words)) command-needs-auth-table)))

(defun async-command-p (text)
  "Non-nil when command in text is async"
  (let ((words (split-string text)))
    (gethash (downcase (car words)) async-command-table)))

;; Let's write some commands.
(defun commands-command (text process sender response target)
  "Command to return built-in commands of bot"
  (let (commands)
    (let ((fn (lambda (key value)
		(setq commands (cons key commands)))))
    (maphash fn
	     command-table)
    (maphash fn async-command-table)
    (mapconcat 'identity (sort commands 'string<) " "))))
(puthash "commands" 'commands-command command-table)

(defun custom-commands-command (text process sender response target)
  "Command to return user-defined commands of bot"
  (mapconcat 
   'identity
   (sort
    (remove-if (lambda (k)
		 (or (gethash k command-table)
		     (gethash k async-command-table)))
	       (let
		   (commands)
		 (maphash (lambda (k v)
			    (setq commands (cons k commands)))
			  custom-command-table)
		 commands))
    'string<)
   " "))
(puthash "custom-commands" 'custom-commands-command command-table)

(defun source-command (text process sender response target)
  "Returns URL to source of this bot"
  "https://github.com/sauerkrause/bot.emacs")
(puthash "source" 'source-command command-table)

(defun spit-page (fn url)
  "Tells web-http-get to call fn with the page-data of the 
url argument when it has retrieved the page."
  (web-http-get (lambda (httpc header page-data)
		  (funcall fn page-data))
		:url url))

(defun html-from-body (body)
  "Returns output of libxml-parse-html-region
for a given html content"
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
  (message "%s -> %s" url page-data)
  (let ((extra-headers (make-hash-table :test 'equal)))
    (puthash 'user-agent "Mozilla/5.0 (iPad; U; CPU OS 3_2_1 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Mobile/7B405" extra-headers)
  (web-http-get
      (lambda (httpc header body)
	(message "%s" body)
	(let ((content-type (gethash 'content-type header)))
	  (if (equal "301" (gethash 'status-code header))
	      (tinyurl fn (gethash 'location header))
	    (if (equal "text/html" (car (split-string content-type "; ")))
		(funcall fn
			 (format "%s%s" page-data 
				 (let ((title (find-title body)))
				   (if title
				       (format " : (%s)" title)
				     ""))))
	      (funcall fn page-data)))))
      :url url
      :extra-headers extra-headers)))

(defun tinyurl (fn url)
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'url url query-data)
    (web-http-post
     (lambda (httpc header page-data)
       (put-title-tinyurl fn url page-data))
     :url "http://tinyurl.com/api-create.php"
     :data query-data)))

(defun tinyurl-command (fn text process sender response target)
  (let* ((url (car (split-string text))))
    (tinyurl fn url)))
(puthash "tinyurl" 'tinyurl-command async-command-table)
(remhash "tinyurl" async-command-table)
(defun xml-from-body (body)
  (with-temp-buffer
    (insert body)
    (xml-parse-region (point-min) (point-max))))

(defun rfc (fn number)
  (let ((query-data (make-hash-table :test 'equal))
	(url (format "http://tools.ietf.org/html/rfc%d" number)))
    (puthash 'url url query-data)
    (web-http-post
     (lambda (httpc header page-data)
       (put-title-tinyurl fn url page-data))
     :url "http://tinyurl.com/api-create.php"
     :data query-data)))

(defun rfc-command (fn text process sender response target)
  (let* ((num (car (split-string text))))
    (tinyurl fn (format "http://tools.ietf.org/html/rfc%s" num))))
(puthash "rfc" 'rfc-command async-command-table)

(defun random-choice (choices)
  (if (and choices (listp choices))
	(let ((element 
	       (elt choices (random (length choices)))))
	  element)))

(defmacro define-reply (cmd choices &optional docstring)
  `(progn
     (defun ,cmd (fn text process sender response target)
       ,docstring
       (let ((choices ,choices))
	 (funcall fn (random-choice choices))))
     (puthash (symbol-name ',cmd) ',cmd async-command-table)))


(defun define-command (text process sender response target)
  "Lets users define a custom command for the bot"
  (let ((name (downcase (car (split-string text))))
	(replies (cdr (split-string text))))
    (puthash name (apply-partially
		    (lambda (r txt p s resp trgt)
		      (random-choice r))
		    replies)
	     custom-command-table)
    (format "%s defined!"
	    (if (gethash name custom-command-table)
		name
	      (format "%s could not be" name)))))
(puthash "define" 'define-command command-table)

(defun undefine-command (text process sender response target)
  "Lets users undefine a custom command"
  (mapcar (lambda (i)
	    (remhash i custom-command-table))
	  (split-string text))
  (format "Deleted %s" text))
(puthash "undefine" 'undefine-command command-table)

(defun purge-customs-command (text process sender response target)
  "Removes all custom commands"
  (maphash (lambda (k v)
	     (remhash k custom-command-table))
	   custom-command-table)
  (setq custom-command-table (make-hash-table :test 'equal)))
(puthash "purge" 'purge-customs-command command-table)

(defun man-command (text process sender response target)
  "Returns the documentation string for a given function, but you probably already knew that considering you just did it."
  (let ((name (downcase (car (split-string text))))
	(fn nil))
    (setq fn (gethash name command-table))
    (unless fn
      (setq fn (gethash name async-command-table)))
    (if fn
	(documentation fn))))
(puthash "man" 'man-command command-table)
(define-reply help '("see 'commands' for a list of available commands. use 'man <command>' to get help for a particular command") "Gives assistance in using bot")
