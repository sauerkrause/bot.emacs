;; -*- lexical-scoping: t -*-
(setq channel-table (make-hash-table :test 'equal))

;; use the channel-mode-table to keep track of modes in channel
(setq channel-mode-table (make-hash-table :test 'equal))

(setq async-instance 0)
(defun async-shell-command (callback name &rest args)
  "Runs program NAME with arguments ARGS. When output is received from program, 
CALLBACK will be called. Returns process identifier"
    (let ((proc (apply 'start-process (format "%s+%d" name async-instance) nil name args)))
      (set-process-filter proc callback)
      (setq async-instance (+ async-instance 1))
      proc))

(defun get-item (triplet n)
  (let ((list (split-string triplet "\n")))
    (replace-regexp-in-string 
     "\"" 
     ""
     (replace-regexp-in-string 
      (format "^%d) " (+ n 1)) 
      "" 
      (elt list n)))))
(defun get-origin (triplet)
  (get-item triplet 1))
(defun get-message (triplet)
  (get-item triplet 2))

(defun build-reply (irc-proc target process output)
  "async-reply to TARGET in IRC-PROC. OUTPUT
is the last stdout received from PROCESS as a single string"
  (let ((origin (get-origin output))
	(message (get-message output)))
    (async-reply irc-proc target
		 (message "%s: %s" origin message))))

(defun redis-subscribe (text process sender response target)
  "subscribes to a redis pubsub. ARGS are a space separated
list of redis-channels to subscribe to"
  (let ((channels (split-string text)))
    (message "%s->%s" target channels)  
    (mapcar (lambda (channel)
	      ;; need identifier to combine redis-channel AND target
	      (let ((id (format "%s:%s" channel target)))
		(unless (gethash id channel-table)
		  (let ((proc (async-shell-command
			       (apply-partially 'build-reply process target)
			       "redis-cli"
			       "subscribe"
			       channel)))
		    (puthash id proc channel-table)))))
	      channels)))

(puthash "subscribe" 'redis-subscribe command-table)

(defun redis-stop (text process sender response target)
  "Cancels all subscriptions to redis"
    (maphash (lambda (key value)
	       (delete-process value)) channel-table)
    (clrhash channel-table)
    nil)
(puthash "unsubscribe" 'redis-stop command-table)

(require 'eredis)
(setq *redis-host* "localhost")
(setq *redis-port* "6379")
(setq *redis-channel* "irc")
(defun subscribed-target-p (text)
  (let (subscribedp)
    (maphash (lambda (key value)
	       (let ((target (car (last (split-string key ":")))))
		 (message "(%s)=(%s)" target text)
		 (when (string= target text)
		   (setq subscribedp t))))
	     channel-table)
    subscribedp))

(defmacro with-redis-connection (host port &rest rest)
  `(progn
     (eredis-connect ,host ,port)
     (unwind-protect
	 (progn ,@rest)
       (eredis-disconnect))))

(defun irc-broadcast-hook (process sender response target text)
  "Broadcasts messages received in subscribed targets to IRC channel in redis"
  (when (subscribed-target-p target)
    (with-redis-connection 
     *redis-host* *redis-port*
	  (eredis-publish *redis-channel* (format "%s:<%s> %s" target sender text)))))
(add-hook 'rcirc-print-functions 'irc-broadcast-hook)

(defun gnufo-cmd-kick (args process target)
  (message "calling gnufo-cmd-kick")
  (if (opp process target)
      (progn (message "kicking %s" (car args))
	     (message "%s" args)
	     (rcirc-cmd-kick (join-strings args) process target))
    (progn (message "actioning %s" (car args))
	   (format "/me kicks %s [%s]" 
		   (car args)
		   (join-strings (cdr args))))))

(defun parse-modes (mode-str)
  (let ((mode-values nil)
	(mode-pos nil))
    (dolist (char (string-to-list mode-str) mode-values)
      (cond ((eq char ?+) (setq mode-pos ?+))
	    ((eq char ?-) (setq mode-pos ?-))
	    (t (setq mode-values (cons (format "%c%c" mode-pos char) mode-values)))))))

(defun apply-modes (modes users)
  (let (mode-pairs)
    (dotimes (i (length modes) mode-pairs)
      (setq mode-pairs (cons (cons (elt modes i) (elt users i)) mode-pairs)))))
	   
(defun merge-modes (modes diff)
  (if (eq (elt diff 0) ?-)
      (remq (elt diff 1) modes)
    (cons (elt diff 1) modes)))

(defun handle-mode-change (target args)
  (let ((mode-pair-list (apply-modes (parse-modes (car args))
	       (cdr args))))
    (let ((channel (gethash target channel-mode-table (make-hash-table :test 'equal))))
      (mapcar (lambda (pair)
		(let ((mode (gethash (cdr pair) channel nil)))
		  (message "%s" (merge-modes mode (car pair)))
		  (message "puthashing %s" (cdr pair))
		  (puthash (cdr pair) (merge-modes mode (car pair)) channel)))
	      mode-pair-list)
      (puthash target channel channel-mode-table)
      nil)))

(defun rcirc-handler-KICK (process sender args text)
  (let* ((channel (car args))
	 (nick (cadr args))
	 (reason (caddr args))
	 (message (concat nick " " channel " " reason)))
    (rcirc-print process sender "KICK" channel message t)
    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) nick)
      (rcirc-print process sender "KICK" nick message))
    (message "Handling kick of %s" nick)
    (gandhi (apply-partially 'async-reply process channel) text process sender nil channel)
    (rcirc-handler-PART-or-KICK process "KICK" channel sender nick reason)))

(defun rcirc-handler-INVITE (process sender args text)
  (message "%s" args)
  (rcirc-cmd-join (mapconcat 'identity
			     (remove-if-not (lambda (x)
					      (if (arrayp x)
						  (eq ?# (aref x 0))))
						      args) " "))
  (rcirc-print process sender "INVITE" nil (mapconcat 'identity args " ") t))


(defun rcirc-handler-PART-or-KICK (process response channel sender nick args)
  ;; clear modes for nick
  (puthash nick nil (gethash channel channel-mode-table))
  (rcirc-ignore-update-automatic nick)
  (if (not (string= nick (rcirc-nick process)))
      ;; this is someone else leaving
      (progn
	(rcirc-maybe-remember-nick-quit process nick channel)
	(rcirc-remove-nick-channel process nick channel))
    ;; this is us leaving
    (mapc (lambda (n)
	    (rcirc-remove-nick-channel process n channel))
	  (rcirc-channel-nicks process channel))

    ;; if the buffer is still around, make it inactive
    (let ((buffer (rcirc-get-buffer process channel)))
      (when buffer
	(rcirc-disconnect-buffer buffer)))))

(defun rcirc-handler-JOIN (process sender args text)
  (let ((channel (car args)))
    (puthash channel 
	     (make-hash-table :test 'equal)
	     channel-mode-table)
    (with-current-buffer (rcirc-get-buffer-create process channel)
      ;; when recently rejoining, restore the linestamp
      (rcirc-put-nick-channel process sender channel
			      (let ((last-activity-lines
				     (rcirc-elapsed-lines process sender channel)))
				(when (and last-activity-lines
					   (< last-activity-lines rcirc-omit-threshold))
                                  (rcirc-last-line process sender channel))))
      ;; reset mode-line-process in case joining a channel with an
      ;; already open buffer (after getting kicked e.g.)
      (setq mode-line-process nil))

    (rcirc-print process sender "JOIN" channel "")

    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) sender)
      (rcirc-print process sender "JOIN" sender channel))))

(defun rcirc-handler-MODE (process sender args text)
  (let ((target (car args))
	(msg (mapconcat 'identity (cdr args) " ")))
    (handle-mode-change target (cdr args))
    (message "%s" args)
    (rcirc-print process sender "MODE"
		 (if (string= target (rcirc-nick process))
		     nil
		   target)
		 msg)
    ;; print in private chat buffers if they exist
    (mapc (lambda (nick)
	    (when (rcirc-get-buffer process nick)
	      (rcirc-print process "MODE" nick msg)))
	  (cddr args))))
  
(defun opp (process target &optional nick)
  (unless nick
    (setq nick (rcirc-nick process)))
  (let ((modes (gethash nick (gethash target channel-mode-table))))
    (message "%s" modes)
    (memq ?o modes)))

(defun kick-command (text process sender response target)
  "kicks a user"
  (if (opp process target (car (split-string text)))
      "No. Such scare. Much hat."
    (gnufo-cmd-kick (split-string text) process target)))

(puthash "kick" 'kick-command command-table)

(defun hats-command (text process sender response target)
  "Gives everybody hats in channel if able."
  (mapcar
   (lambda (x)
     (let ((msg (format "OP %s %s" target x)))
       (rcirc-cmd-mode (format "%s +o %s" target x))
       ;(rcirc-send-message process "ChanServ" msg)
       (message "%s" msg)))
   (rcirc-channel-nicks process target))
    "Hats gaven.")
(puthash "hats" 'hats-command command-table)
(puthash "tf2" 'hats-command command-table)

(defun unhats-command (text process sender response target)
  "Takes away everybody's hats in channel if able."
  (mapcar
   (lambda (x)
     (let ((msg (format "OP %s -%s" target x)))
       (rcirc-cmd-mode (format "%s -o %s" target x))
;       (rcirc-send-message process "ChanServ" msg)
       (message "%s" msg)))
   (rcirc-channel-nicks process target))
    "Hats removed.")
(puthash "de-tf2" 'unhats-command command-table)


