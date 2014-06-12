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
) "Tells joke about latvia. You have potato?")

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

(define-reply cuisine '("sushi" "bbq" "thai" "tex-mex" "mexican" "chinese" "vietnamese" "tacos/burritos" "korean" "americana" "sammiches" "burgers" "unhealthy food" "cajun" "seafood" "steak" "chicken" "pizza" "Reply hazy. Try again later." "indian")
  "Tells you what kind of food to eat")
(define-reply sushi '("Musashino Sushi Dakoro" "Maki Toki" "Mikado Ryotei" "Zushi Sushi" "Ichiban" "Odaku Sushi")
  "Tells you where to get sushi in austin")
(define-reply bbq '("Mann's Smokehouse BBQ" "County Line on the hill" "Franklin's (sucks to be you)" "Pok-e-Jo's" "The Salt Lick"))
(define-reply thai '("Madam Mam's" "Satay")  "Tells you where to get thai food in austin")
(define-reply tex-mex '("La Casita" "Casa Chapala" "Juan in a Million" "Echiladas y mas")   "Tells you where to get tex-mex in austin")
(define-reply mexican '("La Casita" "Casa Chapala" "La Catedral del Marisco") "Tells you where to get mexican food in austin")
(define-reply chinese '("Din Ho" "TC Noodle House" "First Chinese BBQ")   "Tells you where to get chinese food in austin")
(define-reply vietnamese '("Pho Natic" "Pho Dan (Ph. D in Pho)" "Pho Thai Son" "Pho Saigon" "Lily's Sandwich (cash only)")
  "Tells you where to get vietnamese food in austin")
(define-reply tacos/burritos '("Tierra Linda" "La Casita" "Taco Shack" "Taco Deli") "Tells you where to get tacos / burritos in Austin")
(define-reply korean '("Korea House" "Ichiban" "Odaku Sushi" "Korean Grill") "Tells you where to get Korean food in Austin")
(define-reply americana '("The Frisco" "Ross' Old Austin Cafe" "Hyde Park Grill" "East Side Cafe" "Bartlett's")
  "Tells you where to get good-old-fashioned american food in Austin")
(define-reply sammiches '("Lily's Sandwich (cash only)" "Jimmy Johns" "Which wich" "Newk's") "Tells you where to get a sandwich in Austn")
(define-reply burgers '("The Frisco" "Hopdoddy" "Ross' Old Austin Cafe" "Moonie's Burger House" "Top Notch" "Whataburger (whataclassic)") "Tells you where to get a burger in Austin")
(define-reply cajun '("Shoal Creek Saloon" "Crawfish Shack Oyster Bar") "Tells you where to get cajun cuisine in Austin")
(define-reply seafood '("Shoal Creek Saloon" "Crawfish Shack Oyster Bar") "Tells you where where to get seafood in Austin")
(define-reply steak '("Ross Old Austin Cafe" "Bartlett's" "What's that place, again? the one with the steak that is so yum")
  "Tells you where to get steak in Austin")
(define-reply pizza '("Pinthouse Pizza" "Mangia" "Brooklyn Pie Co." "Marco's" "East Side Pies" "Conan's Pizza") "Tells you where to get yummy pizza in Austin")
(define-reply mediterranean '("Par's Deli" "Tino's" "Santorini") "Tells you where to get mediterranean food in Austin")
(define-reply indian '("Star of India" "The Clay Pit" "Tarka" "Masala Wok") "Tells you where to get Indian food in Austin")

(defun food-where (fn text process sender response target)
  "Tells you where to get food in austin by first picking a cuisine and then picking an option from that cuisine"
  (funcall (random-choice (list 'sushi
				'bbq
				'thai
				'tex-mex
				'mexican
				'chinese
				'vietnamese
				'tacos/burritos
				'korean
				'americana
				'sammiches
				'burgers
				'cajun
				'seafood
				'steak
				'pizza
				'mediterranean
				'indian))
	   fn text process sender response target))
(puthash "food-where?" 'food-where async-command-table)

(define-reply where '("Up your butt and around the corner" "Your mother would know") "Tells you where you might find something")

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
  problem
  (mapcar
   (apply-partially 'format "The cause of the problem is: %s")
   (list 
    "clock speed"
    "solar flares"
    "electromagnetic radiation from satellite debris"
    "static from nylon underwear"
    "static from plastic slide rules"
    "global warming"
    "poor power conditioning"
    "static buildup"
    "doppler effect"
    "hardware stress fractures"
    "magnetic interference from money/credit cards"
    "dry joints on cable plug"
    "we're waiting for [the phone company] to fix that line"
    "sounds like a Windows problem, try calling Microsoft support"
    "temporary routing anomaly"
    "somebody was calculating pi on the server"
    "fat electrons in the lines"
    "excess surge protection"
    "floating point processor overflow"
    "divide-by-zero error"
    "POSIX compliance problem"
    "monitor resolution too high"
    "improperly oriented keyboard"
    "network packets travelling uphill (use a carrier pigeon)"
    "Decreasing electron flux"
    "first Saturday after first full moon in Winter"
    "radiosity depletion"
    "CPU radiator broken"
    "It works the way the Wang did, what's the problem"
    "positron router malfunction"
    "cellular telephone interference"
    "techtonic stress"
    "piezo-electric interference"
    "(l)user error"
    "working as designed"
    "dynamic software linking table corrupted"
    "heavy gravity fluctuation, move computer to floor rapidly"
    "secretary plugged hairdryer into UPS"
    "terrorist activities"
    "not enough memory, go get system upgrade"
    "interrupt configuration error"
    "spaghetti cable cause packet failure"
    "boss forgot system password"
    "bank holiday - system operating credits  not recharged"
    "virus attack, luser responsible"
    "waste water tank overflowed onto computer"
    "Complete Transient Lockout"
    "bad ether in the cables"
    "Bogon emissions"
    "Change in Earth's rotational speed"
    "Cosmic ray particles crashed through the hard disk platter"
    "Smell from unhygienic janitorial staff wrecked the tape heads"
    "Little hamster in running wheel had coronary; waiting for replacement to be Fedexed from Wyoming"
    "Evil dogs hypnotised the night shift"
    "Plumber mistook routing panel for decorative wall fixture"
    "Electricians made popcorn in the power supply"
    "Groundskeepers stole the root password"
    "high pressure system failure"
    "failed trials, system needs redesigned"
    "system has been recalled"
    "not approved by the FCC"
    "need to wrap system in aluminum foil to fix problem"
    "not properly grounded, please bury computer"
    "CPU needs recalibration"
    "system needs to be rebooted"
    "bit bucket overflow"
    "descramble code needed from software company"
    "only available on a need to know basis"
    "knot in cables caused data stream to become twisted and kinked"
    "nesting roaches shorted out the ether cable"
    "The file system is full of it"
    "Satan did it"
    "Daemons did it"
    "You're out of memory"
    "There isn't any problem"
    "Unoptimized hard drive"
    "Typo in the code"
    "Yes, yes, its called a design limitation"
    "Look, buddy:  Windows 3.1 IS A General Protection Fault."
    "That's a great computer you have there; have you considered how it would work as a BSD machine?"
    "Please excuse me, I have to circuit an AC line through my head to get this database working."
    "Yeah, yo mama dresses you funny and you need a mouse to delete files."
    "Support staff hung over, send aspirin and come back LATER."
    "Someone is standing on the ethernet cable, causing a kink in the cable"
    "Windows 95 undocumented \"feature\""
    "Runt packets"
    "Password is too complex to decrypt"
    "Boss' kid fucked up the machine"
    "Electromagnetic energy loss"
    "Budget cuts"
    "Mouse chewed through power cable"
    "Stale file handle (next time use Tupperware(tm)!)"
    "Feature not yet implemented"
    "Internet outage"
    "Pentium FDIV bug"
    "Vendor no longer supports the product"
    "Small animal kamikaze attack on power supplies"
    "The vendor put the bug there."
    "SIMM crosstalk."
    "IRQ dropout"
    "Collapsed Backbone"
    "Power company testing new voltage spike (creation) equipment"
    "operators on strike due to broken coffee machine"
    "backup tape overwritten with copy of system manager's favourite CD"
    "UPS interrupted the server's power"
    "The electrician didn't know what the yellow cable was so he yanked the ethernet out."
    "The keyboard isn't plugged in"
    "The air conditioning water supply pipe ruptured over the machine room"
    "The electricity substation in the car park blew up."
    "The rolling stones concert down the road caused a brown out"
    "The salesman drove over the CPU board."
    "The monitor is plugged into the serial port"
    "Root nameservers are out of sync"
    "electro-magnetic pulses from French above ground nuke testing."
    "your keyboard's space bar is generating spurious keycodes."
    "the real ttys became pseudo ttys and vice-versa."
    "the printer thinks its a router."
    "the router thinks its a printer."
    "evil hackers from Serbia."
    "we just switched to FDDI."
    "halon system went off and killed the operators."
    "because Bill Gates is a Jehovah's witness and so nothing can work on St. Swithin's day."
    "user to computer ratio too high."
    "user to computer ration too low."
    "we just switched to Sprint."
    "it has Intel Inside"
    "Sticky bits on disk."
    "Power Company having EMP problems with their reactor"
    "The ring needs another token"
    "new management"
    "telnet: Unable to connect to remote host: Connection refused"
    "SCSI Chain overterminated"
    "It's not plugged in."
    "because of network lag due to too many people playing deathmatch"
    "You put the disk in upside down."
    "Daemons loose in system."
    "User was distributing pornography on server; system seized by FBI."
    "BNC (brain not connected)"
    "UBNC (user brain not connected)"
    "LBNC (luser brain not connected)"
    "disks spinning backwards - toggle the hemisphere jumper."
    "new guy cross-connected phone lines with ac power bus."
    "had to use hammer to free stuck disk drive heads."
    "Too few computrons available."
    "Flat tire on station wagon with tapes.  (\"Never underestimate the bandwidth of a station wagon full of tapes hurling down the highway\" Andrew S. Tannenbaum) "
    "Communications satellite used by the military for star wars."
    "Party-bug in the Aloha protocol."
    "Insert coin for new game"
    "Dew on the telephone lines."
    "Arcserve crashed the server again."
    "Some one needed the powerstrip, so they pulled the switch plug."
    "My pony-tail hit the on/off switch on the power strip."
    "Big to little endian conversion error"
    "You can tune a file system, but you can't tune a fish (from most tunefs man pages)"
    "Dumb terminal"
    "Zombie processes haunting the computer"
    "Incorrect time synchronization"
    "Defunct processes"
    "Stubborn processes"
    "non-redundant fan failure "
    "monitor VLF leakage"
    "bugs in the RAID"
    "no \"any\" key on keyboard"
    "root rot"
    "Backbone Scoliosis"
    "/pub/lunch"
    "excessive collisions & not enough packet ambulances"
    "le0: no carrier: transceiver cable problem?"
    "broadcast packets on wrong frequency"
    "popper unable to process jumbo kernel"
    "NOTICE: alloc: /dev/null: filesystem full"
    "pseudo-user on a pseudo-terminal"
    "Recursive traversal of loopback mount points"
    "Backbone adjustment"
    "OS swapped to disk"
    "vapors from evaporating sticky-note adhesives"
    "sticktion"
    "short leg on process table"
    "multicasts on broken packets"
    "ether leak"
    "Atilla the Hub"
    "endothermal recalibration"
    "filesystem not big enough for Jumbo Kernel Patch"
    "loop found in loop in redundant loopback"
    "system consumed all the paper for paging"
    "permission denied"
    "Reformatting Page. Wait..."
    "..disk or the processor is on fire."
    "SCSI's too wide."
    "Proprietary Information."
    "Just type 'mv * /dev/null'."
    "runaway cat on system."
    "Did you pay the new Support Fee?"
    "We only support a 1200 bps connection."
    "We only support a 28000 bps connection."
    "Me no internet, only janitor, me just wax floors."
    "I'm sorry a pentium won't do, you need an SGI to connect with us."
    "Post-it Note Sludge leaked into the monitor."
    "the curls in your keyboard cord are losing electricity."
    "The monitor needs another box of pixels."
    "RPC_PMAP_FAILURE"
    "kernel panic: write-only-memory (/dev/wom0) capacity exceeded."
    "Write-only-memory subsystem too slow for this machine. Contact your local dealer."
    "Just pick up the phone and give modem connect sounds. \"Well you said we should get more lines so we don't have voice lines.\""
    "Quantum dynamics are affecting the transistors"
    "Police are examining all internet packets in the search for a narco-net-trafficker"
    "We are currently trying a new concept of using a live mouse.  Unfortunately, one has yet to survive being hooked up to the computer.....please bear with us."
    "Your mail is being routed through Germany ... and they're censoring us."
    "Only people with names beginning with 'A' are getting mail this week (a la Microsoft)"
    "We didn't pay the Internet bill and it's been cut off."
    "Lightning strikes."
    "Of course it doesn't work. We've performed a software upgrade."
    "Change your language to Finnish."
    "Fluorescent lights are generating negative ions. If turning them off doesn't work, take them out and put tin foil on the ends."
    "High nuclear activity in your area."
    "What office are you in? Oh, that one.  Did you know that your building was built over the universities first nuclear research site? And wow, aren't you the lucky one, your office is right over where the core is buried!"
    "The MGs ran out of gas."
    "The UPS doesn't have a battery backup."
    "Recursivity.  Call back if it happens again."
    "Someone thought The Big Red Button was a light switch."
    "The mainframe needs to rest.  It's getting old, you know."
    "I'm not sure.  Try calling the Internet's head office -- it's in the book."
    "The lines are all busy (busied out, that is -- why let them in to begin with?)."
    "Jan  9 16:41:27 huber su: 'su root' succeeded for .... on /dev/pts/1"
    "It's those computer people in X {city of world}.  They keep stuffing things up."
    "A star wars satellite accidently blew up the WAN."
    "Fatal error right in front of screen"
    "That function is not currently supported, but Bill Gates assures us it will be featured in the next upgrade."
    "wrong polarity of neutron flow"
    "Lusers learning curve appears to be fractal"
    "We had to turn off that service to comply with the CDA Bill."
    "Ionization from the air-conditioning"
    "TCP/IP UDP alarm threshold is set too low."
    "Someone is broadcasting pygmy packets and the router doesn't know how to deal with them."
    "The new frame relay network hasn't bedded down the software loop transmitter yet. "
    "Fanout dropping voltage too much, try cutting some of those little traces"
    "Plate voltage too low on demodulator tube"
    "You did wha... oh _dear_...."
    "CPU needs bearings repacked"
    "Too many little pins on CPU confusing it, bend back and forth until 10-20% are neatly removed. Do _not_ leave metal bits visible!"
    "_Rosin_ core solder? But..."
    "Software uses US measurements, but the OS is in metric..."
    "The computer fleetly, mouse and all."
    "Your cat tried to eat the mouse."
    "The Borg tried to assimilate your system. Resistance is futile."
    "It must have been the lightning storm we had (yesterday) (last week) (last month)"
    "Due to Federal Budget problems we have been forced to cut back on the number of users able to access the system at one time. (namely none allowed....)"
    "Too much radiation coming from the soil."
    "Unfortunately we have run out of bits/bytes/whatever. Don't worry, the next supply will be coming next week."
    "Program load too heavy for processor to lift."
    "Processes running slowly due to weak power supply"
    "Our ISP is having {switching,routing,SMDS,frame relay} problems"
    "We've run out of licenses"
    "Interference from lunar radiation"
    "Standing room only on the bus."
    "You need to install an RTFM interface."
    "That would be because the software doesn't work."
    "That's easy to fix, but I can't be bothered."
    "Someone's tie is caught in the printer, and if anything else gets printed, he'll be in it too."
    "We're upgrading /dev/null"
    "The Usenet news is out of date"
    "Our POP server was kidnapped by a weasel."
    "It's stuck in the Web."
    "Your modem doesn't speak English."
    "The mouse escaped."
    "All of the packets are empty."
    "The UPS is on strike."
    "Neutrino overload on the nameserver"
    "Melting hard drives"
    "Someone has messed up the kernel pointers"
    "The kernel license has expired"
    "Netscape has crashed"
    "The cord jumped over and hit the power switch."
    "It was OK before you touched it."
    "Bit rot"
    "U.S. Postal Service"
    "Your Flux Capacitor has gone bad."
    "The Dilithium Crystals need to be rotated."
    "The static electricity routing is acting up..."
    "Traceroute says that there is a routing problem in the backbone.  It's not our problem."
    "The co-locator cannot verify the frame-relay gateway to the ISDN server."
    "High altitude condensation from U.S.A.F prototype aircraft has contaminated the primary subnet mask. Turn off your computer for 9 days to avoid damaging it."
    "Lawn mower blade in your fan need sharpening"
    "Electrons on a bender"
    "Telecommunications is upgrading. "
    "Telecommunications is downgrading."
    "Telecommunications is downshifting."
    "Hard drive sleeping. Let it wake up on it's own..."
    "Interference between the keyboard and the chair."
    "The CPU has shifted, and become decentralized."
    "Due to the CDA, we no longer have a root account."
    "We ran out of dial tone and we're and waiting for the phone company to deliver another bottle."
    "You must've hit the wrong any key."
    "PCMCIA slave driver"
    "The Token fell out of the ring. Call us when you find it."
    "The hardware bus needs a new token."
    "Too many interrupts"
    "Not enough interrupts"
    "The data on your hard drive is out of balance."
    "Digital Manipulator exceeding velocity parameters"
    "appears to be a Slow/Narrow SCSI-0 Interface problem"
    "microelectronic Riemannian curved-space fault in write-only file system"
    "fractal radiation jamming the backbone"
    "routing problems on the neural net"
    "IRQ-problems with the Un-Interruptible-Power-Supply"
    "CPU-angle has to be adjusted because of vibrations coming from the nearby road"
    "emissions from GSM-phones"
    "CD-ROM server needs recalibration"
    "firewall needs cooling"
    "asynchronous inode failure"
    "transient bus protocol violation"
    "incompatible bit-registration operators"
    "your process is not ISO 9000 compliant"
    "You need to upgrade your VESA local bus to a MasterCard local bus."
    "The recent proliferation of Nuclear Testing"
    "Elves on strike. (Why do they call EMAG Elf Magic)"
    "Internet exceeded Luser level, please wait until a luser logs off before attempting to log back on."
    "Your EMAIL is now being delivered by the USPS."
    "Your computer hasn't been returning all the bits it gets from the Internet."
    "You've been infected by the Telescoping Hubble virus."
    "Scheduled global CPU outage"
    "Your Pentium has a heating problem - try cooling it with ice cold water.(Do not turn off your computer, you do not want to cool down the Pentium Chip while he isn't working, do you?)"
    "Your processor has processed too many instructions.  Turn it off immediately, do not type any commands!!"
    "Your packets were eaten by the terminator"
    "Your processor does not develop enough heat."
    "We need a licensed electrician to replace the light bulbs in the computer room."
    "The POP server is out of Coke"
    "Fiber optics caused gas main leak"
    "Server depressed, needs Prozac"
    "quantum decoherence"
    "those damn raccoons!"
    "suboptimal routing experience"
    "A plumber is needed, the network drain is clogged"
    "50% of the manual is in .pdf readme files"
    "the AA battery in the wallclock sends magnetic interference"
    "the xy axis in the trackball is coordinated with the summer solstice"
    "the butane lighter causes the pincushioning"
    "old inkjet cartridges emanate barium-based fumes"
    "manager in the cable duct"
    "We'll fix that in the next (upgrade, update, patch release, service pack)."
    "HTTPD Error 666 : BOFH was here"
    "HTTPD Error 4004 : very old Intel cpu - insufficient processing power"
    "The ATM board has run out of 10 pound notes.  We are having a whip round to refill it, care to contribute ?"
    "Network failure -  call NBC"
    "Having to manually track the satellite."
    "Your/our computer(s) had suffered a memory leak, and we are waiting for them to be topped up."
    "The rubber band broke"
    "We're on Token Ring, and it looks like the token got loose."
    "Stray Alpha Particles from memory packaging caused Hard Memory Error on Server."
    "paradigm shift...without a clutch"
    "PEBKAC (Problem Exists Between Keyboard And Chair)"
    "The cables are not the same length."
    "Second-system effect."
    "Chewing gum on /dev/sd3c"
    "Boredom in the Kernel."
    "the daemons! the daemons! the terrible daemons!"
    "I'd love to help you -- it's just that the Boss won't let me near the computer. "
    "struck by the Good Times virus"
    "YOU HAVE AN I/O ERROR -> Incompetent Operator error"
    "Your parity check is overdrawn and you're out of cache."
    "Communist revolutionaries taking over the server room and demanding all the computers in the building or they shoot the sysadmin. Poor misguided fools."
    "Plasma conduit breach"
    "Out of cards on drive D:"
    "Sand fleas eating the Internet cables"
    "parallel processors running perpendicular today"
    "ATM cell has no roaming feature turned on, notebooks can't connect"
    "Webmasters kidnapped by evil cult."
    "Failure to adjust for daylight savings time."
    "Virus transmitted from computer to sysadmins."
    "Virus due to computers having unsafe sex."
    "Incorrectly configured static routes on the corerouters."
    "Forced to support NT servers; sysadmins quit."
    "Suspicious pointer corrupted virtual machine"
    "It's the InterNIC's fault."
    "Root name servers corrupted."
    "Budget cuts forced us to sell all the power cords for the servers."
    "Someone hooked the twisted pair wires into the answering machine."
    "Operators killed by year 2000 bug bite."
    "We've picked COBOL as the language of choice."
    "Operators killed when huge stack of backup tapes fell over."
    "Robotic tape changer mistook operator's tie for a backup tape."
    "Someone was smoking in the computer room and set off the halon systems."
    "Your processor has taken a ride to Heaven's Gate on the UFO behind Hale-Bopp's comet."
    "it's an ID-10-T error"
    "Dyslexics retyping hosts file on servers"
    "The Internet is being scanned for viruses."
    "Your computer's union contract is set to expire at midnight."
    "Bad user karma."
    "/dev/clue was linked to /dev/null"
    "Increased sunspot activity."
    "We already sent around a notice about that."
    "It's union rules. There's nothing we can do about it. Sorry."
    "Interference from the Van Allen Belt."
    "Jupiter is aligned with Mars."
    "Redundant ACLs. "
    "Mail server hit by UniSpammer."
    "T-1's congested due to porn traffic to the news server."
    "Data for intranet got routed through the extranet and landed on the internet."
    "We are a 100% Microsoft Shop."
    "We are Microsoft.  What you are experiencing is not a problem; it is an undocumented feature."
    "Sales staff sold a product we don't offer."
    "Secretary sent chain letter to all 5000 employees."
    "Sysadmin didn't hear pager go off due to loud music from bar-room speakers."
    "Sysadmin accidentally destroyed pager with a large hammer."
    "Sysadmins unavailable because they are in a meeting talking about why they are unavailable so much."
    "Bad cafeteria food landed all the sysadmins in the hospital."
    "Route flapping at the NAP."
    "Computers under water due to SYN flooding."
    "The vulcan-death-grip ping has been applied."
    "Electrical conduits in machine room are melting."
    "Traffic jam on the Information Superhighway."
    "Radial Telemetry Infiltration"
    "Cow-tippers tipped a cow onto the server."
    "tachyon emissions overloading the system"
    "Maintenance window broken"
    "We're out of slots on the server"
    "Computer room being moved.  Our systems are down for the weekend."
    "Sysadmins busy fighting SPAM."
    "Repeated reboots of the system failed to solve problem"
    "Feature was not beta tested"
    "Domain controller not responding"
    "Someone else stole your IP address, call the Internet detectives!"
    "It's not RFC-822 compliant."
    "operation failed because: there is no message for this error (#1014)"
    "stop bit received"
    "internet is needed to catch the etherbunny"
    "network down, IP packets delivered via UPS"
    "Firmware update in the coffee machine"
    "Temporal anomaly"
    "Mouse has out-of-cheese-error"
    "Borg implants are failing"
    "Borg nanites have infested the server"
    "error: one bad user found in front of screen"
    "Please state the nature of the technical emergency"
    "Internet shut down due to maintenance"
    "Daemon escaped from pentagram"
    "crop circles in the corn shell"
    "sticky bit has come loose"
    "Hot Java has gone cold"
    "Cache miss - please take better aim next time"
    "Hash table has woodworm"
    "Trojan horse ran out of hay"
    "Zombie processes detected, machine is haunted."
    "overflow error in /dev/null"
    "Browser's cookie is corrupted -- someone's been nibbling on it."
    "Mailer-daemon is busy burning your message in hell."
    "According to Microsoft, it's by design"
    "vi needs to be upgraded to vii"
    "greenpeace free'd the mallocs"
    "Terrorists crashed an airplane into the server room, have to remove /bin/laden. (rm -rf /bin/laden)"
    "astropneumatic oscillations in the water-cooling"
    "Somebody ran the operating system through a spelling checker."
    "Rhythmic variations in the voltage reaching the power supply."
    "Keyboard Actuator Failure.  Order and Replace."
    "Packet held up at customs."
    "Propagation delay."
    "High line impedance."
    "Someone set us up the bomb."
    "Power surges on the Underground."
    "Don't worry; it's been deprecated. The new one is worse."
    "Excess condensation in cloud network"
    "It is a layer 8 problem"
    "The math co-processor had an overflow error that leaked out and shorted the RAM"
    "Leap second overloaded RHEL6 servers"
    "DNS server drank too much and had a hiccup"))
  "Gives you BOFH excuse")

(define-reply
  pig
  (list
   (list "The Pig, if I am not mistaken,"
	 "Gives us ham and pork and Bacon."
	 "Let others think his heart is big,"
	 "I think it stupid of the Pig.")
   "For Pig so loved the world that He gave them bacon to dine upon."
   "In the beginning, food was formless and grey. Into this blandness Pig created bacon, and tasted that it was good.")
  "Quotes about pig")
(define-reply
  keyswitch
  (list
   "Cherry MX Red"
   "Cherry MX Blue"
   "Cherry MX Black"
   "Cherry MX Brown"
   "Cherry MX Clear"
   "Cherry MX Green"
   "Topre 45g"
   "Topre 55g"
   "Topre Ergo-weighting"
   "Membrane Buckling Spring"
   "Capacitive Buckling Spring"
   "Clicky Alps"
   "Linear Alps"
   "Space Invader"
   "Cherry ML"
   "Cherry MY"
   "MEI")
  "Suggests a keyswitch")

(define-reply
  shh
  '("http://tinyurl.com/qjsfxyp")
  "Shushes you")

