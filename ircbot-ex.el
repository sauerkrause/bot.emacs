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
(define-reply bbq '("Mann's Smokehouse BBQ" "County Line on the hill" "Franklin's (sucks to be you)" "Pok-e-Jo's" "The Salt Lick") "tells you where to get barbecue in Austn")
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
(puthash "food-where" 'food-where async-command-table)

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

(define-reply
  sting
  '("ba-dum *ching*")
  "Stings")

(define-reply 
  nethack
  '("A blindfold can be very useful if you're telepathic."

"A candelabrum affixed with seven candles shows the way with a magical light."

"A crystal plate mail will not rust."

"A katana might slice a worm in two."

"A magic vomit pump could be useful for gourmands."

"A nymph knows how to unlock chains."

"A potion of blindness lets you see invisible things."

"A priest can get the gods to listen easily."

"A priestess and a virgin you might be, but that unicorn won't care."

"A ring of conflict is a bad thing if there is a nurse in the room."

"A short sword is not as good as a long sword."

"A succubus will go farther than a nymph."

"A wand can exorcize a past explorer's ghost."

"Acid blobs should be attacked bare-handed."

"Affairs with nymphs are often very expensive."

"Afraid of nymphs?  Wear a ring of adornment."

"Afraid of your valuables being stolen?  Carry more junk!"

"Always be aware of the phase of the moon!"

"Always sweep the floor before engraving important messages."

"Amulets of Yendor are hard to make.  Even for a wand of wishing."

"An elven cloak protects against magic."

"An umber hulk can be a confusing sight."

"As Crom is my witness, I'll never go hungry again!"

"Asking about monsters may be very useful."

"Attack long worms from the rear -- that is so much safer!"

"Attacking an eel where there is none is usually a fatal mistake!"

"Bandaging wounds helps keep up appearances."

"Bashing monsters with a bow is not such a good idea."

"Be careful!  The Wizard may plan an ambush!"

"Be nice to a nurse:  Put away your weapon and take off your clothes."

"Being digested is a painfully slow process."

"Blank scrolls make more interesting reading."

"Blind?  Catch a floating eye!"

"Booksellers never read scrolls; they might get carried away."

"Chemistry 101: Never pour water into acid."

"Concise conquest:  Control, confuse, conjure, condemn."

"Conserve energy, turn off the lights."

"Digging up a grave could be a bad idea..."

"Dilithium crystals are rare indeed."

"Dogs are attracted by the smell of tripe."

"Dogs are superstitious; they never step on cursed items."

"Dogs of ghosts aren't angry, just hungry."

"Don't forget!  Large dogs are MUCH harder to kill than little dogs."

"Don't mess with shopkeepers, or you'll get the Guild after you."

"Dragons never whip their children; they wouldn't feel it!"

"Eat your carrots.  They're good for your eyes."

"Eating a freezing sphere is like eating a yeti."

"Eating a killer bee is like eating a scorpion."

"Eating a tengu is like eating a nymph."

"Eating a wraith is a rewarding experience!"

"Eating unpaid leprechauns may be advantageous."

"Elbereth has quite a reputation around these parts."

"Elf corpses are incompatible with the sandman, and at times the gods as well."

"Elven cloaks cannot rust."

"Even evil players have a guardian angel."

"Ever fought with an enchanted tooth?"

"Ever tried reading while confused?"

"Ever tried to put a troll into a large box?"

"Ever wondered why one would want to dip something in a potion?"

"Expensive cameras have penetrating flash lights."

"Extra staircases lead to extra levels."

"Fiery letters might deter monsters."

"For a good time engrave `Elbereth'."

"Gems are too precious to be thrown away carelessly."

"Getting hungry?  Stop wearing rings!"

"Getting too warm?  Take off that Amulet of Yendor and stay away from the exit!"

"Gods expect the best from their priesthood."

"Gods look down their noses at demigods."

"Got a question?  Try rec.games.roguelike.nethack."

"Grave robbers sometimes get rich."

"Guy Montag keeps his scrolls in a bag."

"Handle your flasks carefully -- there might be a ghost inside!"

"Holy water has many uses."

"Horses trust their riders, even when not so deserved."

"Hunger is a confusing experience for a dog!"

"I once knew a hacker who ate too fast and choked to death."

"I smell a maze of twisty little passages."

"I wish I never wished a wand of wishing.  (Wishful thinking.)"

"I wouldn't advise playing catch with a giant."

"I'm watching you.  -- The Wizard of Yendor"

"Ice boxes keep your food fresh."

"If you are being punished, it's done with a deadly weapon."

"If you kill the Wizard, you get promoted to demi-god."

"If you need a wand of digging, kindly ask the minotaur."

"If you want to hit, use a dagger."

"If you want to rob a shop, train your dog."

"If you're lost, try buying a map next time you're in a shop."

"Inside a shop you better take a look at the price tags before buying anything."

"It is bad manners to use a wand in a shop."

"It is dangerous to visit a graveyard at midnight."

"It is not always a good idea to whistle for your dog."

"It is rumored that the Wizard has hired some help."

"It is the letter 'c' and not 'e' that changes status to statue."

"It might be a good idea to offer the unicorn a ruby."

"It would be peculiarly sad were your dog turned to stone."

"It's a `d' eats `d' world."

"Keep your armors away from rust."

"Keep your weaponry away from acids."

"Kill a unicorn of your color and you kill your luck."

"Leather is waterproof.  Ever see a cow with an umbrella?"

"Leprechauns are the most skilled cutpurses in this dungeon."

"Lizard corpses protect against cockatrices."

"Money lost, little lost; honor lost, much lost; pluck lost, all lost."

"Most monsters can't swim."

"Music hath charms to affect the stubborn drawbridge."

"Music hath charms to soothe the savage beast."

"Never attack a guard."

"Never ride a long worm."

"Never use your best weapon to engrave a curse."

"No easy fighting with a heavy load!"

"Nurses are trained to touch naked persons:  they don't harm them."

"Nymphs can unlink more than your chain mail."

"Once your little dog will be a big dog, and you will be proud of it."

"Only female monsters can lay eggs."

"Opening a tin is difficult, especially when you attempt it bare handed!"

"Orcs and killer bees share their lifestyle."

"Orcs do not procreate in dark rooms."

"Plain nymphs are harmless."

"Playing AD&D may be helpful."

"Playing Gauntlet might be enlightening in some situations."

"Playing billiards pays when you are in a shop."

"Polymorphing a shopkeeper might make you safer."

"Polymorphing your dog probably makes you safer."

"Potions don't usually mix, but sometimes..."

"Psst!  It's done with mirrors!"

"Put on a ring of teleportation:  it will take you away from onslaught."

"Rays aren't boomerangs, of course, but still..."

"Read the manual before entering the cave -- you might get killed otherwise."

"Reading Herbert might be enlightening in one case."

"Reading Tolkien might help you."

"Reading scrolls after drinking booze can give confusing results."

"Riding a dragon can be an uplifting experience."

"Rust monsters love water.  There are potions they hate, however."

"Sacks protect contents from temperatures up to 452 degrees fahrenheit."

"Scrolls fading?  It's not the heat, it's the humidity."

"Shopkeepers accept credit cards, as long as you pay cash."

"Shopkeepers can spot a tourist a mile away with those Hawaiian shirts."

"Shopkeepers can't tell identical twins apart."

"Shopkeepers don't read, so what use is engraving in a shop?"

"Shopkeepers have incredible patience."

"Shopkeepers might raise their prices for tourists."

"Shopkeepers value money more than revenge."

"Some monsters can be tamed.  I once saw a hacker with a tame dragon!"

"Someone once said that what goes up < might come down >."

"Someone's been spiking the pits!"

"Sometimes monsters are more likely to fight each other than attack you."

"Spinach, carrot, and jelly -- a meal fit for a nurse!"

"Tainted meat is even more sickening than poison!"

"Telepathy is just a trick:  once you know how to do it, it's easy."

"The Leprechaun Gold Tru$t is no division of the Magic Memory Vault."

"The Wizard finds death to be quite an experience."

"The best equipment for your work is, of course, the most expensive."

"The gods don't appreciate pesky priesthood."

"The gods will get angry if you kill your dog."

"The magic marker is mightier than the sword."

"The moon is not the only heavenly body to influence this game."

"The orc swings his orcish broadsword named Elfrist at you.  You die..."

"The secret of wands of Nothing Happens:  try again!"

"There has always been something mystical about mirrors."

"There is a Mastermind deep in the dungeon."

"There is a big treasure hidden in the zoo!"

"There is more magic in this cave than meets the eye."

"There is no harm in praising a large dog."

"There is nothing like eating a mimic."

"There once was a Knight named Lancelot who liked to ride with his lance a lot."

"They say a gelatinous cube can paralyze you..."

"They say that Juiblex is afraid of a wand of digging."

"They say that Medusa would like to put you on a pedestal."

"They say that Vlad lives!!! ... in the mazes."

"They say that `Elbereth' is often written about."

"They say that a bag of holding can't hold everything."

"They say that a blessed tin of quasit meat is a quick meal."

"They say that a cat avoids traps."

"They say that a cave spider will occasionally eat cave spider eggs."

"They say that a clever wizard can have stats:  18/** 24 18 24 24 24."

"They say that a clove of garlic makes a good talisman if handled right."

"They say that a cursed scroll of teleportation could land you in trouble."

"They say that a diamond is another kind of luck stone."

"They say that a dog can be trained to fetch objects."

"They say that a gelatinous cube makes a healthy breakfast."

"They say that a giant gets strong by eating right, try it!"

"They say that a grid bug won't hit you when you cross it."

"They say that a lembas wafer is a very light snack."

"They say that a loadstone has a strange attraction and is not bad luck."

"They say that a lock pick by any other name is still a lock pick."

"They say that a lucky amulet will block poisoned arrows."

"They say that a mirror will freeze a floating eye but you can still see it."

"They say that a neutral character might get Giantslayer."

"They say that a polymorph trap is magic and magic protection prevents it."

"They say that a potion of healing can cancel a potion of sickness."

"They say that a potion of monster detection sometimes works both ways."

"They say that a sink looks different from high above the floor."

"They say that a summoned demon could improve your game."

"They say that a tin of wraith meat is a rare dining experience."

"They say that a unicorn might bring you luck."

"They say that a wand of cancellation is like a wand of polymorph."

"They say that a wand of locking can close more than just doors."

"They say that a wand of polymorph can change your game."

"They say that a wizard is even more powerful the second time around."

"They say that a xorn knows of no obstacles when pursuing you."

"They say that abusing a credit card could shock you sooner or later."

"They say that amulets, like most things, can be deadly or life saving."

"They say that an altar can identify blessings."

"They say that an ooze will bite your boots and a rockmole will eat them."

"They say that an unlucky hacker was once killed by an exploding tin."

"They say that antique dealers are always interested in precious stones."

"They say that bandaging one's wounds helps to keep up one's appearance."

"They say that booze can be diluted but not cancelled."

"They say that by listening carefully, you can hear a secret door!"

"They say that carrots and carrot juice may improve your vision."

"They say that cave spiders are not considered expensive health food."

"They say that demigods must leave behind their prized earthly possessions."

"They say that disturbing a djinni can be a costly mistake."

"They say that dragon scales can be quite enchanting."

"They say that dropping coins into a fountain will not grant you a wish."

"They say that dwarves lawfully mind their own business."

"They say that eating a bat corpse will make you batty, for a while."

"They say that eating a cram ration is a smart move."

"They say that eating blue jelly is cool if you don't fight the feeling."

"They say that escaping a dungeon is only the beginning of the end."

"They say that feeling an unexpected draft of air is sort of a breakthrough."

"They say that finding a cursed gray stone is always bad luck."

"They say that gaining a level is an experience that can raise your sights."

"They say that garter snake meat rarely tastes good but it's still healthy."

"They say that gauntlets of dexterity have a hidden enchanted touch."

"They say that going to heaven is just another way of escaping the dungeon."

"They say that golden nagas are law-abiding denizens as long as you are too."

"They say that gremlins can make you feel cooler than you are now."

"They say that grid bugs only exist in a strictly Cartesian sense."

"They say that hackers often feel jumpy about eating nymphs."

"They say that having polymorph control won't shock you."

"They say that if it's hard getting your food down another bite could kill."

"They say that if you don't wear glasses why bother with carrots?"

"They say that if you notice a loose board beneath you, don't step on it."

"They say that if you start at the bottom the only place to go is up."

"They say that if you teleport to heaven you're presumed to be dead already."

"They say that in a shop you can be charged for old charges."

"They say that in lighter moments you could think of ways to pass a stone."

"They say that in the dungeon breaking a mirror can be seven years bad luck."

"They say that in the dungeon you don't usually have any luck at all."

"They say that in time a blessed luckstone can make your god happy."

"They say that it is easier to kill the Wizard than to make him stand still."

"They say that it only takes 1 zorkmid to meet the Kops."

"They say that it's a blast when you mix the right potions together."

"They say that it's not blind luck if you catch a glimpse of Medusa."

"They say that killing a shopkeeper brings bad luck."

"They say that monsters never step on a scare monster scroll."

"They say that most monsters find flute recitals extremely boring."

"They say that mummy corpses are not well preserved."

"They say that naturally a wand of wishing would be heavily guarded."

"They say that no one notices the junk underneath a boulder."

"They say that nobody expects a unicorn horn to rust."

"They say that nobody knows if an explorer can live forever.  Do you?"

"They say that nothing can change the fact that some potions contain a djinni."

"They say that nothing can change the fact that some potions contain a ghost."

"They say that nymphs always fall for rock'n'roll, try it!"

"They say that once an Olog-Hai is canned it never shows its face again."

"They say that once upon a time xans would never scratch your boots."

"They say that only an experienced wizard can do the tengu shuffle."

"They say that only chaotics can kill shopkeepers and get away with it."

"They say that only female monsters can lay eggs."

"They say that playing a horn really bad is really good."

"They say that rubbing a glowing potion does not make it a magic lamp."

"They say that scalpels become dull because they're not athames."

"They say that shopkeepers don't like pick-axes."

"They say that shopkeepers don't mind you bringing your pets in the shop."

"They say that shopkeepers don't usually mind if you sneak into a shop."

"They say that shopkeepers often have a large amount of money in their purses."

"They say that shopkeepers often remember things that you might forget."

"They say that sinks and armor don't mix, take your cloak off now!"

"They say that sinks run hot and cold and many flavors in between."

"They say that snake charmers aren't charismatic, just musical."

"They say that soldiers are always prepared and usually protected."

"They say that some eggs could hatch in your pack, lucky or not."

"They say that some fire ants will make you a hot meal."

"They say that some horns play hot music and others are too cool for words."

"They say that some humanoids are nonetheless quite human."

"They say that some shopkeepers consider gems to be family heirlooms."

"They say that some shopkeepers recognize gems but they won't tell you."

"They say that some stones are much much heavier than others."

"They say that some yetis are full of hot air."

"They say that something very special would be in a well-protected place."

"They say that speed boots aren't fast enough to let you walk on water."

"They say that teleport traps are the devil's work."

"They say that tengu don't wear rings, why should you?"

"They say that tengu never steal gold although they would be good at it."

"They say that that which was stolen once can be stolen again, ask any nymph."

"They say that the Delphic Oracle knows that lizard corpses aren't confusing."

"They say that the Hand of Elbereth can hold up your prayers."

"They say that the Leprechaun King is rich as Croesus."

"They say that the Wizard of Yendor is schizophrenic and suicidal."

"They say that the experienced character knows how to convert an altar."

"They say that the gods are happy when they drop objects at your feet."

"They say that the idea of invisible Nazguls has a certain ring to it."

"They say that the lady of the lake now lives in a fountain somewhere."

"They say that the local shopkeeper frowns upon the rude tourist."

"They say that the only door to the vampire's tower is on its lowest level."

"They say that the only good djinni is a grateful djinni."

"They say that the thing about genocide is that it works both ways."

"They say that the unicorn horn rule is if it ain't broke then don't fix it."

"They say that the view from a fog cloud is really very moving."

"They say that the walls in shops are made of extra hard material."

"They say that there are at least 15 ways to lose a pair of levitation boots."

"They say that throwing glass gems is the same as throwing rocks."

"They say that trespassing a boulder is probably beneath you."

"They say that unicorns are fond of precious gems."

"They say that prayer at an altar can sometimes make the water there holy."

"They say that what goes down the drain might come back up."

"They say that wielded, a long sword named Fire Brand makes you feel cooler."

"They say that wielded, a long sword named Frost Brand makes you hot stuff."

"They say that wiping its face is impossible for a floating eye."

"They say that with a floating eye you could see in the dark."

"They say that you are lucky if you can get a unicorn to catch a ruby."

"They say that you are what you eat."

"They say that you can find named weapons at an altar if you're lucky."

"They say that you can safely touch cockatrice eggs but why bother?"

"They say that you can't break an amulet of reflection."

"They say that you don't always get what you wish for."

"They say that you should always be prepared for a final challenge."

"They say that you should ask a dwarf to let you into a locked shop."

"They say that you should pray for divine inspiration."

"They say that you should religiously give your gold away."

"They say that you will never get healthy by eating geckos."

"They say that zapping yourself with a wand of undead turning is stupid."

"They say the Wizard's castle is booby-trapped!"

"They say the gods get angry if you kill your dog."

"They say the gods get angry if you pray too much."

"They say there is a powerful magic item hidden in a castle deep down!"

"Those who wield a cockatrice corpse have a rocky road ahead of them."

"Throwing food at a wild dog might tame him."

"To a full belly all food is bad."

"Trolls are described as rubbery:  they keep bouncing back."

"Try the fall-back end-run play against ghosts."

"Try using your magic marker on wet scrolls."

"Two wrongs don't make a right, but three lefts do."

"Valkyries come from the north, and have commensurate abilities."

"Vampires hate garlic."

"Vault guards never disturb their Lords."

"Vegetarians enjoy lichen and seaweed."

"Visitors are requested not to apply genocide to shopkeepers."

"Watch out, the Wizard might come back."

"Water traps have no effect on dragons."

"What is a cockatrice going to eat when it gets hungry?"

"Who needs an apron if they're made of glass?"

"Why do you suppose they call them MAGIC markers?"

"Why do you think they call them mercenaries?"

"Why would anybody in his sane mind engrave \"Elbereth\"?"

"Wishing too much may bring you too little."

"You can't bribe soldier ants."

"You can't leave a shop through the back door:  there isn't one!"

"You may discover a fine spirit inside a potion bottle."

"You may want to dip into a potion of bottled blessings."

"You might be able to bribe a demon lord."

"You might trick a shopkeeper if you're invisible."

"You should certainly learn about quantum mechanics."

"You're going into the morgue at midnight???"

"Your dog knows what to eat; maybe you should take lessons."

"Zap yourself and see what happens..."

"Zapping a wand of undead turning might bring your dog back to life."

"\"So when I die, the first thing I will see in heaven is a score list?\""

"1st Law of Hacking:  leaving is much more difficult than entering."

"2nd Law of Hacking:  first in, first out."

"3rd Law of Hacking:  the last blow counts most."

"4th Law of Hacking:  you will find the exit at the entrance."

"A chameleon imitating a mail daemon often delivers scrolls of fire."

"A cockatrice corpse is guaranteed to be untainted!"

"A dead cockatrice is just a dead lizard."

"A dragon is just a snake that ate a scroll of fire."

"A fading corridor enlightens your insight."

"A glowing potion is too hot to drink."

"A good amulet may protect you against guards."

"A lizard corpse is a good thing to turn undead."

"A long worm can be defined recursively.  So how should you attack it?"

"A monstrous mind is a toy forever."

"A nymph will be very pleased if you call her by her real name:  Lorelei."

"A ring of dungeon master control is a great find."

"A ring of extra ring finger is useless if not enchanted."

"A rope may form a trail in a maze."

"A staff may recharge if you drop it for awhile."

"A visit to the Zoo is very educational; you meet interesting animals."

"A wand of deaf is a more dangerous weapon than a wand of sheep."

"A wand of vibration might bring the whole cave crashing about your ears."

"A winner never quits.  A quitter never wins."

"A wish?  Okay, make me a fortune cookie!"

"Afraid of mimics?  Try to wear a ring of true seeing."

"All monsters are created evil, but some are more evil than others."

"Always attack a floating eye from behind!"

"An elven cloak is always the height of fashion."

"Any small object that is accidentally dropped will hide under a larger object."

"Archeologists find more bones piles."

"Austin Powers says: My Mojo is back!  Yeah, baby!"

"Balrogs do not appear above level 20."

"Banana peels work especially well against Keystone Kops."

"Be careful when eating bananas.  Monsters might slip on the peels."

"Better leave the dungeon; otherwise you might get hurt badly."

"Beware of the potion of nitroglycerin -- it's not for the weak of heart."

"Beware:  there's always a chance that your wand explodes as you try to zap it!"

"Beyond the 23rd level lies a happy retirement in a room of your own."

"Changing your suit without dropping your sword?  You must be kidding!"

"Close the door!  You're letting the heat out!"

"Cockatrices might turn themselves to stone faced with a mirror."

"Consumption of home-made food is strictly forbidden in this dungeon."

"Dark room?  Your chance to develop your photographs!"

"Dark rooms are not *completely* dark:  just wait and let your eyes adjust..."

"David London sez, \"Hey guys, *WIELD* a lizard corpse against a cockatrice!\""

"Death is just life's way of telling you you've been fired."

"Demi-gods don't need any help from the gods."

"Demons *HATE* Priests and Priestesses."

"Didn't you forget to pay?"

"Didn't your mother tell you not to eat food off the floor?"

"Direct a direct hit on your direct opponent, directing in the right direction."

"Do you want to make more money?  Sure, we all do!  Join the Fort Ludios guard!"

"Does your boss know what you're doing right now?"

"Don't bother wishing for things.  You'll probably find one on the next level."

"Don't eat too much:  you might start hiccoughing!"

"Don't play NetHack at your work; your boss might hit you!"

"Don't tell a soul you found a secret door, otherwise it isn't a secret anymore."

"Drinking potions of booze may land you in jail if you are under 21."

"Drop your vanity and get rid of your jewels!  Pickpockets about!"

"Eat 10 cloves of garlic and keep all humans at a two-square distance."

"Eels hide under mud.  Use a unicorn to clear the water and make them visible."

"Elf has extra speed."

"Engrave your wishes with a wand of wishing."

"Eventually you will come to admire the swift elegance of a retreating nymph."

"Ever heard hissing outside?  I *knew* you hadn't!"

"Ever lifted a dragon corpse?"

"Ever seen a leocrotta dancing the tengu?"

"Ever seen your weapon glow plaid?"

"Ever tamed a shopkeeper?"

"Ever tried digging through a Vault Guard?"

"Ever tried enchanting a rope?"

"Floating eyes can't stand Hawaiian shirts."

"For any remedy there is a misery."

"Giant bats turn into giant vampires."

"Good day for overcoming obstacles.  Try a steeplechase."

"Half Moon tonight.  (At least it's better than no Moon at all.)"

"Help!  I'm being held prisoner in a fortune cookie factory!"

"Housecats have nine lives, kittens only one."

"How long can you tread water?"

"Hungry?  There is an abundance of food on the next level."

"I guess you've never hit a mail daemon with the Amulet of Yendor..."

"If you are the shopkeeper, you can take things for free."

"If you ask really nicely, the Wizard will give you the Amulet."

"If you can't learn to do it well, learn to enjoy doing it badly."

"If you thought the Wizard was bad, just wait till you meet the Warlord!"

"If you turn blind, don't expect your dog to be turned into a seeing-eye dog."

"If you want to feel great, you must eat something real big."

"If you want to float, you'd better eat a floating eye."

"If your ghost kills a player, it increases your score."

"Increase mindpower:  Tame your own ghost!"

"It furthers one to see the great man."

"It's easy to overlook a monster in a wood."

"Just below any trap door there may be another one.  Just keep falling!"

"Katanas are very sharp; watch you don't cut yourself."

"Keep a clear mind:  quaff clear potions."

"Kicking the terminal doesn't hurt the monsters."

"Killer bees keep appearing till you kill their queen."

"Killer bunnies can be tamed with carrots only."

"Latest news?  Put `rec.games.roguelike.nethack' in your .newsrc!"

"Learn how to spell.  Play NetHack!"

"Leprechauns hide their gold in a secret room."

"Let your fingers do the walking on the yulkjhnb keys."

"Let's face it:  this time you're not going to win."

"Let's have a party, drink a lot of booze."

"Liquor sellers do not drink; they hate to see you twice."

"Lunar eclipse tonight.  May as well quit now!"

"Meeting your own ghost decreases your luck considerably!"

"Money to invest?  Take it to the local branch of the Magic Memory Vault!"

"Monsters come from nowhere to hit you everywhere."

"Monsters sleep because you are boring, not because they ever get tired."

"Most monsters prefer minced meat.  That's why they are hitting you!"

"Most of the bugs in NetHack are on the floor."

"Much ado Nothing Happens."

"Multi-player NetHack is a myth."

"NetHack is addictive.  Too late, you're already hooked."

"Never ask a shopkeeper for a price list."

"Never burn a tree, unless you like getting whacked with a +5 shovel."

"Never eat with glowing hands!"

"Never mind the monsters hitting you:  they just replace the charwomen."

"Never play leapfrog with a unicorn."

"Never step on a cursed engraving."

"Never swim with a camera:  there's nothing to take pictures of."

"Never teach your pet rust monster to fetch."

"Never trust a random generator in magic fields."

"Never use a wand of death."

"No level contains two shops.  The maze is no level.  So..."

"No part of this fortune may be reproduced, stored in a retrieval system, ..."

"Not all rumors are as misleading as this one."

"Nymphs and nurses like beautiful rings."

"Nymphs are blondes.  Are you a gentleman?"

"Offering a unicorn a worthless piece of glass might prove to be fatal!"

"Old hackers never die:  young ones do."

"One has to leave shops before closing time."

"One homunculus a day keeps the doctor away."

"One level further down somebody is getting killed, right now."

"Only a wizard can use a magic whistle."

"Only adventurers of evil alignment think of killing their dog."

"Only chaotic evils kill sleeping monsters."

"Only real trappers escape traps."

"Only real wizards can write scrolls."

"Operation OVERKILL has started now."

"Ouch.  I hate when that happens."

"PLEASE ignore previous rumor."

"Polymorph into an ettin; meet your opponents face to face to face."

"Praying will frighten demons."

"Row (3x) that boat gently down the stream, Charon (4x), death is but a dream."

"Running is good for your legs."

"Screw up your courage!  You've screwed up everything else."

"Seepage?  Leaky pipes?  Rising damp?  Summon the plumber!"

"Segmentation fault (core dumped)."

"Shopkeepers are insured by Croesus himself!"

"Shopkeepers sometimes die from old age."

"Some mazes (especially small ones) have no solutions, says man 6 maze."

"Some questions the Sphynx asks just *don't* have any answers."

"Sometimes \"mu\" is the answer."

"Sorry, no fortune this time.  Better luck next cookie!"

"Spare your scrolls of make-edible until it's really necessary!"

"Stormbringer doesn't steal souls.  People steal souls."

"Suddenly, the dungeon will collapse..."

"Taming a mail daemon may cause a system security violation."

"The crowd was so tough, the Stooges won't play the Dungeon anymore, nyuk nyuk."

"The leprechauns hide their treasure in a small hidden room."

"The longer the wand the better."

"The magic word is \"XYZZY\"."

"The meek shall inherit your bones files."

"The mines are dark and deep, and I have levels to go before I sleep."

"The use of dynamite is dangerous."

"There are no worms in the UNIX version."

"There is a trap on this level!"

"They say that Demogorgon, Asmodeus, Orcus, Yeenoghu & Juiblex is no law firm."

"They say that Geryon has an evil twin, beware!"

"They say that Medusa would make a terrible pet."

"They say that NetHack bugs are Seldon planned."

"They say that NetHack comes in 256 flavors."

"They say that NetHack is just a computer game."

"They say that NetHack is more than just a computer game."

"They say that NetHack is never what it used to be."

"They say that a baby dragon is too small to hurt or help you."

"They say that a black pudding is simply a brown pudding gone bad."

"They say that a black sheep has 3 bags full of wool."

"They say that a blank scroll is like a blank check."

"They say that a cat named Morris has nine lives."

"They say that a desperate shopper might pay any price in a shop."

"They say that a diamond dog is everybody's best friend."

"They say that a dwarf lord can carry a pick-axe because his armor is light."

"They say that a floating eye can defeat Medusa."

"They say that a fortune only has 1 line and you can't read between it."

"They say that a fortune only has 1 line, but you can read between it."

"They say that a fountain looks nothing like a regularly erupting geyser."

"They say that a gold doubloon is worth more than its weight in gold."

"They say that a grid bug won't pay a shopkeeper for zapping you in a shop."

"They say that a gypsy could tell your fortune for a price."

"They say that a hacker named Alice once level teleported by using a mirror."

"They say that a hacker named David once slew a giant with a sling and a rock."

"They say that a hacker named Dorothy once rode a fog cloud to Oz."

"They say that a hacker named Mary once lost a white sheep in the mazes."

"They say that a helm of brilliance is not to be taken lightly."

"They say that a hot dog and a hell hound are the same thing."

"They say that a lamp named Aladdin's Lamp contains a djinni with 3 wishes."

"They say that a large dog named Lassie will lead you to the amulet."

"They say that a long sword is not a light sword."

"They say that a manes won't mince words with you."

"They say that a mind is a terrible thing to waste."

"They say that a plain nymph will only wear a wire ring in one ear."

"They say that a plumed hat could be a previously used crested helmet."

"They say that a potion of oil is difficult to grasp."

"They say that a potion of yogurt is a cancelled potion of sickness."

"They say that a purple worm is not a baby purple dragon."

"They say that a quivering blob tastes different than a gelatinous cube."

"They say that a runed broadsword named Stormbringer attracts vortices."

"They say that a scroll of summoning has other names."

"They say that a shaman can bestow blessings but usually doesn't."

"They say that a shaman will bless you for an eye of newt and wing of bat."

"They say that a shimmering gold shield is not a polished silver shield."

"They say that a spear will hit a neo-otyugh.  (Do YOU know what that is?)"

"They say that a spotted dragon is the ultimate shape changer."

"They say that a stethoscope is no good if you can only hear your heartbeat."

"They say that a succubus named Suzy will sometimes warn you of danger."

"They say that a wand of cancellation is not like a wand of polymorph."

"They say that a wood golem named Pinocchio would be easy to control."

"They say that after killing a dragon it's time for a change of scenery."

"They say that an amulet of strangulation is worse than ring around the collar."

"They say that an attic is the best place to hide your toys."

"They say that an axe named Cleaver once belonged to a hacker named Beaver."

"They say that an eye of newt and a wing of bat are double the trouble."

"They say that an incubus named Izzy sometimes makes women feel sensitive."

"They say that an opulent throne room is rarely a place to wish you'd be in."

"They say that an unlucky hacker once had a nose bleed at an altar and died."

"They say that and they say this but they never say never, never!"

"They say that any quantum mechanic knows that speed kills."

"They say that applying a unicorn horn means you've missed the point."

"They say that blue stones are radioactive, beware."

"They say that building a dungeon is a team effort."

"They say that chaotic characters never get a kick out of altars."

"They say that collapsing a dungeon often creates a panic."

"They say that counting your eggs before they hatch shows that you care."

"They say that dipping a bag of tricks in a fountain won't make it an icebox."

"They say that dipping an eel and brown mold in hot water makes bouillabaisse."

"They say that donating a doubloon is extremely pious charity."

"They say that dungeoneers prefer dark chocolate."

"They say that eating royal jelly attracts grizzly owlbears."

"They say that eggs, pancakes and juice are just a mundane breakfast."

"They say that everyone knows why Medusa stands alone in the dark."

"They say that everyone wanted rec.games.hack to undergo a name change."

"They say that finding a winning strategy is a deliberate move on your part."

"They say that finding worthless glass is worth something."

"They say that fortune cookies are food for thought."

"They say that gold is only wasted on a pet dragon."

"They say that good things come to those that wait."

"They say that greased objects will slip out of monsters' hands."

"They say that if you can't spell then you'll wish you had a spellbook."

"They say that if you live by the sword, you'll die by the sword."

"They say that if you play like a monster you'll have a better game."

"They say that if you sleep with a demon you might awake with a headache."

"They say that if you step on a crack you could break your mother's back."

"They say that if you're invisible you can still be heard!"

"They say that if you're lucky you can feel the runes on a scroll."

"They say that in the big picture gold is only small change."

"They say that in the dungeon it's not what you know that really matters."

"They say that in the dungeon moon rocks are really dilithium crystals."

"They say that in the dungeon the boorish customer is never right."

"They say that in the dungeon you don't need a watch to tell time."

"They say that in the dungeon you need something old, new, burrowed and blue."

"They say that in the dungeon you should always count your blessings."

"They say that iron golem plate mail isn't worth wishing for."

"They say that it takes four quarterstaffs to make one staff."

"They say that it's not over till the fat ladies sing."

"They say that it's not over till the fat lady shouts `Off with its head'."

"They say that kicking a heavy statue is really a dumb move."

"They say that kicking a valuable gem doesn't seem to make sense."

"They say that leprechauns know Latin and you should too."

"They say that minotaurs get lost outside of the mazes."

"They say that most trolls are born again."

"They say that naming your cat Garfield will make you more attractive."

"They say that no one knows everything about everything in the dungeon."

"They say that no one plays NetHack just for the fun of it."

"They say that no one really subscribes to rec.games.roguelike.nethack."

"They say that no one will admit to starting a rumor."

"They say that nurses sometimes carry scalpels and never use them."

"They say that once you've met one wizard you've met them all."

"They say that one troll is worth 10,000 newts."

"They say that only David can find the zoo!"

"They say that only angels play their harps for their pets."

"They say that only big spenders carry gold."

"They say that orc shamans are healthy, wealthy and wise."

"They say that playing NetHack is like walking into a death trap."

"They say that problem breathing is best treated by a proper diet."

"They say that quaffing many potions of levitation can give you a headache."

"They say that queen bees get that way by eating royal jelly."

"They say that reading a scare monster scroll is the same as saying Elbereth."

"They say that real hackers always are controlled."

"They say that real hackers never sleep."

"They say that shopkeepers are insured by Croesus himself!"

"They say that shopkeepers never carry more than 20 gold pieces, at night."

"They say that shopkeepers never sell blessed potions of invisibility."

"They say that soldiers wear kid gloves and silly helmets."

"They say that some Kops are on the take."

"They say that some guards' palms can be greased."

"They say that some monsters may kiss your boots to stop your drum playing."

"They say that sometimes you can be the hit of the party when playing a horn."

"They say that the NetHack gods generally welcome your sacrifices."

"They say that the Three Rings are named Vilya, Nenya and Narya."

"They say that the Wizard of Yendor has a death wish."

"They say that the `hair of the dog' is sometimes an effective remedy."

"They say that the best time to save your game is now before it's too late."

"They say that the biggest obstacle in NetHack is your mind."

"They say that the gods are angry when they hit you with objects."

"They say that the priesthood are specially favored by the gods."

"They say that the way to make a unicorn happy is to give it what it wants."

"They say that there are no black or white stones, only gray."

"They say that there are no skeletons hence there are no skeleton keys."

"They say that there is a clever rogue in every hacker just dying to escape."

"They say that there is no such thing as free advice."

"They say that there is only one way to win at NetHack."

"They say that there once was a fearsome chaotic samurai named Luk No."

"They say that there was a time when cursed holy water wasn't water."

"They say that there's no point in crying over a gray ooze."

"They say that there's only hope left after you've opened Pandora's box."

"They say that trap doors should always be marked `Caution:  Trap Door'."

"They say that using an amulet of change isn't a difficult operation."

"They say that water walking boots are better if you are fast like Hermes."

"They say that when you wear a circular amulet you might resemble a troll."

"They say that when you're hungry you can get a pizza in 30 moves or it's free."

"They say that when your god is angry you should try another one."

"They say that wielding a unicorn horn takes strength."

"They say that with speed boots you never worry about hit and run accidents."

"They say that you can defeat a killer bee with a unicorn horn."

"They say that you can only cross the River Styx in Charon's boat."

"They say that you can only kill a lich once and then you'd better be careful."

"They say that you can only wish for things you've already had."

"They say that you can train a cat by talking gently to it."

"They say that you can train a dog by talking firmly to it."

"They say that you can trust your gold with the king."

"They say that you can't wipe your greasy bare hands on a blank scroll."

"They say that you cannot trust scrolls of rumor."

"They say that you could fall head over heels for an energy vortex."

"They say that you need a key in order to open locked doors."

"They say that you need a mirror to notice a mimic in an antique shop."

"They say that you really can use a pick-axe unless you really can't."

"They say that you should always store your tools in the cellar."

"They say that you should be careful while climbing the ladder to success."

"They say that you should call your armor `rustproof'."

"They say that you should name your dog Spuds to have a cool pet."

"They say that you should name your weapon after your first monster kill."

"They say that you should never introduce a rope golem to a succubus."

"They say that you should never sleep near invisible ring wraiths."

"They say that you should never try to leave the dungeon with a bag of gems."

"They say that you should remove your armor before sitting on a throne."

"This fortune cookie is copy protected."

"This fortune cookie is the property of Fortune Cookies, Inc."

"This release contains 10% recycled material."

"Time stands still as the succubus changes her calendar to January 1, 2000."

"Tired?  Try a scroll of charging on yourself."

"To achieve the next higher rating, you need 3 more points."

"To reach heaven, escape the dungeon while wearing a ring of levitation."

"Tourists wear shirts loud enough to wake the dead."

"Try calling your katana Moulinette."

"Ulch!  That meat was painted!"

"Unfortunately, this message was left intentionally blank."

"Using a morning star in the evening has no effect."

"Waltz, dumb nymph, for quick jigs vex."

"Want a hint?  Zap a wand of make invisible on your weapon!"

"Want to ascend in a hurry?  Apply at Gizmonic Institute."

"Wanted: shopkeepers.  Send a scroll of mail to Mage of Yendor/Level 35/Dungeon."

"Warning:  fortune reading can be hazardous to your health."

"We have new ways of detecting treachery..."

"Wet towels make great weapons!"

"What a pity, you cannot read it!"

"Whatever can go wrong, will go wrong."

"When a piercer drops in on you, you will be tempted to hit the ceiling!"

"When in a maze follow the right wall and you will never get lost."

"When you have a key, you don't have to wait for the guard."

"Why are you wasting time reading fortunes?"

"Wish for a master key and open the Magic Memory Vault!"

"Wizard expects every monster to do its duty."

"Wow!  You could've had a potion of fruit juice!"

"Yet Another Silly Message (YASM)."

"You are destined to be misled by a fortune."

"You can get a genuine Amulet of Yendor by doing the following:  --More--"

"You can make holy water by boiling the hell out of it."

"You can protect yourself from black dragons by doing the following:  --More--"

"You can't get by the snake."

"You choke on the fortune cookie.  --More--"

"You feel like someone is pulling your leg."

"You have to outwit the Sphynx or pay her."

"You hear the fortune cookie's hissing!"

"You may get rich selling letters, but beware of being blackmailed!"
"You offend Shai-Hulud by sheathing your crysknife without having drawn blood."
"You swallowed the fortune!"
"You want to regain strength?  Two levels ahead is a guesthouse!"
"You will encounter a tall, dark, and gruesome creature...")
  "Prints a random nethack fact")

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


