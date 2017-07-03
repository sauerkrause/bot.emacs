;; -*- lexical-binding: t -*-

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
