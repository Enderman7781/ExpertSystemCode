(deftemplate Level
	1 7
	(
		(Good (1 1)(5 0))
		(Average (1 0)(4 1)(7 0))
	)	
)

(deftemplate Price
	0 200 ten-thousand-NTD
	(
		(Average (25 1)(40 0.5)(60 0))
		(Low (20 1)(25 0.6)(40 0))
	)	
)


(deftemplate Years
	0 100 Years
	(
		(Few (3 1)(10 0))
	)	
)


(deftemplate Space
	0 200 Ping
	(
		(Moderate (20 0)(30 1)(40 0))
	)	
)


(deftemplate location
	(slot ID)
	(slot evaluation (type FUZZY-VALUE Level))
)

(deftemplate house
	(slot ID)
	(slot location-ID)
	(slot unit-price (type FUZZY-VALUE Price))
	(slot house-age (type FUZZY-VALUE Years))
	(slot Ping (type FUZZY-VALUE Space))
)

(deffacts initial
   ;(total 0)
   (location (ID A) (evaluation (2 0) (2 1) (2 0)))
   (location (ID B) (evaluation (4 0) (4 1) (4 0)))
   (location (ID C) (evaluation (6 0) (6 1) (6 0)))
   (house (ID 1) (location-ID A) (unit-price (31 0) (31 1) (31 0)) (house-age (5 0) (5 1) (5 0)) (Ping (32 0) (32 1) (32 0)))
   (house (ID 2) (location-ID B) (unit-price (37 0) (37 1) (37 0)) (house-age (7 0) (7 1) (7 0)) (Ping (24 0) (24 1) (24 0)))
   (house (ID 3) (location-ID B) (unit-price (49 0) (49 1) (49 0)) (house-age (2 0) (2 1) (2 0)) (Ping (30 0) (30 1) (30 0)))
   (house (ID 4) (location-ID B) (unit-price (25 0) (25 1) (25 0)) (house-age (5 0) (5 1) (5 0)) (Ping (27 0) (27 1) (27 0)))
   (house (ID 5) (location-ID B) (unit-price (30 0) (30 1) (30 0)) (house-age (1 0) (1 1) (1 0)) (Ping (34 0) (34 1) (34 0)))
   (house (ID 6) (location-ID C) (unit-price (21 0) (21 1) (21 0)) (house-age (4 0) (4 1) (4 0)) (Ping (30 0) (30 1) (30 0)))
   (house (ID 7) (location-ID C) (unit-price (28 0) (28 1) (28 0)) (house-age (3 0) (3 1) (3 0)) (Ping (35 0) (35 1) (35 0)))
   (house (ID 8) (location-ID C) (unit-price (25 0) (25 1) (25 0)) (house-age (7 0) (7 1) (7 0)) (Ping (36 0) (36 1) (36 0)))
   (house (ID 9) (location-ID C) (unit-price (22 0) (22 1) (22 0)) (house-age (6 0) (6 1) (6 0)) (Ping (26 0) (26 1) (26 0)))
)

(defrule start
	(declare (salience 100))
	=>
	(assert (total 0))
)

(defrule good-house
	?f <- (house (ID ?id)(location-ID ?lid)(unit-price Average)(house-age Few)(Ping Moderate))
	(location (ID ?lid)(evaluation Good))
	?f2 <- (total ?old)
	=>
	(retract ?f ?f2)
	(assert (total (+ ?old 1)))
	(printout t "Recommend House " ?id crlf)
)

(defrule avg-house
	?f <- (house (ID ?id)(location-ID ?lid)(unit-price somewhat Low)(house-age Few)(Ping Moderate))
	(location (ID ?lid)(evaluation Average))
	?f2 <- (total ?old)
	=>
	(retract ?f ?f2)
	(assert (total (+ ?old 1)))
	(printout t "Recommend House " ?id crlf)
)

(defrule not-good-house
	?f <- (house (ID ?id)(location-ID ?lid)(unit-price very Low)(house-age Few)(Ping Moderate))
	(location (ID ?lid)(evaluation not Good))
	?f2 <- (total ?old)
	=>
	(retract ?f ?f2)
	(assert (total (+ ?old 1)))
	(printout t "Recommend House " ?id crlf)
)

(defrule finish
	(declare (salience -100))
	(total ?total)
	=>
	(printout t "Total " ?total crlf)
	(halt)
)
