(deftemplate Level
	1 7
	(
		(High (3 0)(5 0.9)(6 1))
	)
)
(deftemplate Price
	0 200 ten-thounsand-NTD
	(
		(Low (40 1)(60 0.8)(100 0))
	)
)
(deftemplate Years
	0 40 years
	(
		(Few (3 1)(5 0.9)(10 0))
	)
)
(deftemplate Capacity
	0 2000 liter
	(
		(Large (300 0)(600 1))
	)
)
(deftemplate Horsepower
	0 500 HP
	(
		(High (120 0)(180 0.8)(200 1))
	)
)

(deftemplate car
	(slot ID)
	(slot brand)
	(slot model)
	(slot price (type FUZZY-VALUE Price))
	(slot car-age (type FUZZY-VALUE Years))
	(slot trunk (type FUZZY-VALUE Capacity))
	(slot horsepower (type FUZZY-VALUE Horsepower))
)

(deftemplate car-brand
	(slot brand)
	(slot level (type FUZZY-VALUE Level))
)
(deftemplate degree
	(slot ID)
	(slot total)
)






(deffacts initial
    (car (ID 1) (brand Porsche) (model 718) (price (122 0) (122 1) (122 0)) (car-age (9 0) (9 1) (9 0)) 
           (trunk (150 0) (150 1) (150 0)) (horsepower (300 0) (300 1) (300 0)))
    (car (ID 2) (brand BMW) (model 320i) (price (87 0) (87 1) (87 0)) (car-age (8 0) (8 1) (8 0)) 
           (trunk (480 0) (480 1) (480 0)) (horsepower (184 0) (184 1) (184 0)))
    (car (ID 3) (brand Lexus) (model is200) (price (91 0) (91 1) (91 0)) (car-age (5 0) (5 1) (5 0)) 
           (trunk (450 0) (450 1) (450 0)) (horsepower (181 0) (181 1) (181 0)))
    (car (ID 4) (brand Toyota) (model rav4) (price (79 0) (79 1) (79 0)) (car-age (4 0) (4 1) (4 0)) 
           (trunk (733 0) (733 1) (733 0)) (horsepower (173 0) (173 1) (173 0)))
    (car (ID 5) (brand Hyundai) (model Elantra) (price (48 0) (48 1) (48 0)) (car-age (3 0) (3 1) (3 0)) 
           (trunk (458 0) (458 1) (458 0)) (horsepower (128 0) (128 1) (128 0)))
    (car-brand (brand Ferrari) (level (7 0) (7 1) (7 0)))
    (car-brand (brand Porsche) (level (5 0) (6 1) (7 0)))
    (car-brand (brand BMW) (level (4 0) (5 1) (6 1) (7 0)))
    (car-brand (brand Lexus) (level (4 0) (5 1) (6 0)))
    (car-brand (brand Toyota) (level (3 0) (4 1) (5 0)))
    (car-brand (brand Hyundai) (level (2 0) (3 1) (4 1) (5 0)))
    (car-brand (brand Tata) (level (1 0) (1 1) (1 0)))
)

(defrule match-price
	(declare (CF 0.7))
	(car (ID ?id)(price Low))
	=>
	(assert (low-price ?id))
)

(defrule match-brand
	(declare (CF 1.0))
	(car (ID ?id)(brand ?b))
	(car-brand (brand ?b) (level High))
	=>
	(assert (fame-brand ?id))
)

(defrule match-age
	(declare (CF 0.8))
	(car (ID ?id)(car-age Few))
	=>
	(assert (few-years ?id))
)

(defrule match-trunk
	(declare (CF 0.2))
	(car (ID ?id)(trunk Large))
	=>
	(assert (large-trunk ?id))
)

(defrule match-hp
	(declare (CF 0.5))
	(car (ID ?id)(horsepower High))
	=>
	(assert (high-hp ?id))
)

(defrule get-cf
	(declare (salience -10))
	?f1 <- (low-price ?id)
	?f2 <- (fame-brand ?id)
	?f3 <- (few-years ?id)
	?f4 <- (large-trunk ?id)
	?f5 <- (high-hp ?id)
	=>
	(assert (degree (ID ?id)(total (+(+(+(+ (get-cf ?f1)(get-cf ?f2))
										(get-cf ?f3))
										(get-cf ?f4))
										(get-cf ?f5) )
										)))	
)


(defrule print
	(declare (salience -100))
	?p <- (degree (ID ?id)(total ?t))
	=>
	(retract ?p)
	(printout t "Car: " ?id " Degree: " (/ ?t 3.2) crlf)

)