(deftemplate convertion (slot arabic)(slot roman))

(deffacts convertions
    (convertion (arabic 1)(roman I))
    (convertion (arabic 5)(roman V))
    (convertion (arabic 10)(roman X))
    (convertion (arabic 50)(roman L))
    (convertion (arabic 100)(roman C))
    (convertion (arabic 500)(roman D))
    (convertion (arabic 1000)(roman M))
    (convertion (arabic 5000)(roman v))
    (convertion (arabic 10000)(roman x))
    (convertion (arabic 50000)(roman l))
    (convertion (arabic 100000)(roman c))
    (convertion (arabic 500000)(roman d))
    (convertion (arabic 1000000)(roman m))
)
(defrule input-number
    (not (number $?))
    =>
    (printout t "Enter a Arabic number (-1 to end):")
    (assert (number (read)))
    (assert (digit 1000000))
)
(defrule end
    (declare (salience 10))
    (number -1)
    =>
    (halt)
)
(defrule process-fin
    ?f1 <- (number ?number)
    (test (< ?number 1))
    =>
    (retract ?f1)
    (printout t crlf)
)
(defrule process-0
    (declare (salience -1))
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 0))
    =>
    (retract ?f1 ?f2)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)
(defrule process-1
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 1))
    (convertion (arabic ?digit)(roman ?roman))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)
(defrule process-2
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 2))
    (convertion (arabic ?digit)(roman ?roman))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman)
    (printout t ?roman)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)
(defrule process-3
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 3))
    (convertion (arabic ?digit)(roman ?roman))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman)
    (printout t ?roman)
    (printout t ?roman)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)
(defrule process-4
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 4))
    (convertion (arabic ?digit)(roman ?roman_left))
    (convertion (arabic =(* ?digit 5))(roman ?roman_right))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman_left ?roman_right)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)

(defrule process-5
    (declare (salience 1))
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 5))
    (convertion (arabic =(* ?digit 5))(roman ?roman))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)

(defrule process-6
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 6))
    (convertion (arabic ?digit)(roman ?roman_right))
    (convertion (arabic =(* ?digit 5))(roman ?roman_left))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman_left ?roman_right)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)

(defrule process-7
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 7))
    (convertion (arabic ?digit)(roman ?roman_right))
    (convertion (arabic =(* ?digit 5))(roman ?roman_left))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman_left ?roman_right ?roman_right)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)


(defrule process-8
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 8))
    (convertion (arabic ?digit)(roman ?roman_right))
    (convertion (arabic =(* ?digit 5))(roman ?roman_left))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman_left ?roman_right ?roman_right ?roman_right)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)

(defrule process-9
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 9))
    (convertion (arabic ?digit)(roman ?roman_left))
    (convertion (arabic =(* ?digit 10))(roman ?roman_right))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman_left ?roman_right)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)














