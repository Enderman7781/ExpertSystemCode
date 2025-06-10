(deftemplate data (multislot numbers))

(defrule read-data
    (declare (salience 100))
    =>
    (printout t "Data sorting: ")
    (bind ?input(explode$ (readline)))
    (assert (data (numbers ?input)))
)

(defrule bigger-than
    ?f1 <- (data (numbers $?front ?first ?second $?rest))
    (test(> ?first ?second))
    =>
    (retract ?f1)
    (assert (data (numbers $?front ?second $?first $?rest)))
)

(defrule print-res
    (declare (salience -1))
    ?fin <- (data (numbers $?seq))
    =>
    (printout t "Res is " $?seq crlf)
)