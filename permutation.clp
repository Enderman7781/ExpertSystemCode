(deftemplate permutaion (multislot values)(multislot rest))

(deffacts initial (total 0))

(defrule read-base-fact
    (declare (salience 100))
    =>
    (printout t "Please input a base fact for the permutaion...")
    (bind ?input (explode$ (readline)))
    (assert (permutaion (values)(rest ?input)))
)


(defrule move-empty
    ?permutaion <- (permutaion (values $?middle)(rest $?front ?second $?rear))
    =>
    (assert (permutaion(values $?middle ?second)(rest $?front $?rear)))
)

(defrule print-permutaion
    
    ?permutaion <- (permutaion (values $?seq)(rest))
    ?old_total <- (total ?total)
    =>
    (retract ?permutaion ?old_total)
    (assert (total(+ ?total 1)))
    (printout t "Permutaion is " $?seq crlf)
)

(defrule print-total
    (declare (salience -1))
    ;(not (permutaion (values $?seq)(rest)))
    (total ?total)
    =>
    (printout t "Total is "?total crlf)
)





