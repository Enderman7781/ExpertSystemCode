(deftemplate team (slot id)(multislot choice))
(deftemplate advisor (slot id)(multislot choice))
(deftemplate match (slot team)(slot advisor))

(deffacts initial
  (team (id t1)(choice a5 a2 a9 a4 a10 a8 a3 a6 a7 a1))
  (team (id t2)(choice a5 a10 a8 a4 a9 a3 a2 a6 a1 a7))
  (team (id t3)(choice a6 a2 a9 a8 a10 a3 a1 a7 a5 a4))
  (team (id t4)(choice a10 a2 a6 a8 a7 a9 a3 a1 a4 a5))
  (team (id t5)(choice a6 a3 a9 a2 a10 a8 a4 a5 a7 a1))
  (team (id t6)(choice a5 a2 a7 a9 a8 a4 a1 a6 a3 a10))
  (team (id t7)(choice a10 a2 a7 a4 a5 a8 a3 a6 a9 a1))
  (team (id t8)(choice a7 a3 a9 a8 a10 a2 a1 a6 a5 a4))
  (team (id t9)(choice a3 a2 a6 a8 a7 a9 a10 a1 a4 a5))
  (team (id t10)(choice a6 a2 a9 a8 a10 a3 a4 a5 a7 a1))
  (advisor (id a1)(choice t1 t2 t7 t6 t3 t8 t10 t9 t4 t5))
  (advisor (id a2)(choice t2 t7 t3 t6 t10 t9 t1 t5 t4 t8))
  (advisor (id a3)(choice t8 t5 t1 t9 t10 t7 t3 t2 t4 t6))
  (advisor (id a4)(choice t7 t10 t3 t6 t2 t8 t9 t4 t5 t1))
  (advisor (id a5)(choice t7 t2 t1 t6 t10 t8 t3 t5 t4 t9))
  (advisor (id a6)(choice t5 t3 t1 t4 t10 t8 t2 t7 t9 t6))
  (advisor (id a7)(choice t7 t2 t1 t5 t8 t10 t3 t6 t9 t4))
  (advisor (id a8)(choice t1 t2 t6 t10 t7 t8 t3 t9 t4 t5))
  (advisor (id a9)(choice t6 t4 t1 t10 t3 t8 t7 t5 t2 t9))
  (advisor (id a10)(choice t2 t9 t4 t5 t10 t8 t1 t6 t3 t7))
  (current-level 1)
  (last-level 10)
)

    ;(current-level ?c)

    ;(team (id ?ta) (choice $?ta_count ?a $?))
    ;(test (eq ?c (+ (length$ $?ta_count) 1)))
    ;(not (match (team ?ta) (advisor ?)))
    ;(not (match (team ?) (advisor ?a)))

    ;(not 
    ;    (and 
    ;        (team (id ?tb) (choice $?tb_count ?a $?))
    ;        (advisor (id ?a)(choice $? ?tb $? ?ta $?))
    ;        (test (eq ?c (+ (length$ $?tb_count) 1)))
    ;        (not (match (team ?tb) (advisor ?)))
    ;    )
    ;;;)
    ;=>
    ;(assert (match (team ?ta) (advisor ?a)))

    
(defrule team-match-advisor
    (current-level ?c)
    (advisor (id ?pid)(choice $? ?t $? ?id2 $?))
    (team (id ?t)(choice $?ch1))
    (team (id ?id2)(choice $?ch2))
    (test (neq (nth$ (integer ?c) $?ch1)(nth$ (integer ?c) $?ch2)))
    (test (eq ?pid (nth$ (integer ?c) $?ch1)))
    =>
    (retract ?f1)
    (retract ?f3)
    (printout t ?t " " ?id2 " " ?c " " ?pid crlf)
    (assert (match (team ?t)(advisor ?pid)))
)

(defrule change-level
    (declare (salience -10))
    ?f1 <- (current-level ?c)
    =>
    (retract ?f1)
    (assert (current-level (+ ?c 1)))
)

(defrule level-finish
    (declare (salience 10))
    ?f1 <- (current-level ?c)
    ?f2 <- (last-level ?l)
    (test(> ?c ?l))
    =>
    (retract ?f1 ?f2)
)

(defrule print
    (declare (salience -20))
    (match (team ?t)(advisor ?a))
    =>
    (printout t "team " ?t " match advisor " ?a crlf)
)