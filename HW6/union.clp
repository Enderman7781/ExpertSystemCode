(deftemplate set (multislot name)(multislot members))

(deffacts initial (phase input))

(defrule input-number
    (phase input)
    =>
    (printout t "Please input a set s1:")
    (assert (set (name s1) (members (explode$ (readline)))))
    (printout t "Please input a set s2:")
    (assert (set (name s2) (members (explode$ (readline)))))
)
;檢查重複
(defrule check-input-error-1
    ?f1 <- (phase input)
    ?f2 <- (set (name ?t1)(members $? ?a $? ?a $?))
    ?f3 <- (set (name ?t2&~?t1)(members $?))
    =>
    (retract ?f1 ?f2 ?f3)
    (assert (phase input))
    (printout t "Input Error: Duplicate elements not allowed" crlf)
)
;檢查非數字
(defrule check-input-error-2
    ?f1 <- (phase input)
    ?f2 <- (set (name ?t1)(members $? ?a $?))
    ?f3 <- (set (name ?t2&~?t1)(members $?))
    (test (not (integerp ?a)))
    =>
    (retract ?f1 ?f2 ?f3)
    (assert (phase input))
    (printout t "Input Error: Input must all Integers" crlf)
)
;
(defrule input-no-error
    (declare (salience -1))
    ?f <- (phase input)
    =>
    (retract ?f)
    (assert (phase compute))
    (assert (set (name intersect)(members)))
    (assert (set (name union)(members)))
)

;排序
(defrule bigger-than
    (declare (salience -2))
    (phase compute)
    ?f1 <- (set (name ?t1)(members $?front ?first ?second $?rest))
    (test(> ?first ?second))
    =>
    (retract ?f1)
    (assert (set (name ?t1)(members $?front ?second $?first $?rest)))
)

;交集
(defrule intersect
    (phase compute)
    (set (name s1)(members $? ?i $?))
    (set (name s2)(members $?j))
    ?f3 <- (set (name intersect)(members $?rest))
    (test (member$ ?i $?j))
    (test (not (member$ ?i $?rest)))
    =>
    (retract ?f3)
    (assert (set (name intersect)(members ?i $?rest)))
)
;聯集
(defrule union-1
    (phase compute)
    (set (name s1)(members $? ?i $?))
    ?f3 <- (set (name union)(members $?rest))
    (test (not (member$ ?i $?rest)))
    =>
    (retract ?f3)
    (assert (set (name union)(members ?i $?rest)))
)

(defrule union-2
    (phase compute)
    (set (name s2)(members $? ?i $?))
    ?f3 <- (set (name union)(members $?rest))
    (test (not (member$ ?i $?rest)))
    =>
    (retract ?f3)
    (assert (set (name union)(members ?i $?rest)))
)

;輸出
(defrule output
    (declare (salience -100))
    (set (name s1)(members $?s1))
    (set (name s2)(members $?s2))
    (set (name intersect)(members $?in))
    (set (name union)(members $?un))
    (phase compute)
    =>
    (open "C://Users/Leo/Desktop/Code Data/expert system/set.txt" mydata "w")
    (printout mydata "(set (name s1) (members " (implode$ $?s1) "))" crlf)
    (printout mydata "(set (name s2) (members " (implode$ $?s2) "))" crlf)
    (printout mydata "(set (name s1 union s2) (members " (implode$ $?un) "))" crlf)
    (printout mydata "(set (name s1 intersect s2) (members " (implode$ $?in) "))" crlf)
    (close mydata)
)