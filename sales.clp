(deftemplate sales (slot id)(multislot items))
(deftemplate same (multislot pair)(multislot items))
(deftemplate recommend (slot id)(multislot similar)(multislot items))

(deffacts initial (phase load-data))

(defrule assert-data
    (phase load-data)
    =>
    (load-facts "c:/Users/Leo/Desktop/Code Data/expert system/record-02.txt")
    (open "c:/Users/Leo/Desktop/Code Data/expert system/recommend.txt" mydata "w")
)

;(defrule generate-recommend
;    (phase load-data)
;    (sales (id ?id1)(items $?))
;    =>
;    (assert (recommend (id ?id1)(similar)(items)))
;)

(defrule generate-pair
    (phase load-data)
    (sales (id ?id1)(items $?))
    (sales (id ?id2&~?id1)(items $?))
    =>
    (assert (same(pair ?id1 ?id2)(items)))
)

(defrule generate-same-items
    (phase load-data)
    ?f <- (same (pair ?id1 ?id2)(items $?items))
    (sales (id ?id1)(items $? ?x $?))
    (sales (id ?id2)(items $? ?x $?))
    (test (not (member$ ?x $?items)))
    =>
    (modify ?f (items $?items ?x))
)

(defrule change-phase-1
    (declare (salience -10))
    ?f <- (phase load-data)
    =>
    (retract ?f)
    (assert (phase find-similar))
)

(defrule similar-pre-facts
    (phase find-similar)
    (same (pair ?s1 ?s2)(items $?items))
    (sales (id ?s1)(items $? ?x $?))
    (sales (id ?s2)(items $? ?y $?))
    ?f <- (recommend (id ?s1)(similar ?s2)(items $?pre_items))
    (test (not (member$ ?x $?items)))
    (test (not (member$ ?y $?items)))
    (test (not (member$ ?y $?pre_items)))
    =>
    (modify ?f (id ?s1)(similar ?s2)(items $?pre_items ?y))
)

(defrule similar-erode-facts
    (declare (salience -2))
    (phase find-similar)
    (same (pair ?s1 ?s2)(items $?items))
    (sales (id ?s1)(items $? ?x $?))
    (sales (id ?s2)(items $? ?y $?))
    (test (not (member$ ?x $?items)))
    (test (not (member$ ?y $?items)))
    =>
    (assert (recommend (id ?s1)(similar ?s2)(items ?y)))
)

(defrule change-phase-2
    (declare (salience -10))
    ?f <- (phase find-similar)
    =>
    (retract ?f)
    (assert (phase dump))
)

(defrule dump-less
    (phase dump)
    ?f1<- (same (pair ?s1 ?s2)(items $?same_items_2))
    ?f2 <- (same (pair ?s1 ?s3)(items $?same_items_3))
    ?f3 <- (recommend (id ?s1)(similar ?s3)(items $?))
    (test (> (length$ $?same_items_2)(length$ $?same_items_3)))
    =>
    (retract ?f3)
)

(defrule dump-same
    (phase dump)
    ?f1 <- (recommend (id ?s1)(similar ?s2)(items ?a1 $?))
    ?f2 <- (recommend (id ?s1)(similar ?s2)(items ?a2 $?))
    (test (not (= (integer ?a1) (integer ?a2))))
    =>
    (retract ?f2)
)

(defrule change-phase-3
    (declare (salience -10))
    ?f <- (phase dump)
    =>
    (retract ?f)
    (assert (phase combine))
)

(defrule combine
    (phase combine)
    ?f1 <- (recommend (id ?s1)(similar $?rest_sim)(items $?rest))
    ?f2 <- (recommend (id ?s1)(similar ?s3)(items $?bring))
    (test (not (member$ ?s3 $?rest_sim)))
    =>
    (retract ?f2)
    (modify ?f1 (similar $?rest_sim ?s3)(items $?rest $?bring))
)

(defrule change-phase-4
    (declare (salience -10))
    ?f <- (phase combine)
    =>
    (retract ?f)
    (assert (phase shrink))
)

(defrule remove-same-element
    (phase shrink)
    ?f1 <- (recommend (id ?s1)(similar $?)(items $?f ?x $?m ?y $?r))
    (test (= (integer ?x) (integer ?y)))
    =>
    (modify ?f1 (items $?f ?x $?m $?r))
)

(defrule change-phase-5
    (declare (salience -10))
    ?f <- (phase shrink)
    =>
    (retract ?f)
    (assert (phase output))
)

(defrule output-to-file
    (phase output)
    ?f <- (recommend (id ?s1)(similar $?seq)(items $?seq2))
    =>
    (retract ?f)
    (printout mydata "(" ?s1 ")(" (implode$ $?seq) "):(" (implode$ $?seq2) ")" crlf)
)

(defrule close-file
    (declare (salience -100))
    (phase output)
    =>
    (close mydata)
)