(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson (slot ID) (slot state) (slot class) (slot teacher) (slot type) (multislot time) (multislot room)) 
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))
(deftemplate swap-attempt (slot count))

(deffacts initial
  (alltime 101 102 103 104 105 106 107 108 109 110 
           201 202 203 204 205 206 207 208 209 210
           301 302 303 304 305 306 307 308 309 310
           401 402 403 404 405 406 407 408 409 410
           501 502 503 504 505 506 507 508 509 510)
  (phase get-lesson)
  (attempts 0)
  (swap-attempt (count 0))
)

(defrule assert-data
    (declare (salience 10000))
    =>
    (if (load-facts "C:/Users/Leo/Desktop/Code Data/expert system/final/data.txt") then
        (printout t "Data loaded successfully from data.txt" crlf)
    else
        (printout t "Error: Failed to load data.txt" crlf)
        (halt))
    ;  (if (open "C:/Users/Leo/Desktop/Code Data/expert system/final/result.txt" res "w") then
    ;      (printout t "Output file opened successfully" crlf)
    ;  else
    ;      (printout t "Error: Failed to open result.txt" crlf)
    ;      (halt))
)

; 權重選擇 這邊沒動過
(defrule select-lesson
    ?p <- (phase get-lesson)
    (not (select ?))
    (teacher (ID ?teacher) (weight ?weight))
    (lesson (ID ?lesson) (state 0) (teacher ?teacher))
    (not (and (teacher (ID ?else) (weight ?w2&:(< ?w2 ?weight)))
              (lesson (state 0) (teacher ?else))))
    ;(test (or (eq ?type laboratory) (eq ?type computer)))
    =>
    (retract ?p)
    (assert (select ?lesson))
    (assert (phase schedule-3))
)

(defrule schedule-3-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test (and (member$ ?t1 $?f-t) (member$ ?t2 $?f-t) (member$ ?t3 $?f-t)))
        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +3
        ;(printout t "Lesson " ?select " weight " 3 crlf);
        (modify ?f3 (weight (+ ?weight 3)))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-2-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?f-t) then 1 else 0)
                    (if (member$ ?t2 $?f-t) then 1 else 0)
                    (if (member$ ?t3 $?f-t) then 1 else 0)
                )
            2)
        )
        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +2
        (modify ?f3 (weight (+ ?weight 2)))
        ;(printout t "Lesson " ?select " weight " 2 crlf);
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-1-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?f-t) then 1 else 0)
                    (if (member$ ?t2 $?f-t) then 1 else 0)
                    (if (member$ ?t3 $?f-t) then 1 else 0)
                )
            1)
        )
        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +1
        (modify ?f3 (weight (+ ?weight 1)))
        ;(printout t "Lesson " ?select " weight " 1 crlf);
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-0-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +0
        (modify ?f3 (weight (+ ?weight 0)))
        ;(printout t "Lesson " ?select " weight " 0 crlf)
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-whatever-1-rf-time
    (declare (salience -10))
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?r-t) then 1 else 0)
                    (if (member$ ?t2 $?r-t) then 1 else 0)
                    (if (member$ ?t3 $?r-t) then 1 else 0)
                )
            1)
        )

        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +0
        (modify ?f3 (weight (+ ?weight 0)))
        (printout t "Lesson " ?select " weight 0 and 1 rf time" crlf)
        (assert (in-rf-time ?select))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-whatever-2-rf-time
    (declare (salience -11))
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?r-t) then 1 else 0)
                    (if (member$ ?t2 $?r-t) then 1 else 0)
                    (if (member$ ?t3 $?r-t) then 1 else 0)
                )
            2)
        )

        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +0
        (modify ?f3 (weight (+ ?weight 0)))
        (printout t "Lesson " ?select " weight 0 and 2 rf time" crlf)
        (assert (in-rf-time ?select))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule schedule-whatever-any-time
    (declare (salience -12))
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )
        
        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +0
        (modify ?f3 (weight (+ ?weight 0)))
        (printout t "Lesson " ?select " weight 0 and 3 rf time" crlf)
        (assert (in-rf-time ?select))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))

)

(defrule check-completion
    (declare (salience -1000))
    ?f <- (phase get-lesson)
    (not (lesson (state 0)))
    =>
    (retract ?f)
    (printout t "Phase Swap.." crlf)
    (assert (phase swap))
)

(defrule swap-class-a-b-change
    ?f <- (phase swap)
    (alltime $? ?tA1 ?tA2 ?tA3 $? ?tB1 ?tB2 ?tB3 $?)
    (test (and 
              (= (+ ?tA1 1) ?tA2)
              (= (+ ?tA2 1) ?tA3)
              (= (div ?tA1 100) (div ?tA2 100) (div ?tA3 100))))
    (test (and 
              (= (+ ?tB1 1) ?tB2)
              (= (+ ?tB2 1) ?tB3)
              (= (div ?tB1 100) (div ?tB2 100) (div ?tB3 100))))
    ?in-rf <- (in-rf-time ?A)
    ?fA1 <- (lesson (ID ?A) (teacher ?teacherA) (time ?a1 ?a2 ?a3)(class ?classA)(room)(type ?tyA))
    ?fA2 <- (teacher (ID ?teacherA) (weight ?weightA))
    (refuse-time (teacher ?teacherA) (time $?ra-t))
    (favorite-time (teacher ?teacherA) (time $?fa-t))
    ?fB1 <- (lesson (ID ?B) (teacher ?teacherB) (time ?b1 ?b2 ?b3)(class ?classB)(room)(type ?tyB))
    ?fB2 <- (teacher (ID ?teacherB) (weight ?weightB))
    (refuse-time (teacher ?teacherB) (time $?rb-t))
    (favorite-time (teacher ?teacherB) (time $?fb-t))

    (classroom (ID ?r1) (type ?tyA))
    (classroom (ID ?r2) (type ?tyB))

    ; (not (lesson (ID ?id&~?B) (teacher ?teacherA) (time $? ?tA1|?tA2|?tA3 $?)))
    ; (not (lesson (ID ?id&~?B) (class ?classA) (time $? ?tA1|?tA2|?tA3 $?)))
    ; (not (lesson (ID ?id&~?B) (time $? ?tA1|?tA2|?tA3 $?) (room ?r1 ?r1 ?r1)))
    ; (not (lesson (ID ?id&~?A) (teacher ?teacherB) (time $? ?tB1|?tB2|?tB3 $?)))
    ; (not (lesson (ID ?id&~?A) (class ?classB) (time $? ?tB1|?tB2|?tB3 $?)))
    ; (not (lesson (ID ?id&~?A) (time $? ?tB1|?tB2|?tB3 $?) (room ?r2 ?r2 ?r2)))

    (test (not (or (member$ ?tA1 $?ra-t) (member$ ?tA2 $?ra-t) (member$ ?tA3 $?ra-t))))
    (test (not (or (member$ ?tB1 $?rb-t) (member$ ?tB2 $?rb-t) (member$ ?tB3 $?rb-t))))

    (test (not (and (member$ ?a1 (create$ ?tA1 ?tA2 ?tA3))
                    (member$ ?a2 (create$ ?tA1 ?tA2 ?tA3))
                    (member$ ?a3 (create$ ?tA1 ?tA2 ?tA3)))))
    (test (not (and (member$ ?b1 (create$ ?tB1 ?tB2 ?tB3))
                    (member$ ?b2 (create$ ?tB1 ?tB2 ?tB3))
                    (member$ ?b3 (create$ ?tB1 ?tB2 ?tB3)))))

    =>
    (retract ?in-rf)
    (bind ?a-old-weight (min 3 (+ (if (member$ ?a1 $?fa-t) then 1 else 0)
                                  (if (member$ ?a2 $?fa-t) then 1 else 0)
                                  (if (member$ ?a3 $?fa-t) then 1 else 0))))
    (bind ?b-old-weight (min 3 (+ (if (member$ ?b1 $?fb-t) then 1 else 0)
                                  (if (member$ ?b2 $?fb-t) then 1 else 0)
                                  (if (member$ ?b3 $?fb-t) then 1 else 0))))
    (bind ?a-new-weight (min 3 (+ (if (member$ ?tA1 $?fa-t) then 1 else 0)
                                  (if (member$ ?tA2 $?fa-t) then 1 else 0)
                                  (if (member$ ?tA3 $?fa-t) then 1 else 0))))
    (bind ?b-new-weight (min 3 (+ (if (member$ ?tB1 $?fb-t) then 1 else 0)
                                  (if (member$ ?tB2 $?fb-t) then 1 else 0)
                                  (if (member$ ?tB3 $?fb-t) then 1 else 0))))
    (printout t "Swap " ?A " and " ?B crlf)
    (modify ?fA2 (weight (+ (- ?weightA ?a-old-weight) ?a-new-weight)))
    (modify ?fB2 (weight (+ (- ?weightB ?b-old-weight) ?b-new-weight)))
    (modify ?fA1 (time ?tA1 ?tA2 ?tA3) (room ?r1 ?r1 ?r1))
    (modify ?fB1 (time ?tB1 ?tB2 ?tB3) (room ?r2 ?r2 ?r2))
)


(defrule swap-completion
    ?f <- (phase swap)
    (not (in-rf-time ?))
    =>
    (retract ?f)
    (printout t "Phase Print.." crlf)
    (assert (phase print-2-file))
)



(defrule print-schedule
    (declare (salience -2000))
    (phase print-2-file)
    (lesson (ID ?id) (state ?state) (class ?class) (teacher ?teacher) (type ?type) (time $?times) (room $?rooms))
    =>
    ;(printout t "(lesson (ID " ?id ") (state " ?state ") (class " ?class ") (teacher " ?teacher ") (type " ?type ") (time " (implode$ ?times) ") (room " (implode$ ?rooms) "))" crlf)
    (printout res "(lesson (ID " ?id ") (state " ?state ") (class " ?class ") (teacher " ?teacher ") (type " ?type ") (time " (implode$ ?times) ") (room " (implode$ ?rooms) "))" crlf)
)

(defrule close-file
    (declare (salience -5000))
    (phase print-2-file)
    =>
    (close res)
    (printout t "Terminated..." crlf)
    (halt)
)

(defrule print-the-class
    (declare (salience -10000))
    ;(phase print-2-file)
    (classroom (ID ?room))
    (not (printed-class ?room))
    =>
    (printout t crlf "Timetable for room " ?room ":" crlf)
    (bind ?days (create$ "Mon" "Tue" "Wed" "Thu" "Fri"))
    (loop-for-count (?d 1 5)
        (bind ?day (nth$ ?d ?days))
        (printout t ?day " ")
        (loop-for-count (?t 1 10)
        (bind ?slot (+ (* ?d 100) ?t))
        (if (any-factp ((?l lesson))
                        (and 
                            (member$ ?slot ?l:time)
                            (member$ ?room ?l:room)))
        then
            (do-for-fact ((?l lesson))
                        (and 
                            (member$ ?slot ?l:time)
                            (member$ ?room ?l:room))
                        (printout t ?l:ID " "))
        else
            (printout t "_ "))
        )
        (printout t crlf)
    )
    (assert (printed-class ?room))
)