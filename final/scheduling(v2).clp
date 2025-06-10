; 定義模板
(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson
    (slot ID)
    (slot state)
    (slot class)
    (slot teacher)
    (slot type)
    (multislot time)
    (multislot room))
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))

; 初始化所有時段和初始階段
(deffacts initial
    (alltime 101 102 103 104 105 106 107 108 109 110
             201 202 203 204 205 206 207 208 209 210
             301 302 303 304 305 306 307 308 309 310
             401 402 403 404 405 406 407 408 409 410
             501 502 503 504 505 506 507 508 509 510)
    (phase get-lesson)
)

; 載入 data.txt
(defrule assert-data
    (declare (salience 10000))
    =>
    (load-facts "C:/Users/Leo/Desktop/Code Data/expert system/final/data.txt")
)

; 檢查是否為同一天連續3節課
(deffunction is-same-day (?t1 ?t2 ?t3)
    (and (= (+ ?t1 1) ?t2)
         (= (+ ?t2 1) ?t3)
         (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
)

; 優先選擇稀缺教室型態的課程（laboratory, computer）
(defrule select-lesson-scarce
    ?p <- (phase get-lesson)
    (not (select ?))
    (lesson (ID ?lesson) (state 0) (teacher ?teacher) (type ?type))
    (test (or (eq ?type laboratory) (eq ?type computer)))
    =>
    (retract ?p)
    (assert (select ?lesson))
    (assert (phase schedule-3))
)

; 選擇其他課程（lecture）
(defrule select-lesson-lecture
    ?p <- (phase get-lesson)
    (not (select ?))
    (lesson (ID ?lesson) (state 0) (teacher ?teacher) (type lecture))
    (not (lesson (state 0) (type laboratory|computer)))
    =>
    (retract ?p)
    (assert (select ?lesson))
    (assert (phase schedule-3))
)

(defrule schedule-3-all-fav
  ?p <- (phase schedule-3)
  ?f1 <- (select ?select)
  ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
  (alltime $? ?t1 ?t2 ?t3 $?)
  (test (is-same-day ?t1 ?t2 ?t3))
  (refuse-time (teacher ?teacher) (time $?r-t))
  (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
  (favorite-time (teacher ?teacher) (time $?f-t))
  (test (and (member$ ?t1 $?f-t) (member$ ?t2 $?f-t) (member$ ?t3 $?f-t)))
  (classroom (ID ?classroom) (type ?type))
  (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
  (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
  (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
  =>
  (retract ?p ?f1)
  (assert (phase get-lesson))
  (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
)

(defrule schedule-3-two-fav
   ?p <- (phase schedule-3)
   ?f1 <- (select ?select)
   ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
   (alltime $? ?t1 ?t2 ?t3 $?)
   (test (is-same-day ?t1 ?t2 ?t3))
   (refuse-time (teacher ?teacher) (time $?r-t))
   (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
   (favorite-time (teacher ?teacher) (time $?f-t))
   (test (= (+ (if (member$ ?t1 $?f-t) then 1 else 0)
               (if (member$ ?t2 $?f-t) then 1 else 0)
               (if (member$ ?t3 $?f-t) then 1 else 0)) 2))
   (classroom (ID ?classroom) (type ?type))
   (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
   (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
   (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
   =>
   (retract ?p ?f1)
   (assert (phase get-lesson))
   (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
)



(defrule schedule-3-one-fav
   ?p <- (phase schedule-3)
   ?f1 <- (select ?select)
   ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
   (alltime $? ?t1 ?t2 ?t3 $?)
   (test (is-same-day ?t1 ?t2 ?t3))
   (refuse-time (teacher ?teacher) (time $?r-t))
   (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
   (favorite-time (teacher ?teacher) (time $?f-t))
   (test (= (+ (if (member$ ?t1 $?f-t) then 1 else 0)
               (if (member$ ?t2 $?f-t) then 1 else 0)
               (if (member$ ?t3 $?f-t) then 1 else 0)) 1))
   (classroom (ID ?classroom) (type ?type))
   (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
   (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
   (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
   =>
   (retract ?p ?f1)
   (assert (phase get-lesson))
   (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
)




; 分配任何可用時段（不強制喜好時段）
(defrule schedule-3-any-time
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    (alltime $? ?t1 ?t2 ?t3 $?)
    (test (is-same-day ?t1 ?t2 ?t3))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
)

; 輸出結果到 result.txt
(defrule output-result
    (declare (salience -1000))
    (not (lesson (state 0)))
    =>
    (open "C:/Users/Leo/Desktop/Code Data/expert system/final/result.txt" out "w")
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (printout out "(lesson (ID " ?l:ID ") (state 1) (class " ?l:class ") (teacher " ?l:teacher ") (type " ?l:type ") (time "
                      (nth$ 1 ?l:time) " " (nth$ 2 ?l:time) " " (nth$ 3 ?l:time) ") (room "
                      (nth$ 1 ?l:room) " " (nth$ 2 ?l:room) " " (nth$ 3 ?l:room) "))" crlf)
    )
    (close out)
    (halt)
)

; 無法排課時，記錄原因並繼續嘗試下一課程
(defrule skip-unassignable-lesson
    (declare (salience -2000))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    (not (and (alltime $? ?t1 ?t2 ?t3 $?)
              (test (is-same-day ?t1 ?t2 ?t3))
              (refuse-time (teacher ?teacher) (time $?r-t))
              (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
              (classroom (ID ?classroom) (type ?type))
              (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
              (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
              (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (printout t "Warning: Lesson " ?select " (teacher " ?teacher ", class " ?class ", type " ?type ") cannot be scheduled." crlf)
)

; 最終檢查，若有未排課，報告並終止
(defrule final-check
    (declare (salience -3000))
    =>
    (open "result.txt" out "w")
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (printout out "(lesson (ID " ?l:ID ") (state 1) (class " ?l:class ") (teacher " ?l:teacher ") (type " ?l:type ") (time "
                      (nth$ 1 ?l:time) " " (nth$ 2 ?l:time) " " (nth$ 3 ?l:time) ") (room "
                      (nth$ 1 ?l:room) " " (nth$ 2 ?l:room) " " (nth$ 3 ?l:room) "))" crlf)
    )
    (do-for-all-facts ((?l lesson)) (= ?l:state 0)
        (printout t "Error: Lesson " ?l:ID " not scheduled." crlf)
    )
    (close out)
    (halt)
)