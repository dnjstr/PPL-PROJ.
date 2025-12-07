;;; ========================================================================
;;; EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
;;; Functional Paradigm - Common Lisp Implementation
;;; Optimized for Online Compilers
;;; ========================================================================

;;; ========================================================================
;;; DATA STRUCTURES
;;; ========================================================================

(defstruct time-slot
  (hour 0 :type integer)
  (minute 0 :type integer))

(defstruct event
  title
  start-time
  end-time
  location
  resource
  description
  (conflicting nil :type boolean))

;;; ========================================================================
;;; PURE FUNCTIONS
;;; ========================================================================

(defun time-to-minutes (time-slot)
  (+ (* (time-slot-hour time-slot) 60)
     (time-slot-minute time-slot)))

(defun time-overlaps-p (start1 end1 start2 end2)
  (let ((s1 (time-to-minutes start1))
        (e1 (time-to-minutes end1))
        (s2 (time-to-minutes start2))
        (e2 (time-to-minutes end2)))
    (and (< s1 e2) (> e1 s2))))

(defun events-overlap-p (event1 event2)
  (time-overlaps-p (event-start-time event1)
                   (event-end-time event1)
                   (event-start-time event2)
                   (event-end-time event2)))

(defun location-conflict-p (event1 event2)
  (and (events-overlap-p event1 event2)
       (string= (event-location event1) (event-location event2))))

(defun resource-conflict-p (event1 event2)
  (and (events-overlap-p event1 event2)
       (or (search (event-resource event1) (event-resource event2) :test #'char=)
           (search (event-resource event2) (event-resource event1) :test #'char=))))

(defun find-conflicts (events)
  (let ((conflicts '()))
    (loop for i from 0 below (length events)
          do (loop for j from (1+ i) below (length events)
                   do (let ((e1 (nth i events))
                            (e2 (nth j events)))
                        (when (location-conflict-p e1 e2)
                          (push (list :type "Location"
                                     :resource (event-location e1)
                                     :event1 e1
                                     :event2 e2)
                                conflicts))
                        (when (and (resource-conflict-p e1 e2)
                                   (not (location-conflict-p e1 e2)))
                          (push (list :type "Resource"
                                     :resource (event-resource e1)
                                     :event1 e1
                                     :event2 e2)
                                conflicts)))))
    (reverse conflicts)))

(defun mark-conflicting-events (events conflicts)
  (mapcar (lambda (event)
            (let ((has-conflict
                    (some (lambda (conflict)
                            (or (eq event (getf conflict :event1))
                                (eq event (getf conflict :event2))))
                          conflicts)))
              (if has-conflict
                  (let ((new-event (copy-structure event)))
                    (setf (event-conflicting new-event) t)
                    new-event)
                  event)))
          events))

(defun sort-events-chronologically (events)
  (sort (copy-list events)
        (lambda (e1 e2)
          (< (time-to-minutes (event-start-time e1))
             (time-to-minutes (event-start-time e2))))))

(defun filter-events-by-resource (events resource-name)
  (remove-if-not (lambda (event)
                   (search resource-name (event-resource event) :test #'char=))
                 events))

(defun count-conflicting-events (events)
  (count-if #'event-conflicting events))

;;; ========================================================================
;;; FORMATTING FUNCTIONS
;;; ========================================================================

(defun format-time (time-slot)
  (format nil "~2,'0d:~2,'0d" 
          (time-slot-hour time-slot)
          (time-slot-minute time-slot)))

(defun print-conflict (conflict num)
  (format t "~%Conflict ~d: ~a Overlap~%" num (getf conflict :type))
  (format t "Type: ~a Double-Booking~%" (getf conflict :type))
  (format t "~a: ~a~%" 
          (if (string= (getf conflict :type) "Location") "Location" "Resource")
          (getf conflict :resource))
  (format t "Conflicting Events:~%")
  (let ((e1 (getf conflict :event1))
        (e2 (getf conflict :event2)))
    (format t "  - ~a (~a - ~a)" 
            (event-title e1)
            (format-time (event-start-time e1))
            (format-time (event-end-time e1)))
    (when (string= (getf conflict :type) "Resource")
      (format t " in ~a" (event-location e1)))
    (format t "~%  - ~a (~a - ~a)" 
            (event-title e2)
            (format-time (event-start-time e2))
            (format-time (event-end-time e2)))
    (when (string= (getf conflict :type) "Resource")
      (format t " in ~a" (event-location e2)))
    (format t "~%"))
  (format t "Resolution Guidance:~%")
  (if (string= (getf conflict :type) "Location")
      (progn
        (format t "  - Relocate one event to an available room~%")
        (format t "  - Adjust event times to avoid overlap~%"))
      (progn
        (format t "  - Assign an alternate resource~%")
        (format t "  - Reschedule one of the events~%")))
  (format t "~%"))

(defun print-conflict-report (conflicts)
  (format t "~%========================================~%")
  (format t "CONFLICT DETECTION AND RESOLUTION REPORT~%")
  (format t "========================================~%~%")
  
  (if (null conflicts)
      (format t "No conflicts detected.~%~%")
      (loop for conflict in conflicts
            for i from 1
            do (print-conflict conflict i))))

(defun print-chronological-schedule (events)
  (let* ((sorted-events (sort-events-chronologically events))
         (conflict-count (count-conflicting-events sorted-events)))
    
    (format t "~%========================================~%")
    (format t "CHRONOLOGICAL SCHEDULE DISPLAY~%")
    (format t "========================================~%~%")
    
    (format t "Summary: ~d total events, ~d conflicts detected, ~d events affected~%~%"
            (length events)
            (/ conflict-count 2)
            conflict-count)
    
    (format t "Event List (Temporal Order):~%~%")
    
    (dolist (event sorted-events)
      (format t "~a - ~a: ~a (~a)~%"
              (format-time (event-start-time event))
              (format-time (event-end-time event))
              (event-title event)
              (event-location event))
      (format t "  Resource: ~a~%" (event-resource event))
      (format t "  Status: ~a~%"
              (if (event-conflicting event)
                  "Conflicting"
                  "Successfully Scheduled"))
      (format t "  Description: ~a~%~%" (event-description event)))))

(defun print-filtered-view (events resource-name)
  (let* ((filtered-events (filter-events-by-resource events resource-name))
         (conflict-count (count-conflicting-events filtered-events)))
    
    (format t "~%========================================~%")
    (format t "FILTERED VIEW: ~a~%" resource-name)
    (format t "========================================~%~%")
    
    (if (null filtered-events)
        (format t "No events found for this resource.~%")
        (progn
          (dolist (event filtered-events)
            (format t "~a - ~a: ~a (~a)~%"
                    (format-time (event-start-time event))
                    (format-time (event-end-time event))
                    (event-title event)
                    (event-location event))
            (format t "  Status: ~a~%~%"
                    (if (event-conflicting event)
                        "Conflicting"
                        "No Conflict")))
          
          (format t "Summary: ~d events found, ~d conflicts~%"
                  (length filtered-events)
                  conflict-count)
          
          (when (and (> (length filtered-events) 1)
                     (= conflict-count (length filtered-events)))
            (format t "WARNING: 100%% conflict rate - severe over-scheduling detected!~%")
            (format t "Recommendation: Implement buffer time between events.~%"))))))

;;; ========================================================================
;;; INITIALIZATION
;;; ========================================================================

(defun create-sample-events ()
  (list
   (make-event :title "Math Seminar"
               :start-time (make-time-slot :hour 9 :minute 0)
               :end-time (make-time-slot :hour 10 :minute 30)
               :location "Room 201"
               :resource "Prof. A"
               :description "Linear Algebra Review")
   
   (make-event :title "CS Department Meeting"
               :start-time (make-time-slot :hour 10 :minute 0)
               :end-time (make-time-slot :hour 11 :minute 0)
               :location "Room 201"
               :resource "Prof. B, Prof. C"
               :description "Curriculum planning discussions")
   
   (make-event :title "Project Review Session"
               :start-time (make-time-slot :hour 9 :minute 45)
               :end-time (make-time-slot :hour 10 :minute 15)
               :location "Room 101"
               :resource "Prof. A"
               :description "Student capstone reviews")
   
   (make-event :title "Lab Equipment Maintenance"
               :start-time (make-time-slot :hour 11 :minute 0)
               :end-time (make-time-slot :hour 12 :minute 0)
               :location "Computer Lab"
               :resource "Technician Joe"
               :description "Monthly maintenance checks")))

;;; ========================================================================
;;; MAIN FUNCTION
;;; ========================================================================

(defun run-scheduling-system ()
  (format t "EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM~%")
  (format t "Functional Paradigm Implementation (Common Lisp)~%")
  (format t "===============================================~%")
  
  (let* ((events (create-sample-events))
         (conflicts (find-conflicts events))
         (marked-events (mark-conflicting-events events conflicts)))
    
    (print-conflict-report conflicts)
    (print-chronological-schedule marked-events)
    (print-filtered-view marked-events "Prof. A")
    
    ;; Force output flush for some online compilers
    (finish-output)))

;;; ========================================================================
;;; AUTO-EXECUTE FOR ONLINE COMPILERS
;;; ========================================================================

(format t "Starting program...~%")
(finish-output)

(run-scheduling-system)

(format t "~%Program completed successfully!~%")
(finish-output)