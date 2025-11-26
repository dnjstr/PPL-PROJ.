;;; ========================================================================
;;; EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
;;; Functional Paradigm - Common Lisp Implementation
;;; 
;;; File: event-scheduling.lisp
;;; To run in Steel Bank Common Lisp (SBCL) or other Common Lisp:
;;;   1. Load the file: (load "event-scheduling.lisp")
;;;   2. Run the system: (run-scheduling-system)
;;; ========================================================================

;;; ========================================================================
;;; DATA STRUCTURES - Using structures to represent immutable data
;;; ========================================================================

(defstruct time-slot
  "Structure representing a time with hour and minute"
  (hour 0 :type integer)
  (minute 0 :type integer))

(defstruct event
  "Structure representing an event with all necessary information"
  title
  start-time
  end-time
  location
  resource
  description
  (conflicting nil :type boolean))

;;; ========================================================================
;;; PURE FUNCTIONS - Functions without side effects
;;; ========================================================================

(defun time-to-minutes (time-slot)
  "Convert a time-slot to minutes since midnight.
   This is a pure function - no side effects, always returns same output for same input."
  (+ (* (time-slot-hour time-slot) 60)
     (time-slot-minute time-slot)))

(defun time-overlaps-p (start1 end1 start2 end2)
  "Check if two time intervals overlap.
   Returns T if they overlap, NIL otherwise.
   Pure function using only its parameters."
  (let ((s1 (time-to-minutes start1))
        (e1 (time-to-minutes end1))
        (s2 (time-to-minutes start2))
        (e2 (time-to-minutes end2)))
    (and (< s1 e2) (> e1 s2))))

(defun events-overlap-p (event1 event2)
  "Check if two events overlap in time.
   Higher-order function that uses time-overlaps-p."
  (time-overlaps-p (event-start-time event1)
                   (event-end-time event1)
                   (event-start-time event2)
                   (event-end-time event2)))

(defun location-conflict-p (event1 event2)
  "Check if two events have a location conflict.
   Combines time overlap and location comparison."
  (and (events-overlap-p event1 event2)
       (string= (event-location event1) (event-location event2))))

(defun resource-conflict-p (event1 event2)
  "Check if two events have a resource conflict.
   Returns T if events overlap in time and share a resource."
  (and (events-overlap-p event1 event2)
       (or (search (event-resource event1) (event-resource event2) :test #'char=)
           (search (event-resource event2) (event-resource event1) :test #'char=))))

(defun find-conflicts (events)
  "Find all conflicts in a list of events.
   Returns a list of conflict descriptions.
   Pure function - doesn't modify input, creates new data structure."
  (let ((conflicts '()))
    (loop for i from 0 below (length events)
          do (loop for j from (1+ i) below (length events)
                   do (let ((e1 (nth i events))
                            (e2 (nth j events)))
                        ;; Check for location conflict
                        (when (location-conflict-p e1 e2)
                          (push (list :type "Location"
                                     :resource (event-location e1)
                                     :event1 e1
                                     :event2 e2)
                                conflicts))
                        ;; Check for resource conflict
                        (when (resource-conflict-p e1 e2)
                          (push (list :type "Resource"
                                     :resource (event-resource e1)
                                     :event1 e1
                                     :event2 e2)
                                conflicts)))))
    (nreverse conflicts)))

(defun mark-conflicting-events (events conflicts)
  "Mark events that have conflicts.
   Functional approach - returns a NEW list with updated events.
   Original events list remains unchanged (immutability)."
  (mapcar #'(lambda (event)
              (let ((has-conflict
                      (some #'(lambda (conflict)
                                (or (eq event (getf conflict :event1))
                                    (eq event (getf conflict :event2))))
                            conflicts)))
                (if has-conflict
                    (let ((new-event (copy-event event)))
                      (setf (event-conflicting new-event) t)
                      new-event)
                    event)))
          events))

(defun sort-events-chronologically (events)
  "Sort events by start time.
   Returns a NEW sorted list - demonstrates immutability.
   Uses higher-order function 'sort' with lambda comparator."
  (sort (copy-list events)
        #'(lambda (e1 e2)
            (< (time-to-minutes (event-start-time e1))
               (time-to-minutes (event-start-time e2))))))

(defun filter-events-by-resource (events resource-name)
  "Filter events that involve a specific resource.
   Returns NEW filtered list using higher-order function 'remove-if-not'."
  (remove-if-not #'(lambda (event)
                     (search resource-name (event-resource event) :test #'char=))
                 events))

(defun count-conflicting-events (events)
  "Count how many events are marked as conflicting.
   Uses higher-order function 'count-if' with predicate."
  (count-if #'event-conflicting events))

;;; ========================================================================
;;; FORMATTING FUNCTIONS - Higher-order functions for output
;;; ========================================================================

(defun format-time (time-slot)
  "Format a time-slot as HH:MM string.
   Pure function for formatting."
  (format nil "~2,'0d:~2,'0d" 
          (time-slot-hour time-slot)
          (time-slot-minute time-slot)))

(defun print-conflict (conflict num)
  "Print details of a single conflict with formatting."
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
  "Print the conflict detection and resolution report."
  (format t "~%========================================~%")
  (format t "CONFLICT DETECTION AND RESOLUTION REPORT~%")
  (format t "========================================~%~%")
  
  (if (null conflicts)
      (format t "No conflicts detected.~%~%")
      (loop for conflict in conflicts
            for i from 1
            do (print-conflict conflict i))))

(defun print-chronological-schedule (events)
  "Print events in chronological order.
   Demonstrates functional composition."
  (let* ((sorted-events (sort-events-chronologically events))
         (conflict-count (count-conflicting-events sorted-events)))
    
    (format t "~%========================================~%")
    (format t "CHRONOLOGICAL SCHEDULE DISPLAY~%")
    (format t "========================================~%~%")
    
    (format t "Summary: ~d total events, ~d conflicts detected, ~d events affected~%~%"
            (length events)
            (/ conflict-count 2)  ; Each conflict affects 2 events
            conflict-count)
    
    (format t "Event List (Temporal Order):~%~%")
    
    ;; Using dolist - functional iteration
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
  "Print events filtered by a specific resource.
   Demonstrates filtering and conditional output."
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
          
          ;; Warning for severe over-scheduling
          (when (and (> (length filtered-events) 1)
                     (= conflict-count (length filtered-events)))
            (format t "WARNING: 100%% conflict rate - severe over-scheduling detected!~%")
            (format t "Recommendation: Implement buffer time between events.~%"))))))

;;; ========================================================================
;;; INITIALIZATION - Create sample events
;;; ========================================================================

(defun create-sample-events ()
  "Create the sample events from the problem statement.
   Returns a list of event structures."
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
;;; MAIN FUNCTION - Compose all operations functionally
;;; ========================================================================

(defun run-scheduling-system ()
  "Main function to run the event scheduling system.
   Demonstrates functional composition and data flow.
   Each operation creates new data without modifying original."
  (format t "EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM~%")
  (format t "Functional Paradigm Implementation (Common Lisp)~%")
  (format t "===============================================~%")
  
  ;; Functional composition - each step produces new data
  ;; Notice how data flows through transformations
  (let* ((events (create-sample-events))              ; Create initial data
         (conflicts (find-conflicts events))          ; Transform: find conflicts
         (marked-events (mark-conflicting-events events conflicts))) ; Transform: mark events
    
    ;; Generate all reports using the transformed data
    (print-conflict-report conflicts)
    (print-chronological-schedule marked-events)
    (print-filtered-view marked-events "Prof. A")))

;;; ========================================================================
;;; ADDITIONAL UTILITY FUNCTIONS
;;; ========================================================================

(defun add-event (events new-event)
  "Add a new event to the events list.
   Returns a NEW list (functional/immutable approach)."
  (append events (list new-event)))

(defun get-events-at-time (events hour minute)
  "Get all events occurring at a specific time.
   Uses filter with time checking."
  (remove-if-not 
   #'(lambda (event)
       (let ((start-min (time-to-minutes (event-start-time event)))
             (end-min (time-to-minutes (event-end-time event)))
             (check-min (+ (* hour 60) minute)))
         (and (>= check-min start-min) (< check-min end-min))))
   events))

(defun get-events-in-location (events location)
  "Get all events in a specific location.
   Demonstrates filtering with location predicate."
  (remove-if-not #'(lambda (event)
                     (string= (event-location event) location))
                 events))

;;; ========================================================================
;;; EXAMPLE USAGE AND TESTING
;;; ========================================================================

(defun test-individual-functions ()
  "Test individual functions to demonstrate functional programming concepts."
  (format t "~%========================================~%")
  (format t "TESTING INDIVIDUAL FUNCTIONS~%")
  (format t "========================================~%~%")
  
  (let ((events (create-sample-events)))
    
    ;; Test 1: Filter by resource
    (format t "Events for Prof. A:~%")
    (let ((prof-a-events (filter-events-by-resource events "Prof. A")))
      (dolist (e prof-a-events)
        (format t "  - ~a~%" (event-title e))))
    
    ;; Test 2: Get events at specific time
    (format t "~%Events at 10:00:~%")
    (let ((events-at-10 (get-events-at-time events 10 0)))
      (dolist (e events-at-10)
        (format t "  - ~a~%" (event-title e))))
    
    ;; Test 3: Get events in location
    (format t "~%Events in Room 201:~%")
    (let ((room-201-events (get-events-in-location events "Room 201")))
      (dolist (e room-201-events)
        (format t "  - ~a~%" (event-title e))))
    
    ;; Test 4: Count conflicts
    (format t "~%Total conflicting events: ~d~%"
            (count-conflicting-events 
             (mark-conflicting-events events (find-conflicts events))))))

;;; ========================================================================
;;; FUNCTIONAL PARADIGM CHARACTERISTICS DEMONSTRATED:
;;; ========================================================================
;;;
;;; 1. PURE FUNCTIONS:
;;;    - Functions like time-to-minutes, time-overlaps-p always return
;;;      same output for same input
;;;    - No side effects or external state modification
;;;
;;; 2. IMMUTABILITY:
;;;    - All data transformation creates NEW data structures
;;;    - Original data remains unchanged
;;;    - See: mark-conflicting-events, sort-events-chronologically
;;;
;;; 3. HIGHER-ORDER FUNCTIONS:
;;;    - Functions that take other functions as arguments
;;;    - mapcar, remove-if-not, count-if, some
;;;
;;; 4. FUNCTION COMPOSITION:
;;;    - Building complex operations from simple functions
;;;    - See: run-scheduling-system's let* chain
;;;
;;; 5. RECURSION AND ITERATION:
;;;    - Functional iteration with dolist, loop
;;;    - List processing paradigm
;;;
;;; 6. DECLARATIVE STYLE:
;;;    - Describe WHAT to compute, not HOW
;;;    - Filter-events-by-resource expresses intent clearly
;;;
;;; ========================================================================

;;; To run the complete system, evaluate:
;;; (run-scheduling-system)

;;; To test individual functions:
;;; (test-individual-functions)

;;; ========================================================================
;;; END OF FUNCTIONAL PARADIGM IMPLEMENTATION
;;; ========================================================================