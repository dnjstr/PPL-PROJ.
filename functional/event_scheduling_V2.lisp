;;; ========================================================================
;;; EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
;;; Functional Paradigm - Common Lisp Implementation with User Input
;;; ========================================================================

;;; ========================================================================
;;; DATA STRUCTURES
;;; ========================================================================

(defstruct time-slot
  (hour 0 :type integer)
  (minute 0 :type integer))

(defstruct event
  (id 0 :type integer)
  title
  start-time
  end-time
  location
  resource
  description
  (conflicting nil :type boolean))

;;; ========================================================================
;;; GLOBAL STATE (for user input management)
;;; ========================================================================

(defparameter *events*
  (list
   (make-event :id 1 :title "Database Design Lecture"
               :start-time (make-time-slot :hour 9 :minute 0)
               :end-time   (make-time-slot :hour 10 :minute 30)
               :location "Room 101" :resource "Prof. Smith"
               :description "Introduction to database normalization")
   (make-event :id 2 :title "Web Development Lab"
               :start-time (make-time-slot :hour 10 :minute 0)
               :end-time   (make-time-slot :hour 11 :minute 30)
               :location "Lab A" :resource "Prof. Johnson"
               :description "HTML/CSS/JavaScript fundamentals")
   (make-event :id 3 :title "Data Structures Seminar"
               :start-time (make-time-slot :hour 11 :minute 0)
               :end-time   (make-time-slot :hour 12 :minute 30)
               :location "Room 201" :resource "Prof. Williams"
               :description "Advanced tree and graph algorithms")
   (make-event :id 4 :title "Database Design Practical"
               :start-time (make-time-slot :hour 10 :minute 30)
               :end-time   (make-time-slot :hour 12 :minute 0)
               :location "Room 101" :resource "Prof. Smith"
               :description "Hands-on database design exercise")
   (make-event :id 5 :title "Algorithms Workshop"
               :start-time (make-time-slot :hour 13 :minute 0)
               :end-time   (make-time-slot :hour 14 :minute 30)
               :location "Lab B" :resource "Prof. Brown"
               :description "Algorithm optimization techniques")))

(defparameter *next-id* 6)

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

(defun find-event-by-id (events id)
  "Find an event by its ID"
  (find id events :key #'event-id :test #'=))

(defun remove-event-by-id (events id)
  "Remove an event by its ID"
  (remove id events :key #'event-id :test #'=))

;;; ========================================================================
;;; UTILITY FUNCTIONS
;;; ========================================================================

(defun split-string (string delimiter)
  "Split string by delimiter character"
  (let ((result '())
        (current ""))
    (loop for char across string
          do (if (char= char delimiter)
                 (progn
                   (push current result)
                   (setf current ""))
                 (setf current (concatenate 'string current (string char)))))
    (push current result)
    (reverse result)))

(defun string-trim-all (string)
  "Trim whitespace from string"
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

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
    (format t "  - [ID:~d] ~a (~a - ~a)" 
            (event-id e1)
            (event-title e1)
            (format-time (event-start-time e1))
            (format-time (event-end-time e1)))
    (when (string= (getf conflict :type) "Resource")
      (format t " in ~a" (event-location e1)))
    (format t "~%  - [ID:~d] ~a (~a - ~a)" 
            (event-id e2)
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
            (if (> conflict-count 0) (/ conflict-count 2) 0)
            conflict-count)
    
    (format t "Event List (Temporal Order):~%~%")
    
    (dolist (event sorted-events)
      (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
              (event-id event)
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
            (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
                    (event-id event)
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

(defun list-all-events ()
  "List all events with their details"
  (format t "~%========================================~%")
  (format t "ALL EVENTS~%")
  (format t "========================================~%~%")
  
  (if (null *events*)
      (format t "No events scheduled.~%~%")
      (dolist (event *events*)
        (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
                (event-id event)
                (format-time (event-start-time event))
                (format-time (event-end-time event))
                (event-title event)
                (event-location event))
        (format t "  Resource: ~a~%" (event-resource event))
        (format t "  Status: ~a~%"
                (if (event-conflicting event) "CONFLICTING" "OK"))
        (format t "  Description: ~a~%~%" (event-description event)))))

(defun list-conflicting-events ()
  "List only conflicting events"
  (format t "~%========================================~%")
  (format t "CONFLICTING EVENTS~%")
  (format t "========================================~%~%")
  
  (let ((conflicting (remove-if-not #'event-conflicting *events*)))
    (if (null conflicting)
        (format t "No conflicting events.~%~%")
        (progn
          (dolist (event conflicting)
            (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
                    (event-id event)
                    (format-time (event-start-time event))
                    (format-time (event-end-time event))
                    (event-title event)
                    (event-location event))
            (format t "  Resource: ~a~%" (event-resource event))
            (format t "  Description: ~a~%~%" (event-description event)))
          (format t "Total conflicting events: ~d~%~%" (length conflicting))))))

;;; ========================================================================
;;; USER INPUT FUNCTIONS
;;; ========================================================================

(defun read-line-safe ()
  "Read a line of input safely"
  (string-trim-all (read-line)))

(defun parse-time (time-str)
  "Parse time string in HH:MM format"
  (let* ((parts (split-string time-str #\:))
         (hour (parse-integer (first parts) :junk-allowed t))
         (minute (parse-integer (second parts) :junk-allowed t)))
    (if (and hour minute
             (>= hour 0) (<= hour 23)
             (>= minute 0) (<= minute 59))
        (make-time-slot :hour hour :minute minute)
        nil)))

(defun add-event-from-input ()
  "Add event from user input"
  (format t "~%========================================~%")
  (format t "ADD NEW EVENT~%")
  (format t "========================================~%")
  
  (format t "Event Title: ")
  (finish-output)
  (let ((title (read-line-safe)))
    
    (format t "Start Time (HH:MM in 24-hour format): ")
    (finish-output)
    (let ((start-time (parse-time (read-line-safe))))
      
      (format t "End Time (HH:MM in 24-hour format): ")
      (finish-output)
      (let ((end-time (parse-time (read-line-safe))))
        
        (if (and start-time end-time
                 (< (time-to-minutes start-time) (time-to-minutes end-time)))
            (progn
              (format t "Location: ")
              (finish-output)
              (let ((location (read-line-safe)))
                
                (format t "Resource (e.g., Prof. Name, Equipment): ")
                (finish-output)
                (let ((resource (read-line-safe)))
                  
                  (format t "Description: ")
                  (finish-output)
                  (let ((description (read-line-safe)))
                    
                    (let ((new-event (make-event
                                     :id *next-id*
                                     :title title
                                     :start-time start-time
                                     :end-time end-time
                                     :location location
                                     :resource resource
                                     :description description)))
                      (incf *next-id*)
                      (setf *events* (append *events* (list new-event)))
                      (format t "~%Event added successfully!~%")
                      (format t "Event [ID:~d]: ~a - ~a: ~a (~a)~%~%"
                              (event-id new-event)
                              (format-time start-time)
                              (format-time end-time)
                              title
                              location)
                      
                      ;; Check for conflicts immediately
                      (let* ((conflicts (find-conflicts *events*))
                             (marked-events (mark-conflicting-events *events* conflicts)))
                        (setf *events* marked-events)
                        (when (event-conflicting (car (last *events*)))
                          (format t "WARNING: This event has conflicts with existing events!~%")
                          (format t "Use 'Detect and Display Conflicts' to see details.~%~%"))))))))
            (format t "~%Error: Invalid time format or start time is not before end time.~%~%"))))))

(defun edit-event ()
  "Edit an existing event"
  (format t "~%========================================~%")
  (format t "EDIT EVENT~%")
  (format t "========================================~%")
  
  (let* ((conflicts (find-conflicts *events*))
         (marked-events (mark-conflicting-events *events* conflicts)))
    (setf *events* marked-events)
    (list-all-events))
  
  (format t "Enter Event ID to edit: ")
  (finish-output)
  (let ((id (parse-integer (read-line-safe) :junk-allowed t)))
    (if (null id)
        (format t "~%Invalid input.~%~%")
        (let ((event (find-event-by-id *events* id)))
          (if (null event)
              (format t "~%Event not found!~%~%")
              (progn
                (format t "~%Current Event Details:~%")
                (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
                        (event-id event)
                        (format-time (event-start-time event))
                        (format-time (event-end-time event))
                        (event-title event)
                        (event-location event))
                (format t "Resource: ~a~%" (event-resource event))
                (format t "Description: ~a~%~%" (event-description event))
                
                (format t "What would you like to edit?~%")
                (format t "1. Title~%")
                (format t "2. Time~%")
                (format t "3. Location~%")
                (format t "4. Resource~%")
                (format t "5. Description~%")
                (format t "6. Edit All~%")
                (format t "7. Cancel~%")
                (format t "Choice: ")
                (finish-output)
                
                (let ((choice (parse-integer (read-line-safe) :junk-allowed t)))
                  (cond
                    ((= choice 1)
                     (format t "New Title: ")
                     (finish-output)
                     (setf (event-title event) (read-line-safe)))
                    
                    ((= choice 2)
                     (format t "New Start Time (HH:MM): ")
                     (finish-output)
                     (let ((start (parse-time (read-line-safe))))
                       (format t "New End Time (HH:MM): ")
                       (finish-output)
                       (let ((end (parse-time (read-line-safe))))
                         (if (and start end (< (time-to-minutes start) (time-to-minutes end)))
                             (progn
                               (setf (event-start-time event) start)
                               (setf (event-end-time event) end))
                             (format t "~%Error: Invalid time.~%~%")))))
                    
                    ((= choice 3)
                     (format t "New Location: ")
                     (finish-output)
                     (setf (event-location event) (read-line-safe)))
                    
                    ((= choice 4)
                     (format t "New Resource: ")
                     (finish-output)
                     (setf (event-resource event) (read-line-safe)))
                    
                    ((= choice 5)
                     (format t "New Description: ")
                     (finish-output)
                     (setf (event-description event) (read-line-safe)))
                    
                    ((= choice 6)
                     (format t "New Title: ")
                     (finish-output)
                     (setf (event-title event) (read-line-safe))
                     
                     (format t "New Start Time (HH:MM): ")
                     (finish-output)
                     (let ((start (parse-time (read-line-safe))))
                       (format t "New End Time (HH:MM): ")
                       (finish-output)
                       (let ((end (parse-time (read-line-safe))))
                         (when (and start end)
                           (setf (event-start-time event) start)
                           (setf (event-end-time event) end))))
                     
                     (format t "New Location: ")
                     (finish-output)
                     (setf (event-location event) (read-line-safe))
                     
                     (format t "New Resource: ")
                     (finish-output)
                     (setf (event-resource event) (read-line-safe))
                     
                     (format t "New Description: ")
                     (finish-output)
                     (setf (event-description event) (read-line-safe)))
                    
                    ((= choice 7)
                     (format t "~%Edit cancelled.~%~%")
                     (return-from edit-event))
                    
                    (t
                     (format t "~%Invalid choice.~%~%")
                     (return-from edit-event)))
                  
                  (when (and choice (/= choice 7))
                    (format t "~%Event updated successfully!~%")
                    (format t "Updated Event [ID:~d]: ~a - ~a: ~a (~a)~%~%"
                            (event-id event)
                            (format-time (event-start-time event))
                            (format-time (event-end-time event))
                            (event-title event)
                            (event-location event))
                    
                    ;; Recheck conflicts
                    (let* ((conflicts (find-conflicts *events*))
                           (marked-events (mark-conflicting-events *events* conflicts)))
                      (setf *events* marked-events)
                      (if (event-conflicting event)
                          (progn
                            (format t "WARNING: This event still has conflicts!~%")
                            (format t "Use 'Detect and Display Conflicts' to see details.~%~%"))
                          (format t "This event has no conflicts.~%~%")))))))))))

(defun delete-event ()
  "Delete an existing event"
  (format t "~%========================================~%")
  (format t "DELETE EVENT~%")
  (format t "========================================~%")
  
  (let* ((conflicts (find-conflicts *events*))
         (marked-events (mark-conflicting-events *events* conflicts)))
    (setf *events* marked-events)
    (list-all-events))
  
  (format t "Enter Event ID to delete: ")
  (finish-output)
  (let ((id (parse-integer (read-line-safe) :junk-allowed t)))
    (if (null id)
        (format t "~%Invalid input.~%~%")
        (let ((event (find-event-by-id *events* id)))
          (if (null event)
              (format t "~%Event not found!~%~%")
              (progn
                (format t "~%Event to delete:~%")
                (format t "[ID:~d] ~a - ~a: ~a (~a)~%"
                        (event-id event)
                        (format-time (event-start-time event))
                        (format-time (event-end-time event))
                        (event-title event)
                        (event-location event))
                
                (format t "~%Are you sure you want to delete this event? (yes/no): ")
                (finish-output)
                (let ((confirm (string-downcase (read-line-safe))))
                  (if (or (string= confirm "yes") (string= confirm "y"))
                      (progn
                        (setf *events* (remove-event-by-id *events* id))
                        (format t "~%Event deleted successfully!~%~%")
                        
                        ;; Recheck conflicts after deletion
                        (let* ((conflicts (find-conflicts *events*))
                               (marked-events (mark-conflicting-events *events* conflicts)))
                          (setf *events* marked-events)
                          (format t "Conflicts have been re-evaluated.~%~%")))
                      (format t "~%Deletion cancelled.~%~%")))))))))

(defun display-menu ()
  "Display the main menu"
  (format t "========================================~%")
  (format t "MAIN MENU~%")
  (format t "========================================~%")
  (format t "1. Add New Event~%")
  (format t "2. View All Events~%")
  (format t "3. Detect and Display Conflicts~%")
  (format t "4. View Conflicting Events Only~%")
  (format t "5. Edit Event~%")
  (format t "6. Delete Event~%")
  (format t "7. Display Chronological Schedule~%")
  (format t "8. Filter Events by Resource~%")
  (format t "9. Exit~%")
  (format t "========================================~%"))

(defun process-menu-choice (choice)
  "Process user menu choice"
  (cond
    ((= choice 1)
     (add-event-from-input))
    
    ((= choice 2)
     (if (null *events*)
         (format t "~%No events to display. Please add events first.~%~%")
         (let* ((conflicts (find-conflicts *events*))
                (marked-events (mark-conflicting-events *events* conflicts)))
           (setf *events* marked-events)
           (list-all-events))))
    
    ((= choice 3)
     (if (null *events*)
         (format t "~%No events to check. Please add events first.~%~%")
         (let* ((conflicts (find-conflicts *events*))
                (marked-events (mark-conflicting-events *events* conflicts)))
           (setf *events* marked-events)
           (print-conflict-report conflicts))))
    
    ((= choice 4)
     (if (null *events*)
         (format t "~%No events to check. Please add events first.~%~%")
         (let* ((conflicts (find-conflicts *events*))
                (marked-events (mark-conflicting-events *events* conflicts)))
           (setf *events* marked-events)
           (list-conflicting-events))))
    
    ((= choice 5)
     (if (null *events*)
         (format t "~%No events to edit. Please add events first.~%~%")
         (edit-event)))
    
    ((= choice 6)
     (if (null *events*)
         (format t "~%No events to delete. Please add events first.~%~%")
         (delete-event)))
    
    ((= choice 7)
     (if (null *events*)
         (format t "~%No events to display. Please add events first.~%~%")
         (let* ((conflicts (find-conflicts *events*))
                (marked-events (mark-conflicting-events *events* conflicts)))
           (setf *events* marked-events)
           (print-chronological-schedule *events*))))
    
    ((= choice 8)
     (if (null *events*)
         (format t "~%No events to filter. Please add events first.~%~%")
         (progn
           (format t "~%Enter resource name to filter: ")
           (finish-output)
           (let* ((resource-name (read-line-safe))
                  (conflicts (find-conflicts *events*))
                  (marked-events (mark-conflicting-events *events* conflicts)))
             (setf *events* marked-events)
             (print-filtered-view *events* resource-name)))))
    
    ((= choice 9)
     (format t "~%Thank you for using the Event Scheduling System!~%")
     t)
    
    (t
     (format t "~%Invalid choice. Please select 1-9.~%~%")
     nil)))

;;; ========================================================================
;;; MAIN FUNCTION
;;; ========================================================================

(defun run-scheduling-system ()
  (format t "===============================================~%")
  (format t "EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM~%")
  (format t "Functional Paradigm Implementation (Common Lisp)~%")
  (format t "===============================================~%~%")
  (format t "Sample data loaded. 5 events initialized.~%~%")

  (loop
    (display-menu)
    (format t "Enter your choice: ")
    (finish-output)
    (let ((input (read-line-safe)))
      (let ((choice (parse-integer input :junk-allowed t)))
        (when choice
          (when (process-menu-choice choice)
            (return)))))))

;;; ========================================================================
;;; AUTO-EXECUTE
;;; ========================================================================

(run-scheduling-system)