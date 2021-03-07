;; Research Group 07

;;  Sustained Attention to Response Task (SART)
;;
;;  In each trial the participant sees a letter: "O" or "Q".
;;  They must press a key every time an O appears (90% of trials),
;;  but withhold their response when the stimulus is a Q (10% of trials).
;;
;;  Cognitive Modelling Practical 2021
;;  Updated for ACT-R 7.21 by Loran Knol

;; Model created by group 7:
;; Alina Dima (s3919951)
;; Alexandru Dimofte (s4019199)
;; Lisa Koopmans (s3933083)
;; Viktor Vesely (s3970043)

;;===================;;
;;  Experiment code  ;;
;;===================;;


;; Experiment settings
(defvar *stimulus-duration* 2) ; number of seconds the stimulus is shown
(defvar *inter-stimulus-interval* 0.5) ; number of seconds between trials
(defvar *target-trials* 180) ; number of target trials
(defvar *non-target-trials* 20) ; number of non-target trials

(defvar *output-directory* "~/output/") ; location where output files are stored
(defvar *trace-to-file-only* nil) ; whether the model trace should only be saved to file and not appear in terminal
(defvar *trace-file-name* "sart-trace") ; name of file in which the trace is stored

(defvar *terminal-stream* *standard-output*) ; necessary for stream management

(defvar *visible* nil) ; visibility of the experiment window

;; Global variables for data storage
(defvar *stimuli* nil)
(defvar *trial-response* nil)
(defvar *trial-start* nil)
(defvar *trial-rt* nil)
(defvar *trial-done* nil)
(defvar *all-responses* nil)
(defvar *all-rts* nil)

;; Parallellism management
(defvar *lock* (bt:make-lock))

;; Do SART experiment n times, save trace to output file
(defun do-sart-n (n)
  (with-open-file
    (*file-stream*
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name *trace-file-name* :type "txt")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)

  (if *trace-to-file-only*
    ; If true, direct standard output only file
    (setf *standard-output* *file-stream*)
    ; Else, direct standard output to terminal and file
    (setf *standard-output*
      (make-broadcast-stream *terminal-stream* *file-stream*)))

  ; Direct ACT-R output to the stream contained within *standard-output*
  (echo-act-r-output)

  (setf *visible* nil)
  (format t "Running ~a model participants~%" n)
  (dotimes (i n)
    (let ((participant (1+ i)))
      (format t "Run ~a...~%" participant)
      (do-sart)
      (write-results-to-file
        (concatenate 'string "dat" (write-to-string participant))
        participant
        *stimuli*
        (reverse *all-responses*)
        (reverse *all-rts*))))
  (format t "Done~%")

  ; We will close *file-stream* now, so make sure *standard-output*
  ; no longer points to it
  (setf *standard-output* *terminal-stream*)
  ; We also have to make sure ACT-R knows about the new value of
  ; *standard-output*
  (echo-act-r-output)
  )
)

;; Do SART experiment 1 time
(defun do-sart ()
  (reset)
  (setf *all-responses* nil)
  (setf *all-rts* nil)
  (setf *stimuli*
    (permute-list
      (concatenate
        'list
        (make-array *target-trials* :initial-element "O")
        (make-array *non-target-trials* :initial-element "Q"))))
  (setf *visible* nil)
  (loop for stim in *stimuli* do (run-trial stim))
)

;; Do a single SART trial with a target stimulus
(defun do-sart-trial-o ()
  (setf *visible* t)
  (run-trial "O")
)

;; Do a single SART trial with a non-target stimulus
(defun do-sart-trial-q ()
  (setf *visible* t)
  (run-trial "Q")
)


;; Execute a trial with a given stimulus
(defun run-trial (stim)
  (let ((window (open-exp-window "SART Experiment"
                               :visible *visible*
                               :width 300
                               :height 300
                               :x 300
                               :y 300)))

    (add-text-to-exp-window window
                            stim
                            :width 30
                            :height 30
                            :x 145
                            :y 150)

  (add-act-r-command
    "sart-key-press"
    'key-event-handler
    "SART task key press monitor")
  (monitor-act-r-command "output-key" "sart-key-press")

  (setf *trial-response* nil)
  (setf *trial-start* (get-time))
  (setf *trial-rt* nil)
  (setf *trial-done* nil)

  (install-device window)
  (run-full-time *stimulus-duration* *visible*)
  (clear-exp-window)
  (run-full-time *inter-stimulus-interval* *visible*)

  (remove-act-r-command-monitor "output-key" "sart-key-press")
  (remove-act-r-command "sart-key-press"))

  ; Prevent race conditions
  (bt:with-lock-held
    (*lock*)
    (push *trial-response* *all-responses*)
    (push *trial-rt* *all-rts*))
)

;; Register the model's key presses (ignore the model parameter)
(defun key-event-handler (model key)
  (declare (ignore model))

  ; Prevent race conditions
  (bt:with-lock-held
  (*lock*)
    (setf *trial-rt* (/ (- (get-time) *trial-start*) 1000.0))
    (setf *trial-response* (string key))
    (setf *trial-done* t))
)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant stimuli responses rts)
  (with-open-file
    (out
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name name :type "csv")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format out "participant, trial, stimulus, response, rt~%")
    (loop
      for trial from 1
      for stimulus in stimuli
      for response in responses
      for rt in rts
      do (format out "~a, ~a, ~a, ~a, ~a~%" participant trial stimulus response rt)))
)



;;===================;;
;;    Model code     ;;
;;===================;;

(clear-all)

(define-model sart

;; Model parameters
(sgp :v t ; main trace detail
  :act low ; activation trace detail
  :sact t ; include activation trace in main trace

  :show-focus t ; show where the model is looking
  :esc t ; enable sub-symbolic level
  :rt -5 ; retrieval threshold
  :bll 0.5 ; base-level learning
  :ans 0.2 ;activation noise
  :mas 1
)

(chunk-type beginning label)
(chunk-type goal state mw-type)
(chunk-type subgoal step)
(chunk-type memory type) ;4 types: positive, neutral, negative and reminder, which reminds the model to attend
(chunk-type srmapping stimulus hand)

(add-dm
  (start isa chunk)
  (press-on-O isa srmapping stimulus "O" hand left)
  (withhold-on-Q isa srmapping stimulus "Q" hand nil)
  (startgoal isa beginning label start)
  (attend isa goal state attend mw-type nil)
  (D-wander isa goal state wander mw-type deliberate)
  (S-wander isa goal state wander mw-type spontaneous)
  (identify isa subgoal step identify)
  (get-response isa subgoal step get-response)
  (make-response isa subgoal step make-response)
  
  (wander isa chunk)
  (deliberate isa chunk)
  (spontaneous isa chunk)

  (positive isa chunk)
  (neutral isa chunk)
  (negative isa chunk)
  (reminder isa chunk)
  
  (positive1 isa memory type positive)
  (positive2 isa memory type positive)
  (negative1 isa memory type negative)
  (negative2 isa memory type negative)
  (neutral1 isa memory type neutral)
  (neutral2 isa memory type neutral)
  (remember-to-attend isa memory type reminder)
)

(set-base-levels
  (attend      10000  -10000)
  (D-wander      10000  -10000)
  (S-wander     10000  -10000)
  (press-on-O    10000  -10000)
  (withhold-on-Q  10000  -10000)

  (positive1    5000   -5000)
  (positive2    5000   -5000)
  (negative1    5000   -5000)
  (negative2    5000   -5000)
  (neutral1    5000   -5000)
  (neutral2    5000   -5000)
  (remember-to-attend   5000  -5000)
)

(p start-task
  =goal>
    isa      beginning
    label    start
  ?retrieval>
    buffer   empty
    state    free
  - state    error
==>
  +retrieval>
    isa      goal
    state    attend
  -goal>
)

(p check-current-goal
  =retrieval>
    isa           goal
    state         attend
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
  ?visual>
  - scene-change  T
==>
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           goal
  - state         nil
)

(p identify-stimulus
  ?goal>
    buffer      empty
  =retrieval>
    isa         goal
    state       attend
  =visual-location>
  ?visual>
    state       free
==>
  +visual>
    isa         move-attention
    screen-pos  =visual-location
  +goal>
    isa         subgoal
    step        get-response
  =retrieval>
    state       nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
)

(p retrieve-response
  =goal>
    isa       subgoal
    step      get-response
  =visual>
    isa       text
    value     =letter
  ?visual>
    state     free
  ?retrieval>
    state     free
==>
  +retrieval>
    isa       srmapping
    stimulus  =letter
  +goal>
    isa       subgoal
    step      make-response
  +visual>
    isa       clear-scene-change
)

(p respond-if-O
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  =letter
    hand      =hand
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      =hand
    finger    index
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  - state     nil
)

(p do-not-respond-if-Q
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  =letter
    hand      nil
==>
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  -  state     nil
)

(p spontaneous-initiate-mind-wander
  =retrieval>
    isa           goal
    state         wander
    mw-type       spontaneous
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
==>
  +goal>
    isa           goal
    state         wander
    mw-type       spontaneous
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           memory
  - type          nil
)

(p spontaneous-mind-wander
  =goal>
    isa           goal
    state         wander
    mw-type       spontaneous
  =retrieval>
    isa           memory
  - type          reminder
  ?visual>
	-	scene-change  T
==>
  =goal>
  =retrieval>
    state         nil           ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           memory
  - type          nil
)

(p deliberate-initiate-mind-wander
  =retrieval>
    isa           goal
    state         wander
    mw-type       deliberate
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
==>
  +goal>
    isa           goal
    state         wander
    mw-type      deliberate
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           memory
  - type          nil
)

(p deliberate-mind-wander-one
  =goal>
    isa           goal
    state         wander
    mw-type       deliberate
  =retrieval>
    isa           memory
  - type          reminder
  ?imaginal>
    state         free
  ?visual>
	-	scene-change  T
==>
  =goal>
  +imaginal>
    isa           memory
    type          reminder
  =retrieval>
    state         nil           ; clear retrieval buffer without strengthening chunk
  -retrieval>
)

(p deliberate-mind-wander-two
  =goal>
    isa           goal
    state         wander
    mw-type       deliberate
  ?retrieval>
    state         free
  =imaginal>
    isa           memory
    type          reminder
  ?visual>
  - scene-change  T
==>
  =goal>
  -imaginal>                  ; strengthen reminder chunk
  +retrieval>
    isa           memory
  - type          nil
)

(p notice-stimulus
  =goal>
    isa           goal
    state         wander
  ?visual>
    scene-change  t
  ?visual-location>
    buffer        full
    buffer        unrequested
  ?manual>
    state         free
  ?retrieval>                   ; The model responds only when it is not busy retrieving
    state         free
  - state         error
==>
  +manual>
    isa       punch
    hand      left
    finger    index
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  - state     nil
)

(p regain-focus
  =goal>
    isa           goal
    state         wander
  =retrieval>
    isa           memory
    type          reminder
==>
  =retrieval>
    state         nil           ; clear retrieval buffer without strengthening chunk
  -retrieval> 
  +retrieval>
    isa           goal
    state         attend
  -goal>
)

(goal-focus startgoal)

)
