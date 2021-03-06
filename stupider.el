;; stupider.el --- Create speed reading frames for buffers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Optimise width
;; TODO: Previous word
;; IDEA: Progress bar
;; IDEA: Display zoomed out window on RHS
;; DOCS: Animated GIF as a screencast.
;; TODO: Speed as WPM
;; TODO: Improve punctuation function.
;; IDEA: Shortcuts in the echo area
;; CLEANUP: Configuration params
;; TODO: Package
;; TODO: Determine screen geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(require 'cl)

(defvar stupider-buffer-name "stupider" "The name of the buffer created by `stupider'.")
(defvar pause-time 0.16)

(defun center-frame-horizontally (&optional frame)
  "Center `frame' along the x axis."
  (interactive)
  (let* ((f (or frame (selected-frame)))
         (screen-width (x-display-pixel-width))
         (frame-width (frame-pixel-width f))
         (estimated-border-width 16)
         (required-left (max 0 (/ (- screen-width
                                     frame-width
                                     estimated-border-width) 2))))
    (set-frame-parameter f 'left required-left)))

(defun center-frame-vertically (&optional frame)
  "Center `frame' along the y axis."
  (interactive)
  (let* ((f (or frame (selected-frame)))
         (screen-height (x-display-pixel-height))
         (frame-height (frame-pixel-height f))
         (estimated-border-height 16)
         (required-top (max 0 (/ (- screen-height
                                    frame-height
                                    estimated-border-height) 2))))
    (set-frame-parameter f 'top required-top)))

(defun center-frame (&optional frame)
  "Center `frame'."
  (interactive)
  (redisplay)
  (center-frame-horizontally frame)
  (center-frame-vertically frame))

(defvar stupider-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "q") 'stupider-quit)
    (define-key km (kbd "SPC") 'stupider-toggle-pause)
    (define-key km (kbd "<up>") 'stupider-speedup)
    (define-key km (kbd "<down>") 'stupider-slowdown)
    (define-key km (kbd "n") 'stupider-tick)
    km)
  "Keymap for `stupider-speed-read'.")

(defun stupider-speed-read (source-buffer)
  "Create and run a speed reading frame for `SOURCE-BUFFER'.
Words from BUFFER are displayed individually and progressed by an adjustable timer."
  (interactive
   (list (read-buffer
          "Create stupider-speed-reader for source buffer: "
          (remove ;; Don't suggest stupider-buffer
           (when (boundp 'stupider-buffer)
             stupider-buffer)
           (buffer-list))
          t)))
  (stupider-quit) ; Ensure that it is starting from a clean state.
  (setq min-pause-time 0.1)
  (setq tokeniser "[^ \n]+")
  (setq *source-buffer* (make-indirect-buffer source-buffer "source buffer"))
  (setq stupider-frame
        (make-frame `((height . 1)
                      (width . 30)
                      (top . 100)
                      (mode-line-format . nil) ;; This no longer works. A workaround is below.
                      (cursor-type . nil)
                      (minibuffer . nil)
                      (left-margin . 0)
                      (left-fringe . 0)
                      (right-fringe . 0)
                      (tool-bar-lines . 0)
                      (menu-bar-lines . 0)
                      (line-spacing . 0)
                      (unsplittable . t)
                      (fill-column . 30))))
  (run-at-time "0.2 seconds" nil
               ;; The delayed execution is a bug workaround
               ;; for an incorrectly calculated length,
               ;; and hiding of the mode line.
               (lambda ()
                 (center-frame stupider-frame)
                 (setq mode-line-format nil)
                 (set-frame-height stupider-frame 1)))
  (setq stupider-buffer (get-buffer-create stupider-buffer-name))
  
  (with-current-buffer *source-buffer*
    (goto-char (point-min))
    (with-selected-frame stupider-frame
      (display-buffer stupider-buffer '((display-buffer-same-window)))))
  (stupider--do
   (fundamental-mode)
   (setq buffer-read-only t)
   (use-local-map stupider-map)
   (set-frame-font (font-spec :size 50)))
  (stupider--start))

(defmacro stupider--do (&rest body)
  "Do something in the speed reading frame."
  `(with-selected-frame stupider-frame
     (with-current-buffer stupider-buffer
       ,@body)))

(defun stupider-running-p ()
  "Is the speed reader running."
  (and (get-buffer stupider-buffer-name) t))

(defun stupider-toggle-pause ()
  "Pause or unpause."
  (interactive)
  (if (timerp resume-timer)
      (stupider--stop)
    (stupider--start)))

(defun stupider--start ()
  "Start, and then continue on a timer."
  (stupider-tick)
  (stupider--resume pause-time))

(defun stupider--resume (delay)
  "Iterate after a delay of `DELAY' seconds."
  (if (> delay 0)
      (setq resume-timer
            (run-at-time
             (format "%2f seconds" delay)
             nil #'stupider-tick))
    (stupider-quit)))

(defun stupider--stop ()
  "Stop. Well more of a pause than a stop, really."
  (when (and (boundp 'resume-timer) (timerp resume-timer))
    (cancel-timer resume-timer)
    (setq resume-timer nil)))

(defun stupider-quit ()
  "Quit the speed reader."
  (interactive)
  (stupider--stop)
  (when (and (boundp 'stupider-buffer)
             (buffer-live-p stupider-buffer))
    (kill-buffer stupider-buffer)
    (setq stupider-buffer nil))
  (when (buffer-live-p (get-buffer "source buffer"))
    (kill-buffer "source buffer")
    (setq *source-buffer* nil))
  (when (and (boundp 'stupider-frame)
             (frame-live-p stupider-frame))
    (delete-frame stupider-frame)
    (setq stupider-frame nil)))

(defun stupider-speedup ()
  "Speed control."
  (interactive)
  (setq pause-time
        (max (- pause-time 0.04)
             min-pause-time))
  (stupider--log))

(defun stupider-slowdown ()
  "Speed control."
  (interactive)
  (setq pause-time (+ pause-time 0.1))
  (stupider--log))

(defun stupider-tick ()
  "Progress the reading.
This function handles being called either during manual or automatic iteration,
whether paused or not."
  (interactive)
  (let ((was-running-p (and (boundp 'resume-timer) (timerp resume-timer)))
        (s (with-current-buffer *source-buffer*
             (and (search-forward-regexp tokeniser nil t)
                  (match-string-no-properties 0)))))
    (stupider--stop) ; ensure stopped
    (if s
        (let ((center (max (/ (length s) 2) 1)))
          (stupider--do
           (setq buffer-read-only nil)
           (put-text-property 0 (length s) 'face '(foreground-color . "DeepSkyBlue") s)
           (put-text-property (- center 1) center 'face '(foreground-color . "tomato") s)
           (goto-char (point-min))
           (loop repeat
                 (max (- (/ 30 2) center) 0)
                 do (insert " "))
           (insert s)
           (delete-region (point) (point-max))
           (setq buffer-read-only t)
           (when was-running-p
             (stupider--resume (* (stupider--punctuation-weighting s)
                                  pause-time)))))
      (stupider-quit))))

(defun stupider--punctuation-weighting (str)
  "Returns the punctuation delay modifier for `STR', where 0 is a request to stop."
  (if str
      (*
       ;; last character modifier -- pause for questions, etc.
       (case (last (car (last (string-to-list str))))
         (?, 1.4)
         (?\; 1.6)
         (?. 2)
         (?\: 2.2)
         (?\? 2.2)
         (t 1))
       ;; string length modifier -- pause longer for long words
       (let ((long-word-length 12) ;; definition of a long word.
             (extra-pause-mutiplier 0.5)
             (len (length str)))
         (+ 1 (* extra-pause-mutiplier
                 (/ (- len (% len long-word-length))
                    long-word-length)))))
    0))

(defun stupider--log ()
  "User feedback."
  (message (format "Pause-time: %.2f" pause-time)))

(provide 'stupider)

;;; stupider.el ends here
