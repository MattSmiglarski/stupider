;; stupider.el --- Create speed reading frames for buffers.

;; No license 2014.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Optimise width
;; TODO: Run against samples
;; IDEA: Display zoomed out window on RHS
;; DOCS: Animated GIF as a screencast.
;; TODO: Speed as WPM
;; TODO: Improve punctuation function.
;; IDEA: Shortcuts in the echo area
;; CLEANUP: Frame font in parameters
;; CLEANUP: Obvious initialisation sequence
;; CLEANUP: Configuration params
;; CLEANUP: Choose a name
;; TODO: Package
;; TODO: Confirm if already running
;; TODO: Determine screen geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defvar stupider-buffer-name "stupider" "The name of the buffer created by `stupider'.")
(defvar pause-time 1)

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
  (interactive "bCreate stupider-speed-reader for source buffer: ")
  (stupider-quit) ; Ensure that we are starting from a clean state.
  (setq min-pause-time 0.1)
  (setq tokeniser "[^ \n]+")
  (setq *source-buffer* (make-indirect-buffer source-buffer "source buffer"))
  (setq stupider-frame (make-frame  '((width . 30)
                                   (height . 1)
                                   (left . 200)
                                   (top . 100)
                                   (mode-line-format . nil)
                                   (cursor-type . nil)
                                   (minibuffer . nil)
                                   (left-margin . 0)
                                   (left-fringe . 0)
                                   (right-fringe . 0)
                                   (tool-bar-lines . 0)
                                   (menu-bar-lines . 0)
                                   (line-spacing . 0)
                                   (fill-column . 30))))
  (setq stupider-buffer (get-buffer-create stupider-buffer-name))
  (with-current-buffer *source-buffer*
    (goto-char (point-min))
    (with-selected-frame stupider-frame
      (display-buffer stupider-buffer '((display-buffer-same-window))))
    (stupider--do
     (fundamental-mode)
     (setq buffer-read-only t)
     (use-local-map stupider-map)
     (set-frame-font (font-spec :size 50))))
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
        (max (- pause-time 0.1)
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
      (case (last (car (last (string-to-list str))))
        (?, 1.4)
        (?\; 1.6)
        (?. 2)
        (?\: 2.2)
        (t 1))
    0))

(defun stupider--log ()
  "User feedback."
  (message (format "Pause-time: %.2f" pause-time)))

(provide 'stupider)

;;; stupider.el ends here
