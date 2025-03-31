;;; phd-ark-tabline.el --- My tabline customization
;; ----------------------------------------------
;;; Author: phdenzel
;;
;;
;;; Commentary:
;; ------------
;; This is my module for customising the Emacs tab-line.
;;
;;; Installation (for example):
;; ----------------------------
;; (add-to-list 'load-path "~/phd-ark-tabline/")
;; (require 'phd-ark-tabline)
;; (phd-ark-tabline-mode 1)
;; (global-set-key (kbd "C-x ^") 'phd-ark-tabline-mode)
;;
;; or
;;
;; (use-package phd-ark-tabline
;;   :ensure nil
;;   :load-path "~/phd-ark-tabline/"
;;   :hook (after-init . phd-ark-tabline-mode)
;;   :bind (("C-x |" . phd-ark-tabline-mode))
;;   :config (...))
;;
;;; Code:
(require 's)
(require 'tab-line)


(defgroup phd-ark-tabline nil
  "A minimal, but visually slightly nicer tab-line."
  :group 'tab-line
  :link '(url-link :tag "Homepage" "https://github.com/phdenzel/phd-ark-tabline"))


;; --- Mode map
(defvar phd-ark-tabline-mode-map (make-sparse-keymap))


;; Customization
(defcustom phd-ark-tabline-tab-max-width 30
  "The maximum width of a phd-ark-tabline tab."
  :type 'integer
  :group 'phd-ark-tabline)

(defcustom phd-ark-tabline-tab-min-width 10
  "The minimum width of a phd-ark-tabline tab."
  :type 'integer
  :group 'phd-ark-tabline)


;; --- Functions
(defun phd-ark-tabline-tabs-window-buffers ()
  "Filtering scheme for displaying phd-ark-tabline tabs."
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (next-buffers (seq-remove (lambda (b) (or (eq b buffer)
                                                   (s-starts-with? "*" (buffer-name b))))
                                   (window-next-buffers window)))
         (next-buffers (seq-filter #'buffer-live-p next-buffers))
         (prev-buffers (seq-remove (lambda (b) (or (eq b buffer)
                                                   (s-starts-with? "*" (buffer-name b))))
                                   (mapcar #'car (window-prev-buffers window))))
         (prev-buffers (seq-filter #'buffer-live-p prev-buffers))
         ;; Remove next-buffers from prev-buffers
         (prev-buffers (seq-difference prev-buffers next-buffers)))
    (append (reverse prev-buffers)
            (list buffer)
            next-buffers)))


(defun phd-ark-tabline-tab-name-buffer (buffer &rest _buffers)
  "BUFFER naming scheme for phd-ark-tabline with padding and truncation.
The buffer name is centred and truncated if its length falls outside of limits
`phd-ark-tabline-tab-min-width' and `phd-ark-tabline-tab-max-width'."
  (with-current-buffer buffer
    (let* ((window-width (window-width (get-buffer-window)))
           (tab-amount (length (phd-ark-tabline-tabs-window-buffers)))
           (window-max-tab-width (if (>= (* (+ phd-ark-tabline-tab-max-width 3) tab-amount) window-width)
                                     (/ window-width tab-amount)
                                   phd-ark-tabline-tab-max-width))
           (tab-width (- (cond ((> window-max-tab-width phd-ark-tabline-tab-max-width)
                                phd-ark-tabline-tab-max-width)
                               ((< window-max-tab-width phd-ark-tabline-tab-min-width)
                                phd-ark-tabline-tab-min-width)
                               (t window-max-tab-width))
                         3)) ;; compensation for ' x ' button
           (buffer-name (string-trim (buffer-name)))
           (name-width (length buffer-name)))
      (if (>= name-width tab-width)
          (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
        (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
               (buffer-name (concat padding buffer-name)))
          (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))


(defun phd-ark-tl/set-config ()
  "Set configuration of the phd-ark-tabline."
  (interactive)
  (setq tab-line-close-button-show t
	tab-line-new-button-show nil
	tab-line-separator ""
	tab-line-tab-name-function #'phd-ark-tabline-tab-name-buffer
	tab-line-tabs-function #'phd-ark-tabline-tabs-window-buffers
	tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
					  'face 'tab-line-tab
                                          'keymap tab-line-right-map
                                          'mouse-face 'tab-line-highlight
                                          'help-echo "Click to scroll right")
	tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
					 'face 'tab-line-tab
					 'keymap tab-line-left-map
					 'mouse-face 'tab-line-highlight
					 'help-echo "Click to scroll left")
	tab-line-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
					  'face 'tab-line-tab
                                          'keymap tab-line-tab-close-map
                                          'mouse-face 'tab-line-close-highlight
                                          'help-echo "Click to close tab")))

(defun phd-ark-tl/reset-default ()
  "Default configuration of the built-in default tab-line."
  (interactive)
  (custom-reevaluate-setting 'tab-line-close-button-show)
  (custom-reevaluate-setting 'tab-line-new-button-show)
  (custom-reevaluate-setting 'tab-line-separator)
  (custom-reevaluate-setting 'tab-line-tab-name-function)
  (custom-reevaluate-setting 'tab-line-tabs-function)
  (setq tab-line-right-button (propertize "> "
					  'display '(image :type xpm
							   :file "tabs/right-arrow.xpm"
							   :margin (2 . 0)
							   :ascent center)
					  'keymap tab-line-right-map
					  'mouse-face 'tab-line-highlight
					  'help-echo "Click to scroll right")
	tab-line-left-button (propertize " <"
					  'display '(image :type xpm
							   :file "tabs/left-arrow.xpm"
							   :margin (2 . 0)
							   :ascent center)
					  'keymap tab-line-left-map
					  'mouse-face 'tab-line-highlight
					  'help-echo "Click to scroll left")
	tab-line-close-button (propertize " x"
					  'display '(image :type xpm
							   :file "tabs/close.xpm"
							   :margin (2 . 0)
							   :ascent center)
					  'keymap tab-line-tab-close-map
					  'mouse-face 'tab-line-close-highlight
					  'help-echo "Click to close tab")
	)
  )


;;;###autoload
(define-minor-mode phd-ark-tabline-mode
  "Toggle phd-ark-tabline on or off."
  :init-value nil
  :group 'phd-ark-tabline
  :global t
  :keymap phd-ark-tabline-mode-map
  (if phd-ark-tabline-mode
      (progn
        (phd-ark-tl/set-config)
        (message "phd-ark-tabline-mode activated!"))
    (progn
      (phd-ark-tl/reset-default)
      (message "phd-ark-tabline-mode deactivated!"))))


(provide 'phd-ark-tabline)

;;; phd-ark-tabline.el ends here
