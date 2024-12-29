;; -*- coding: utf-8; -*-

;;; dict-line --- View dict in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.6
;; Package-Requires: ((emacs "24.2") (async "1.8") (posframe "1.0.0"))
;; Keywords: dict sdcv
;; URL: https://github.com/ISouthRain/dict-line

;;; Commentary:
;;
;; This package is quickly view git blame information of the current file line in Emacs in real time.

;;; Require:
(require 'async)
(require 'posframe)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup dict-line nil
  "Emacs dictionary lookup on cursor movement."
  :group 'tools)

(defcustom dict-line-dict-directory "~/my-dict/"
  "The directory where .ts dictionary files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-dict-personal-file "~/my-dict/my-dict.ts"
  "Personal dict file"
  :type 'string
  :group 'dict-line)

(defcustom dict-line-audio nil
  "Toggle play audio file."
  :type 'boolean
  :group 'dict-line)

(defcustom dict-line-audio-root-dir "~/my-dict/my-audio/"
  "The directory where audio files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-audio-play-program "mplayer"
  "Play audio file program.
List: `mplayer`, `mpg123`, `mpv`"
  :type 'string
  :group 'dict-line)

(defcustom dict-line-audio-play-program-arg ""
  "Audio play program arguments.
Default example: -volume 80 to mplayer play volume 80%"
  :type 'string
  :group 'dict-line)

(defcustom dict-line-idle-time 0.5
  "Idle time in seconds before triggering dictionary lookup."
  :type 'number
  :group 'dict-line)

(defcustom dict-line-display #'dict-line--message
  "dict-line to display function."
  :type '(choice (const nil)
                 function)
  :group 'dict-line)

(defcustom dict-line-posframe-border-width 10
  "The border width of dict-line-posframe, in pixels."
  :type 'integer
  :group 'dict-line)

(defface dict-line-posframe-face
  '((t (:foreground "#00ff00" :background "gray12")))
  "Face for sdcv tooltip"
  :group 'dict-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dict-line-word nil
  "dict-line point word.")

(defvar dict-line-dict nil
  "dict-line result dict txt.")

(defvar dict-line--current-buffer nil
  "dict-line word current buffer name.")

(defvar dict-line--posframe-buffer " *dict-line-posframe*"
  "dict-line show dict txt buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dict-line--message ()
  "dict-line display function."
  (dict-line--dict-convert)
  (message dict-line-dict))

(defun dict-line--posframe ()
  "Show translation in the posframe"
  (dict-line--dict-convert)
  (when (posframe-workable-p)
    (posframe-show dict-line--posframe-buffer
                   :string dict-line-dict
                   :max-width 30
                   :timeout 10
                   :position (point)
                   :background-color (face-attribute 'dict-line-posframe-face :background)
                   :foreground-color (face-attribute 'dict-line-posframe-face :foreground)
                   :border-width dict-line-posframe-border-width
                   :border-color (face-attribute 'dict-line-posframe-face :background)))
  (unwind-protect
      (push (read-event " ") unread-command-events)
    (posframe-delete dict-line--posframe-buffer)))

(defun dict-line--posframe-delete ()
  "Delete the posframe associated with BUFFER if it exists."
  (when (eq dict-line-display #'dict-line--posframe)
    (posframe-hide dict-line--posframe-buffer))
  )

(defun dict-line--dict-convert ()
  "dict-line convert dict txt."
  (setq dict-line-dict (replace-regexp-in-string "\\\\\\\\n" "\n" dict-line-dict))
  (setq dict-line-dict (replace-regexp-in-string "\"," "\" " dict-line-dict))
  (setq dict-line-dict (substring dict-line-dict 1 -2))
  )

;; Inspired by: github.com/manateelazycat/sdcv/blob/master/sdcv.el#L526
(defun dict-line--play-audio (word)
  "Play the audio pronunciation of the given WORD."
  (if (featurep 'cocoa)
      ;; macOS: Use `say` command
      (call-process-shell-command
       (format "say %s" word) nil 0)
    ;; Non-macOS: Use the specified audio play program
    (let ((player (executable-find dict-line-audio-play-program)))
      (if player
          (let ((args (split-string dict-line-audio-play-program-arg)))
            ;; Ensure args is a list, even if empty
            (unless args (setq args '()))
            ;; Add the URL as the last argument
            (setq args (append args (list (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))))
            ;; Start the process
            (apply #'start-process player nil player args))
        (message "%s is needed to play word voice" dict-line-audio-play-program)))))

;;;###autoload
(defun dict-line--get-dict-async ()
  "Check the word under cursor and look it up in the dictionary asynchronously."
  (interactive)
  (let ((word (if (use-region-p) ;; Check if there is a selected area
                  (buffer-substring-no-properties (region-beginning) (region-end)) ;; Use selected text
                (thing-at-point 'word t))) ;; Otherwise use the word under the cursor
        (buffer (get-buffer (buffer-name)))
        (dir dict-line-dict-directory)) ;; Extract dictionary directory
    (setq dict-line-word word)
    (setq dict-line--current-buffer buffer) ;; Need to query the word buffer
    (when (and word (not (minibufferp)))
      (async-start
       `(lambda ()
          (let ((dict-files (directory-files ,dir t "\\.ts$"))
                (dicts nil))
            (while (and dict-files (not dicts))
              (with-temp-buffer
                (insert-file-contents (car dict-files))
                (goto-char (point-min))
                (when (search-forward (concat "\"" ,word "\":") nil t)
                  (setq dicts (buffer-substring-no-properties (point) (line-end-position)))))
              (setq dict-files (cdr dict-files)))
            dicts))
       ;; Callback
       (lambda (dicts)
         ;; Play audio
         (when dict-line-audio
           (dict-line--play-audio dict-line-word))
         (when dicts
           (setq dict-line-dict dicts)
           (with-current-buffer (get-buffer-create dict-line--current-buffer)
             (when (functionp dict-line-display)
               (funcall dict-line-display))))))
      )
    )
  )

;;;###autoload
(defun dict-line-word-save-from-echo ()
  "Extract the word under the cursor, prompt the user to enter information, and then save 'word': 'Input information' to the last line of the specified file."
  (interactive)
  (let* ((word (thing-at-point 'word t))
         (input (read-string (format "Enter information for '%s': " word)))
         (entry (format "\"%s\":\"%s\"," word input)))
    (when (and word input)
      (with-temp-buffer
        (insert-file-contents dict-line-dict-personal-file)
        (goto-char (point-max))
        (insert (concat "\n" entry))
        (write-region (point-min) (point-max) dict-line-dict-personal-file))
      (message "Save %s to %s" entry dict-line-dict-personal-file))))

;;;###autoload
(define-minor-mode dict-line-mode
  "Minor mode to look up words under the cursor asynchronously."
  :lighter " "
  :group 'dict-line
  (if dict-line-mode
      (progn
        ;; Start the idle timer for asynchronous word lookup
        (run-with-idle-timer dict-line-idle-time t #'dict-line--get-dict-async)
        ;; Add hook to delete posframe after each command
        (add-hook 'post-command-hook #'dict-line--posframe-delete))
    ;; Cancel all timers for dict-line--get-dict-async
    (cancel-function-timers #'dict-line--get-dict-async)
    ;; Remove the hook for deleting posframe
    (remove-hook 'post-command-hook #'dict-line--posframe-delete))
  )

(provide 'dict-line)
