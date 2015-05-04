;;; ta.el --- A utility to deal with homophonic characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar-local ta-overlay nil) ;(make-variable-buffer-local 'ta-overlay)
(defvar-local ta-current-position nil)
(defvar-local ta-current-homophony-list nil)

(defvar ta-flattened-homophony-list nil)
(defvar ta--timer-object nil)

;; ======================================================
;; Settings
;; ======================================================

(setq ta-homophony-list
      '(("他" "她" "它" "牠" "祂")
        ("你" "妳" "您")
        ("的" "得")
        )
      )

(defvar ta-max-search-range 300
  "Max search range for possible homophony.")


(defvar ta-delay 3
  "The number of seconds to wait before checking, after a \"delayed\" command."
  )

(defcustom flyspell-default-delayed-commands
  '(self-insert-command
    delete-backward-char
    backward-or-forward-delete-char
    delete-char
    scrollbar-vertical-drag
    backward-delete-char-untabify)
  "The standard list of delayed commands for Flyspell.
See `flyspell-delayed-commands'."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

;; ======================================================
;; Homophony list function
;; ======================================================

(defun ta-reload-homophony-list ()
  "Update `ta-flattened-homophony-list',
which is a flatten list, like '(20182 22905 ...)"
  (interactive)
  (setq ta-flattened-homophony-list (mapcar (lambda (c) (string-to-char c))
                                            (apply #'append ta-homophony-list))))

(ta-reload-homophony-list)

(defun ta-get-homophony-list (char-str)
  "Get the homophony list of CHAR-STR"
  (car (member* char-str
                ta-homophony-list
                :test (lambda (char list) (member char list)))))

;; ======================================================
;; Face
;; ======================================================

(defface ta-candidates
  '((((class color) (background light))
     (:foreground "#ff8700"))
    (((class color) (background dark))
     (:foreground "#ffa722")))
  "Face for all candidates"
  :group 'ta-faces)

(defface ta-current-candidate
  '((((class color) (background light))
     (:foreground "#ff66aa"))
    (((class color) (background dark))
     (:foreground "#ff66aa")))
  "Face for current candidate"
  :group 'ta-faces)

;; ======================================================
;; Minor-mode
;; ======================================================

(defun ta-post-command-hook ()
  (if (null ta-delay)
      (ta-find-previous-candidate)
    (when (null ta--timer-object)
      (setq ta--timer-object
            (run-with-idle-timer ta-delay nil
                                 (lambda ()
                                   (ta-show)
                                   (setq ta--timer-object nil)))))))

(defun ta-pre-command-hook ()
  (ta-remove))


(define-minor-mode ta-mode
  "Deal with homophonic characters"
  :lighter " ta"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") 'ta-previous-homophony)
            (define-key map (kbd "<down>") 'ta-next-homophony)
            (define-key map (kbd "<left>") 'ta-previous-candidate)
            (define-key map (kbd "<right>") 'ta-next-candidate)
            map)
  :global nil
  (if ta-mode
      (progn
        (add-hook 'pre-command-hook 'ta-pre-command-hook nil t)
        (add-hook 'post-command-hook 'ta-post-command-hook nil t))
    (progn
      (remove-hook 'pre-command-hook 'ta-pre-command-hook t)
      (remove-hook 'post-command-hook 'ta-post-command-hook t))))

(defvar ta-choose-homophony-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'ta-previous-homophony)
    (define-key map (kbd "<down>") 'ta-next-homophony)
    map))

;; ======================================================
;; Overlays
;; ======================================================

;;(remove-overlays beg end 'flyspell-overlay t)
;;(overlay-put overlay 'flyspell-overlay t)
;;(overlays-at pos)

(defun ta-make-overlay (position face)
  "Allocate an overlay to highlight a possible candidate character."
  (let ((ol (make-overlay position (+ 1 position) nil t nil)))
    (overlay-put ol 'face face)
    (overlay-put ol 'ta-overlay t)
    ))

(defun ta-delete-region-overlay (begin end)
  (remove-overlays begin end 'ta-overlay t))

(defun ta-delete-all-overlays ()
  (ta-delete-region-overlay (point-min) (point-max)))

(defun ta-overlay-p (obj)
  "Return true if o is an overlay used by flyspell."
  (and (overlayp obj) (overlay-get obj 'ta-overlay)))

;; ======================================================
;; Main
;; ======================================================

(defun ta-replace-char (position character)
  "Replace the char in position, then add face."
  (delete-region position (+ position 1))
  (save-excursion
    (goto-char position)
    (insert character))
  (ta-make-overlay position 'ta-candidates)
  )

(defun ta-auto-update-candidate ()
  "Update `ta-current-position' and `ta-current-homophony-list' within
 `ta-max-search-range' steps.
Used in idle timer."
  (interactive)
  (save-excursion
    (do ((i ta-max-search-range (1- i)))
        (
         ;;End Test
         (or (= i 0)
             (= (point) (point-min))
             (memq (char-after (point)) ta-flattened-homophony-list))
         ;; Final Result (Run after end test passed)
         (progn
           (ta-delete-all-overlays)
           (when (memq (char-after (point)) ta-flattened-homophony-list)
             (ta-make-overlay (point) 'ta-candidates)
             (setq ta-current-position (point)
                   ta-current-homophony-list (ta-get-homophony-list
                                              (char-to-string
                                               (char-after ta-current-position))))
             (point)))
         )
      ;; Main
      (left-char))))


(defun ta-find-previous-candidate (&optional reverse)
  "Update `ta-current-position' and `ta-current-homophony-list',
 without any range limit. When REVERSE is non-nil,
find nextcandidate. Should be called interactively, not by idle timer."
  (interactive)
  (save-excursion
    (do ((i 0 (1+ i)))
        (
         ;;End Test
         (or (and reverse (= (point) (point-max)))
             (= (point) (point-min))
             (memq (char-after (point)) ta-flattened-homophony-list))
         ;; Final Result (Run after end test passed)
         (if (memq (char-after (point)) ta-flattened-homophony-list)
             (progn
               (ta-delete-all-overlays)
               (ta-make-overlay (point) 'ta-candidates)
               (setq ta-current-position (point)
                     ta-current-homophony-list (ta-get-homophony-list
                                                (char-to-string
                                                 (char-after ta-current-position))))
               (point))
           (message (if reverse "The last candidate" "The first candidate")))
         )
      ;; Main
      (if reverse (right-char) (left-char)))))

(defun ta--get-next-elem (elem list)
  (let ((l (member elem list)))
    (if (null (cdr l))
        (car list)
      (cadr l))))

(defun ta-next-homophony (&optional reverse)
  (interactive)
  (ta-find-previous-candidate)
  (let ((current-character (char-to-string (char-after ta-current-position))))
    (ta-replace-char
     ta-current-position
     (ta--get-next-elem current-character
                        (if reverse
                            (reverse ta-current-homophony-list)
                          ta-current-homophony-list
                          )))))

(defun ta-previous-homophony ()
  (interactive)
  (ta-next-homophony 'reverse))

(defun ta-left-candidate ()
  (interactive)
  (save-excursion
    ;; [FIXME] Remove idle timer first, if it exists.
    (when ta-current-position
      (goto-char (- ta-current-position 1)))
    (ta-find-previous-candidate)

    ))

(ta-delete-all-overlays)
(defun ta-right-candidate ()
  (interactive)
  (save-excursion
    ;; [FIXME] Remove idle timer first, if it exists.
    (when ta-current-position
      (goto-char (+ ta-current-position 1)))
    (ta-find-previous-candidate 'reverse)
    ))





(provide 'ta)
;;; ta.el ends here
