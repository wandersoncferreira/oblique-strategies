;;; oblique-strategies --- Oblique Strategies

;; Copyright (C)

;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Homepage: https://github.com/wandersoncferreira/oblique-strategies
;; Package: oblique-strategies
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Most (or all) initial prompts were taken from https://kevinlawler.com/prompts

;;; Code:

(require 'json)

(defvar oblique-strategies-json-data-file (expand-file-name "sources/data.json" (file-name-directory load-file-name))
  "Filepath with good oblique strategies.")

(defvar oblique-strategies-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'oblique-strategies)
    (define-key map "p" 'oblique-strategies)
    (define-key map "q" 'quit-window)
    map))

(defvar oblique-strategies-buffer-name "*Oblique Strategy*"
  "Name of the buffer.")

(defun oblique-strategies-read-data ()
  "Read strategies from JSON data file."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file oblique-strategies-json-data-file)))
    json))

(defun oblique-strategies-random-elt (list)
  "Get random element from LIST."
  (nth (random (length list)) list))

(defun oblique-strategies-create-buffer (prompt desc)
  "Create the oblique buffer with PROMPT message and DESC message."
  (pop-to-buffer oblique-strategies-buffer-name)
  (let ((inhibit-read-only t)
        (dashes-fn (lambda () (dotimes (_ (* (length prompt) 1.3)) (insert "-")))))
    (setq-local fill-column 60)
    (erase-buffer)
    (funcall dashes-fn)
    (insert "\n")
    (insert (propertize prompt 'font-lock-face '(:weight bold :height 1.4)))
    (insert "\n")
    (funcall dashes-fn)
    (insert "\n")
    (insert "\n")
    (insert desc)
    (fill-paragraph)
    (special-mode)
    (use-local-map oblique-strategies-map)))

(defun oblique-strategies ()
  "Got stuck? We might help!"
  (interactive)
  (let* ((data (oblique-strategies-read-data))
         (entry (oblique-strategies-random-elt data))
         (prompt (gethash "prompt" entry))
         (desc (oblique-strategies-random-elt (gethash "descriptions" entry))))
    (oblique-strategies-create-buffer prompt desc)))

(provide 'oblique-strategies)
;;; oblique-strategies.el ends here
