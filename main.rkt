#lang racket

(require "scanner.rkt"
         "parser.rkt")

; Function to read input file and convert each line into a list of strings, maintaining whitespace and newline characters.
(define (read-input-file filename)
  (with-handlers ([exn:fail:filesystem? (lambda (exn) (displayln "Error: Input file not found.") 'error)])
    (with-input-from-file filename
      (lambda ()
        (let loop ((lines '()))
          (let ((line (read-line)))
            (if (eof-object? line)
                (reverse lines)
                (loop (cons line lines)))))))))

; Main entry point for the program, responsible for initiating the scanning and parsing workflow.
(define (main)
(displayln "Please enter the name of the input file: ")
  (define input-file (read-line)) ; Prompt user for the input file name.
  (define input-lines (read-input-file input-file)) ; Read the input file into a list of lines.
  (if (eq? input-lines 'error)
      (displayln "Terminating program due to input error.") ; Terminate if the file does not exist.
      (process-lines input-lines))) ; Proceed if file reading is successful.

; Function to pass each line to the scanner and keep track of errors.
(define (process-lines lines)
  (define (loop remaining-lines error?)
    (if (or (null? remaining-lines) error?)
        (if error?
            (displayln "Error encountered during scanning, terminating process.")
            (displayln "All lines processed successfully."))
        (let ((scan-result (create-tokens (first remaining-lines)))) ; Pass to scanner.rkt.
          (if (eq? scan-result 'error)
              (loop '() #t) ; Set error flag and stop processing.
              (loop (rest remaining-lines) #f)))))
  (loop lines #f))

; Only run main if this file is the main module.
(when (equal? (variable-reference->module-declaration-inspector (#%variable-reference))
              (quote main.rkt))
  (main))